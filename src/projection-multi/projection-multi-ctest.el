;;; projection-multi-ctest.el --- Projection integration for `compile-multi' and the ctest with the CMake project type. -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mohsin Kaleem

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library exposes a target generation function for `compile-multi' which
;; sources the list of available targets from a CMake projects ctest build
;; config.

;;; Code:

(require 'projection-utils)
(require 'projection-core-log)
(require 'projection-utils-cmake)
(require 'projection-multi)
(require 'projection-types)

(defcustom projection-multi-ctest-cache-targets nil
  "When true cache the CMake ctest targets of each project permanently."
  :type '(boolean :tag "Always/Never cache targets")
  :group 'projection-multi-cmake)

(defun projection-multi-ctest--command (&rest argv)
  "Helper function to  generate a ctest command.
ARGV if provided will be appended to the command."
  (projection--join-shell-command
   `("ctest" "--test-dir" ,projection-cmake-build-directory ,@argv)))

(defcustom projection-multi-ctest-add-exclude-label-targets t
  "When true add targets to run all tests except a given label."
  :type 'boolean
  :group 'projection-types
  :group 'projection-multi)

(defun projection-multi-ctest--resolve-targets ()
  "Resolve available ctest targets for a project respecting the project cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-ctest-targets
   projection-multi-ctest-cache-targets
   #'projection-multi-ctest--resolve-targets2))

(defun projection-multi-ctest--resolve-targets2 ()
  "Resolve available ctest targets for a project.
Returns a list of cons cells containing the kind of target and the target
value. Supported target types include test for tests and label for labels."
  (projection--log :debug "Resolving available CMake ctest targets")

  (projection--with-shell-command-buffer
    (projection-multi-ctest--command "--show-only=json-v1")
    (let ((json-array-type 'list))
      (cl-loop for test in (alist-get 'tests (json-read))
               with test-name = nil
               do (setq test-name (alist-get 'name test))
               when test-name
               collect (cons :test test-name)

               with label-set = (make-hash-table :test #'equal)
               with test-labels = nil
               do (setq test-labels
                        (seq-filter
                         (lambda (label)
                           (when-let ((doesnt-exist (not (gethash label label-set))))
                             (puthash label t label-set)
                             t))
                         (cl-dolist (prop (alist-get 'properties test))
                           (when (string-equal "LABELS" (alist-get 'name prop))
                             (cl-return (alist-get 'value prop))))))
               append (mapcar (apply-partially #'cons :label) test-labels)))))

;;;###autoload
(defun projection-multi-ctest-targets (&optional project-type)
  "`compile-multi' target generator function for CMake ctest targets.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "ctest"))

  (let ((projection-cmake-preset 'silent))
    (cl-loop
     for (type . target) in (projection-multi-ctest--resolve-targets)
     with target-regex = nil
     do (setq target-regex (concat "^" target "$"))

     if (eq type :test)
       collect `(,(concat project-type ":" target)
                 :command ,(projection-multi-ctest--command "-R" target-regex)
                 :annotation ,(concat "ctest " target))
     else if (eq type :label)
       collect `((concat project-type ":label:" target)
                 :command (projection-multi-ctest--command "-L" target-regex)
                 :annotation ,(concat "ctest label:" target))
       and if projection-multi-ctest-add-exclude-label-targets
         collect `((concat project-type ":label:not:" target)
                   :command (projection-multi-ctest--command "-LE" target-regex)
                   :annotation ,(concat "ctest except-label:" target))
       end
     else
       do (error "Unexpected ctest target type=%s" type))))

;;;###autoload
(defun projection-multi-compile-ctest ()
  "`compile-multi' wrapper for only CMake ctest targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-ctest-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (oset projection-project-type-cmake compile-multi-targets
        (seq-uniq
         (append
          (oref projection-project-type-cmake compile-multi-targets)
          (list #'projection-multi-ctest-targets)))))

(provide 'projection-multi-ctest)
;;; projection-multi-ctest.el ends here
