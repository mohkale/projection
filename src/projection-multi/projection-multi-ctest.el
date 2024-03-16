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
(require 'projection-core)
(require 'projection-type-cmake)
(require 'projection-multi)
(require 'projection-types)

(define-obsolete-variable-alias 'projection-multi-ctest-cache-targets 'projection-cmake-ctest-cache-targets "0.1")

(defcustom projection-multi-ctest-add-exclude-label-targets t
  "When true add targets to run all tests except a given label."
  :type 'boolean
  :group 'projection-type-cmake
  :group 'projection-multi)

(defun projection-multi-ctest--resolve-targets ()
  "Resolve available ctest targets for a project respecting the project cache."
  (cl-loop for test in (alist-get 'tests (projection-cmake-ctest--targets))
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
           append (mapcar (apply-partially #'cons :label) test-labels)))

;;;###autoload
(defun projection-multi-ctest-targets (&optional project-type)
  "`compile-multi' target generator function for CMake ctest targets.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "ctest"))

  (when-let* ((projection-cmake-preset 'silent)
              (ctest-targets (projection-multi-ctest--resolve-targets)))
    (append
     (cl-loop
      for (type . target) in ctest-targets
      with target-regex = nil
      do (setq target-regex (concat "^" target "$"))

      if (eq type :test)
        collect `(,(concat project-type ":" target)
                  :command ,(projection-cmake--ctest-command "-R" target-regex)
                  :annotation ,(projection-cmake--ctest-annotation target))
      else if (eq type :label)
        collect `(,(concat project-type ":label:" target)
                  :command ,(projection-cmake--ctest-command "-L" target-regex)
                  :annotation ,(projection-cmake--ctest-annotation
                                (concat "label:" target)))
        and if projection-multi-ctest-add-exclude-label-targets
          collect `(,(concat project-type ":label:not:" target)
                    :command ,(projection-cmake--ctest-command "-LE" target-regex)
                    :annotation ,(projection-cmake--ctest-annotation
                                  (concat "except-label:" target)))
        end
      else
        do (error "Unexpected ctest target type=%s" type))
     `((,(concat project-type ":rerun:failed")
        :command ,(projection-cmake--ctest-command "--rerun-failed")
        :annotation ,(projection-cmake--ctest-annotation "rerun-failed"))))))

;;;###autoload
(defun projection-multi-compile-ctest ()
  "`compile-multi' wrapper for only CMake ctest targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-ctest-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (projection-type-append-compile-multi-targets projection-project-type-cmake
    #'projection-multi-ctest-targets))

(provide 'projection-multi-ctest)
;;; projection-multi-ctest.el ends here
