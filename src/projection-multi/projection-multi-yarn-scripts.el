;;; projection-multi-yarn-scripts.el --- Projection integration for `compile-multi' and the Yarn package.json scripts. -*- lexical-binding: t; -*-

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
;; sources the list of available targets from a yarn projects scripts config
;; in package.json.

;;; Code:

(require 'cl-lib)

(require 'projection-core)
(require 'projection-core-log)
(require 'projection-utils)
(require 'projection-multi)
(require 'projection-types)

(defgroup projection-multi-yarn-scripts nil
  "Helpers for `compile-multi' and yarn projects."
  :group 'projection-multi)

(defcustom projection-multi-yarn-scripts-cache-targets 'auto
  "When true cache the Yarn script targets of each project."
  :type '(choice
          (const :tag "Cache targets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache targets"))
  :group 'projection-multi-yarn-scripts)

(defun projection-multi-yarn-scripts--targets-from-file (package-json)
  "Read yarn script targets based on PROJECT-FILE respecting project cache.
PACKAGE-JSON is the file that will be used to invalidate the cache of targets."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-yarn-script-targets
   (cond
    ((eq projection-multi-yarn-scripts-cache-targets 'auto)
     (projection--cache-modtime-predicate package-json))
    (t projection-multi-yarn-scripts-cache-targets))
   #'projection-multi-yarn-scripts--targets-from-file2))

(projection--declare-cache-var
  'projection-multi-yarn-script-targets
  :title "Multi YARN script targets"
  :category "Yarn"
  :description "Yarn script targets associated with this project"
  :hide t)

(defun projection-multi-yarn-scripts--targets-from-file2 ()
  "Read Yarn script targets."
  (projection--log :debug "Resolving available yarn script targets")
  (projection--with-shell-command-buffer "yarn run"
    (let (result)
      (save-match-data
        (while (search-forward-regexp
                (rx
                 ;; bol "  " (group (+ (not " "))) eol
                 (and
                  bol
                  (or
                   (and
                    "info Commands available from binary scripts: "
                    (group-n 1 (+ any)))
                   (and (repeat 3 space) "-" space (group-n 2 (+ any))))
                  eol))
                nil 'noerror)
          (cond
           ((match-string 1)
            (setq result (append result
                                 (split-string (match-string 1) ", " 'omit-nulls))))
           ((match-string 2)
            (push (match-string 2) result))))
        (nreverse result)))))

;;;###autoload
(defun projection-multi-yarn-script-targets (&optional project-type)
  "`compile-multi' target generator function for Yarn projects with scripts.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (or project-type (setq project-type "yarn"))

  (when (and (file-exists-p "package.json")
             (executable-find "yarn" 'remote))
    (cl-loop
     for target in (projection-multi-yarn-scripts--targets-from-file "package.json")
     collect (cons (concat project-type ":" target)
                   (concat "yarn run " (shell-quote-argument target))))))

;;;###autoload
(defun projection-multi-compile-yarn-scripts ()
  "`compile-multi' wrapper for only yarn script targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-yarn-script-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (oset projection-project-type-yarn compile-multi-targets
        (seq-uniq
         (append
          (oref projection-project-type-yarn compile-multi-targets)
          (list #'projection-multi-yarn-script-targets)))))

(provide 'projection-multi-yarn-scripts)
;;; projection-multi-yarn-scripts.el ends here
