;;; projection-multi-make.el --- Projection integration for `compile-multi' and the Make project type. -*- lexical-binding: t; -*-

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
;; sources the list of available targets from a projects Makefile. This works
;; by parsing the set of targets directly from the Makefile using the same
;; algorithm as `helm-make'.

;;; Code:

(require 'cl-lib)
(require 'projection-core)
(require 'projection-multi)
(require 'projection-types)

(defgroup projection-multi-make nil
  "Helpers for `compile-multi' and Makefile projects."
  :group 'projection-multi)

(defcustom projection-multi-make-cache-targets 'auto
  "When true cache the Make targets of each project."
  :type '(choice
          (const :tag "Cache targets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache targets"))
  :group 'projection-multi-make)

(defcustom projection-multi-make-makefiles '("Makefile" "GNUMakefile")
  "Collection of possible basenames for Makefiles to query targets from."
  :type '(list string)
  :group 'projection-multi-make)

(defconst projection-multi-make--help-regex
  "^\\([^: \n]+\\) *:\\(?: \\|$\\)"
  "Regexp to match targets from a Makefile.")

(defun projection-multi-make--targets-from-file (makefile)
  "Read makefile targets from MAKEFILE respecting project cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-make-targets
   (cond
    ((eq projection-multi-make-cache-targets 'auto)
     (projection--cache-modtime-predicate makefile))
    (t projection-multi-make-cache-targets))
   (apply-partially #'projection-multi-make--targets-from-file2 makefile)))

(projection--declare-cache-var
  'projection-multi-make-targets
  :title "Multi make targets"
  :category "Make"
  :description "Make targets associated with this project"
  :hide t)

(defun projection-multi-make--targets-from-file2 (makefile)
  "Read makefile targets from MAKEFILE."
  ;; Taken from [[https://github.com/abo-abo/helm-make/blob/ebd71e85046d59b37f6a96535e01993b6962c559/helm-make.el#L284][helm-make/helm--make-target-list-default]].
  (projection--log
   :debug "Resolving available Makefile targets from file=%s" makefile)

  (let (make-targets)
    (with-temp-buffer
      (insert-file-contents makefile)
      (goto-char (point-min))
      (while (re-search-forward projection-multi-make--help-regex nil t)
        (let ((str (match-string 1)))
          (unless (string-match "^\\." str)
            (push str make-targets)))))
    (setq make-targets (nreverse make-targets))))

;;;###autoload
(defun projection-multi-make-targets (&optional project-type file-name)
  "`compile-multi' target generator function for Makefile projects.
When set the generated targets will be prefixed with PROJECT-TYPE.
When set this function will read targets from FILE-NAME instead of
the first Makefile it finds in the current directory."
  (setq project-type (or project-type "make"))
  (setq file-name
        (or file-name
            (cl-find-if #'file-exists-p projection-multi-make-makefiles)))

  (when file-name
    (cl-loop
     for target in (projection-multi-make--targets-from-file file-name)
     collect (cons (concat project-type ":" target)
                   (concat "make " (shell-quote-argument target))))))

;;;###autoload
(defun projection-multi-compile-make ()
  "`compile-multi' wrapper for only Make targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-make-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (oset projection-project-type-make compile-multi-targets
        (seq-uniq
         (append
          (oref projection-project-type-make compile-multi-targets)
          (list #'projection-multi-make-targets)))))

(provide 'projection-multi-make)
;;; projection-multi-make.el ends here
