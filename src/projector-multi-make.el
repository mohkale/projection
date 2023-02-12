;;; projector-multi-make.el --- projector integration for `compile-multi' and the Make project type. -*- lexical-binding: t; -*-

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

;; TODO

;;; Code:

(require 'cl-lib)
(require 'projector-core)

(defgroup projector-multi-make nil
  "Helpers for `compile-multi' and Makefile projects."
  :group 'projector-multi)

(defcustom projector-multi-make-cache-targets t
  "When true cache the Make targets of each project."
  :type 'boolean
  :group 'projector-multi-make)

(defconst compile-multi-make--help-regex
  "^\\([^: \n]+\\) *:\\(?: \\|$\\)"
  "Regexp to match targets from a Makefile.")

(defun projector-multi-make--targets-from-file (makefile)
  "Read makefile target from MAKEFILE."
  (let* (make-targets
         (project (projector--current-project 'no-error))
         (modtime (when projector-multi-make-cache-targets
                    (file-attribute-modification-time
                     (file-attributes makefile 'integer))))
         (cached-targets (when (and projector-multi-make-cache-targets
                                    project)
                           (projector--cache-get
                            project 'projector-multi-make-targets))))
    (if (and modtime
             cached-targets
             (time-less-p modtime (car cached-targets)))
        (setq make-targets (cdr cached-targets))
      ;; Read and then maybe cache targets from the Makefile.
      ;; Taken from [[https://github.com/abo-abo/helm-make/blob/ebd71e85046d59b37f6a96535e01993b6962c559/helm-make.el#L284][helm-make/helm--make-target-list-default]].
      (with-temp-buffer
        (insert-file-contents makefile)
        (goto-char (point-min))
        (while (re-search-forward compile-multi-make--help-regex nil t)
          (let ((str (match-string 1)))
            (unless (string-match "^\\." str)
              (push str make-targets)))))
      (setq make-targets (nreverse make-targets))
      (when (and projector-multi-make-cache-targets modtime project)
        (projector--cache-put
         project 'projector-multi-make-targets (cons modtime make-targets))))
    make-targets))

;;;###autoload
(defun projector-multi-make-targets (&optional project-type file-name)
  "`compile-multi' target generator function for Makefile projects.
When set the generated targets will be prefixed with PROJECT-TYPE.
When set this function will read targets from FILE-NAME instead of
the first Makefile it finds in the current directory."
  (setq project-type (or project-type "make"))
  (setq file-name
        (or file-name
            (cl-find-if #'file-exists-p '("Makefile" "GNUMakefile"))))

  (when file-name
    (cl-loop
     for target in (projector-multi-make--targets-from-file file-name)
     collect (cons (concat project-type ":" target)
                   (concat "make " (shell-quote-argument target))))))

(provide 'projector-multi-make)
;;; projector-multi-make.el ends here
