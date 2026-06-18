;;; projection-multi-just.el --- Projection integration for `compile-multi' and justfiles. -*- lexical-binding: t; -*-

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
;; sources the list of available targets from a projects justfile. This works
;; by parsing the set of targets directly from the justfile using the same
;; algorithm as `projection-multi-make'.

;;; Code:

(require 'cl-lib)
(require 'projection-core)
(require 'projection-multi)
(require 'projection-types)

(defgroup projection-multi-just nil
  "Helpers for `compile-multi' and Justfile projects."
  :group 'projection-multi)

(defcustom projection-multi-just-cache-targets 'auto
  "When true cache the Just targets of each project."
  :type '(choice
          (const :tag "Cache targets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache targets"))
  :group 'projection-multi-just)

(defcustom projection-multi-just-justfiles '("justfile")
  "Collection of possible basenames for Justfiles to query targets from."
  :type '(list string)
  :group 'projection-multi-just)

(defconst projection-multi-just--help-regex
  "^\\([^: \n]+\\) *:\\(?: \\|$\\)"
  "Regexp to match targets from a Justfile.")

(defun projection-multi-just--targets-from-file (justfile)
  "Read justfile targets from JUSTFILE respecting project cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-just-targets
   (cond
    ((eq projection-multi-just-cache-targets 'auto)
     (projection--cache-modtime-predicate justfile))
    (t projection-multi-just-cache-targets))
   (apply-partially #'projection-multi-just--targets-from-file2 justfile)))

(projection--declare-cache-var
  'projection-multi-just-targets
  :title "Multi just targets"
  :category "Just"
  :description "Just targets associated with this project"
  :hide t)

(defun projection-multi-just--targets-from-file2 (justfile)
  "Read justfile targets from JUSTFILE."
  ;; Taken from [[https://github.com/abo-abo/helm-just/blob/ebd71e85046d59b37f6a96535e01993b6962c559/helm-just.el#L284][helm-just/helm--just-target-list-default]].
  (projection--log
   :debug "Resolving available Justfile targets from file=%s" justfile)

  (let (just-targets)
    (with-temp-buffer
      (insert-file-contents justfile)
      (goto-char (point-min))
      (while (re-search-forward projection-multi-just--help-regex nil t)
        (let ((str (match-string 1)))
          (unless (string-match "^\\." str)
            (push str just-targets)))))
    (setq just-targets (nreverse just-targets))))

;;;###autoload
(defun projection-multi-just-targets (&optional project-type file-name)
  "`compile-multi' target generator function for Justfile projects.
When set the generated targets will be prefixed with PROJECT-TYPE.
When set this function will read targets from FILE-NAME instead of
the first Justfile it finds in the current directory."
  (setq project-type (or project-type "just"))
  (setq file-name
        (or file-name
            (cl-find-if #'file-exists-p projection-multi-just-justfiles)))

  (when file-name
    (cl-loop
     for target in (projection-multi-just--targets-from-file file-name)
     collect (cons (concat project-type ":" target)
                   (projection-just-run-build target)))))

;;;###autoload
(defun projection-multi-compile-just ()
  "`compile-multi' wrapper for only Just targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-just-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (projection-type-append-compile-multi-targets projection-project-type-just
    #'projection-multi-just-targets))

(provide 'projection-multi-just)
;;; projection-multi-just.el ends here
