;;; projection-multi-poetry-poe.el --- Projection integration for `compile-multi' and the Poetry project type with the poe command backend. -*- lexical-binding: t; -*-

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
;; sources the list of available targets from a poetry projects poe the poet
;; config. This assumes poe is invoked within the poetry venv.

;;; Code:

(require 'cl-lib)
(require 'projection-core)
(require 'projection-utils)
(require 'projection-multi)
(require 'projection-types)

(defgroup projection-multi-poetry-poe nil
  "Helpers for `compile-multi' and Poetry projects using poe."
  :group 'projection-multi)

(defcustom projection-multi-poetry-poe-cache-targets 'auto
  "When true cache the Poetry poe targets of each project."
  :type '(choice
          (const :tag "Cache targets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache targets"))
  :group 'projection-multi-poetry-poe)

(defcustom projection-multi-poetry-poe-project-file '("pyproject.toml")
  "Collection of possible basenames for project files to query targets from."
  :type '(list string)
  :group 'projection-multi-poetry-poe)

(defcustom projection-multi-poetry-poe-command-prefix "poetry run poe"
  "Command used prior to invoking poe."
  :type 'string
  :group 'projection-multi-poetry-poe)

(defun projection-multi-poetry-poe--targets-from-file (project-file)
  "Read poetry-poe targets based on PROJECT-FILE respecting project cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-poetry-poe-targets
   (cond
    ((eq projection-multi-poetry-poe-cache-targets 'auto)
     (projection--cache-modtime-predicate project-file))
    (t projection-multi-poetry-poe-cache-targets))
   #'projection-multi-poetry-poe--targets-from-file2))

(projection--declare-cache-var
  'projection-multi-poetry-poe-targets
  :title "Multi Poetry poe targets"
  :category "Poetry"
  :description "Poetry poe targets"
  :hide t)

(defun projection-multi-poetry-poe--targets-from-file2 ()
  "Read poetry-poe targets."
  (projection--log :debug "Resolving available Poetry Poe targets")

  (let ((targets))
    (projection--with-shell-command-buffer
        projection-multi-poetry-poe-command-prefix
      (save-match-data
        (when (search-forward-regexp (rx bol "CONFIGURED TASKS" eol) nil 'no-error)
          (while (and
                  (not (eobp))
                  (progn (forward-line 1) t)
                  (search-forward-regexp
                   (rx bol (+ space) (group alnum (+ (not space))))
                   (line-end-position)
                   'no-error))
            (push (match-string 1) targets)))))
    (nreverse targets)))

;;;###autoload
(defun projection-multi-poetry-poe-targets (&optional project-type)
  "`compile-multi' target generator function for poetry-poe projects.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "poetry:poe"))

  (when-let* ((file-name (cl-find-if #'file-exists-p projection-multi-poetry-poe-project-file)))
    (cl-loop
     for target in (projection-multi-poetry-poe--targets-from-file file-name)
     collect (cons (concat project-type ":" target)
                   (concat projection-multi-poetry-poe-command-prefix
                           " " (shell-quote-argument target))))))

;;;###autoload
(defun projection-multi-compile-poetry-poe ()
  "`compile-multi' wrapper for only poetry-poe targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-poetry-poe-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (projection-type-append-compile-multi-targets projection-project-type-python-poetry
    #'projection-multi-poetry-poe-targets))

(provide 'projection-multi-poetry-poe)
;;; projection-multi-poetry-poe.el ends here
