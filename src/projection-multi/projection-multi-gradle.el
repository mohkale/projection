;;; projection-multi-gradle.el --- Projection integration for `compile-multi' and the Gradle project type. -*- lexical-binding: t; -*-

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
;; sources the list of available targets from a Gradle projects tasks.

;;; Code:

(require 'projection-types)
(require 'projection-type-gradle)
(require 'projection-multi)

(defgroup projection-multi-gradle nil
  "Helpers for `compile-multi' and gradle projects."
  :group 'projection-multi)



(defcustom projection-multi-gradle-cache-targets t
  "When true cache the Gradle targets of each project."
  :type '(boolean :tag "Always/Never cache targets")
  :group 'projection-multi-gradle)

(defun projection-multi-gradle--tasks ()
  "Read gradle targets respecting project cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-gradle-tasks
   projection-multi-gradle-cache-targets
   #'projection-multi-gradle--tasks2))

(projection--declare-cache-var
  'projection-multi-gradle-tasks
  :title "Multi Gradle tasks"
  :category "Gradle"
  :description "Gradle tasks associated with this project"
  :hide t)

(defun projection-multi-gradle--tasks2 ()
  "Read gradle tasks."
  (let ((result))
    (projection--log :debug "Resolving available gradle targets")
    (save-match-data
      (projection--with-shell-command-buffer
        (let ((projection-gradle-use-daemon nil))
          (projection-gradle--command "tasks" "--all"))
        (while (re-search-forward (rx space "tasks" ?\n (+ "-") eol) nil 'noerror)
          (while (and
                  (forward-line)
                  (not (eobp))
                  (re-search-forward
                   (rx bol
                       (group (+ (or alnum ":")))
                       (? space "-" space (group (+ any)))
                       eol)
                   (line-end-position)
                   'noerror))
            (push (cons (match-string 1)
                        (unless (string-empty-p (match-string 2))
                          (match-string 2)))
                  result))))
      (nreverse result))))



;;;###autoload
(defun projection-multi-gradle-targets (&optional project-type)
  "`compile-multi' target generator function for Gradle projects.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "gradle"))

  (cl-loop for (task . documentation) in
           (projection-multi-gradle--tasks)
           collect `(,(concat project-type ":" task)
                     :command
                     ,(projection-gradle--command task)
                     ,@(when documentation
                         (list :annotation documentation)))))

;;;###autoload
(defun projection-multi-compile-gradle ()
  "`compile-multi' wrapper for only Gradle targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-gradle-targets))))

;;;###autoload
(defvar projection-project-type-gradle)
;;;###autoload
(with-eval-after-load 'projection-types
  (projection-type-append-compile-multi-targets projection-project-type-gradle
    #'projection-multi-gradle-targets))

(provide 'projection-multi-gradle)
;;; projection-multi-gradle.el ends here
