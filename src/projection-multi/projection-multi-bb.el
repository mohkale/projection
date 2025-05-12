;;; projection-multi-bb.el --- Projection integration for `compile-multi' and Babashka tasks. -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Mohsin Kaleem

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
;; sources the list of available tasks from a Babshka project's bb.edn.

;;; Code:

(require 'cl-lib)
(require 'projection-core)
(require 'projection-multi)
(require 'projection-types)

(defgroup projection-multi-bb nil
  "Helpers for `compile-multi' and Babashka projects."
  :group 'projection-multi)

(defcustom projection-multi-bb-executable "bb"
  "Path to a Babashka executable used to query available tasks."
  :type 'string
  :group 'projection-multi-bb)

(defcustom projection-multi-bb-cache-tasks 'auto
  "When true cache the Babashka tasks of each project."
  :type '(choice
          (const :tag "Cache targets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache targets"))
  :group 'projection-multi-bb)

(projection--declare-cache-var
  'projection-multi-bb-tasks
  :title "Multi Babashka tasks"
  :category "Babashka"
  :description "Babashka tasks associated with this project"
  :hide t)

(defvar projection-multi-bb--parser-expression
  "
(for [i *input*
      [task-name {task-doc :doc}] (:tasks i)
      :when (symbol? task-name)]
  (str task-name (char 0) task-doc))
"
  "A Babashka expression that will read bb.edn structures from *input*, and
evaluate to a sequence of strings.  Each string is the task name, a null byte,
then the task description.")

;;;###autoload
(defun projection-multi-bb--tasks2 ()
  "`compile-multi' target generator function for Babashka tasks.  Returns a list
of lists each of form (TASK-NAME TASK-DESCRIPTION)."
  ;; REVIEW: This procedure parses the output of 
  (projection--log :debug "Resolving available Babashka tasks")
  (projection--with-shell-command-buffer
    ;; Important not to quote the redirection!
    (format "%s < bb.edn %s"
            (shell-quote-argument projection-multi-bb-executable)
            (projection--join-shell-command
             `("-I" "-o" "-e" ,projection-multi-bb--parser-expression)))
    (save-match-data
      (mapcar (lambda (line)
                (split-string line "\0"))
              (split-string (buffer-string) "\n" t (rx whitespace))))))

;;;###autoload
(defun projection-multi-bb--tasks ()
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-bb-tasks
   (cond ((eq projection-multi-bb-cache-tasks 'auto)
          (projection--cache-modtime-predicate "bb.edn"))
         (t projection-multi-bb-cache-tasks))
   #'projection-multi-bb--tasks2))

;;;###autoload
(defun projection-multi-bb-targets ()
  "Generator for `compile-multi' targets for Babashka tasks."
  (cl-loop for (task-name doc) in (projection-multi-bb--tasks)
           collect (list (concat "bb:" task-name)
                         :command (projection--join-shell-command
                                   (list projection-multi-bb-executable
                                         "run" task-name))
                         :annotation doc)))

;;;###autoload
(defun projection-multi-bb ()
  "`compile-multi' wrapper for only Babashka tasks."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-bb-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (projection-type-append-compile-multi-targets
    (list projection-project-type-babashka)
    #'projection-multi-bb-targets))

(provide 'projection-multi-bb)
;;; projection-multi-bb.el ends here
