;;; projection-multi-vscode-tasks.el --- Projection integration for `compile-multi' and the VScode tasks.json. -*- lexical-binding: t; -*-

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
;; sources the list of available targets from a VSCode task.json file. See
;; https://code.visualstudio.com/docs/editor/tasks#_custom-tasks

;;; Code:

(require 'json)
(require 'projection-multi)
(require 'projection-types)

(defgroup projection-multi-vscode-tasks nil
  "Helpers for `compile-multi' and VSCode task projects."
  :group 'projection-multi)

(defcustom projection-multi-vscode-tasks-cache-tasks 'auto
  "When true cache the VSCode task targets of each project."
  :type '(choice
          (const :tag "Cache targets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache targets")))

(defun projection-multi-vscode-tasks--contents ()
  "Read VSCode tasks file respecting project-cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-vscode-tasks
   (cond
    ((eq projection-multi-vscode-tasks-cache-tasks 'auto)
     (projection--cache-modtime-predicate ".vscode/tasks.json"))
    (t projection-multi-vscode-tasks-cache-tasks))
   #'projection-multi-vscode-tasks--contents2))

(projection--declare-cache-var
  'projection-multi-vscode-tasks
  :title "Multi VSCode tasks"
  :category "VSCode"
  :description "VSCode tasks associated with this project"
  :hide t)

(defun projection-multi-vscode-tasks--contents2 ()
  "Read VSCode tasks file."
  (projection--log :debug "Reading VSCode tasks.json")
  (condition-case err
      (let ((json-array-type 'list))
        (json-read-file ".vscode/tasks.json"))
    ((file-missing json-readtable-error)
     (projection--log :error "Failed to read VSCode tasks.json: %S." (cdr err)))))



;;;###autoload
(defun projection-multi-compile-vscode-targets (&optional project-type)
  "`compile-multi' target generator function for VSCode task projects.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "vscode"))

  (let ((result))
    (dolist (task (alist-get 'tasks (projection-multi-vscode-tasks--contents)))
      (let-alist task
        (setq .type (intern (or .type "shell")))
        (when (consp .group)
          (setq .group (alist-get 'kind .group)))

        (when (and (cl-member .type '(process shell))
                   .command)
          (push (cons (concat project-type ":"
                              (when .group
                                (concat .group ":"))
                              (or .label .command))
                      (projection--join-shell-command
                       `(,@(projection--env-shell-command-prefix
                            (cl-loop for (key . value) in (alist-get 'env .options)
                                     collect (cons (symbol-name key) value))
                            (alist-get 'cwd .options))
                         ,.command
                         ,@(cl-loop for arg in .args
                                    when (consp arg)
                                      collect (alist-get 'value arg)
                                    else
                                      collect arg))))
                result))))
    (nreverse result)))

;;;###autoload
(defun projection-multi-compile-vscode-tasks ()
  "`compile-multi' wrapper for only VSCode task targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-compile-vscode-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (projection-type-append-compile-multi-targets projection-project-type-vscode-tasks
    #'projection-multi-compile-vscode-targets))

(provide 'projection-multi-vscode-tasks)
;;; projection-multi-vscode-tasks.el ends here
