;;; projector-commands.el --- Run project-specific shell commands. -*- lexical-binding: t; -*-

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

;; This module exposes a collection of functions to allow you to run distinct
;; shell commands (or elisp function) for different purposes in a project
;; specific way. This could involve separate commands to configure, build,
;; and test a project.

;;; Code:

(require 'subr-x)
(require 'projector-core)

(defun projector-commands--read-shell-command (project type)
  "Interactively read a shell command for the command TYPE in PROJECT."
  (read-shell-command
   (projector--prompt "%s project: " project
                      (capitalize (symbol-name type)))
   (when-let ((command
               (projector-commands--get-command project type nil 'no-error)))
     (and (stringp command)
          command))
   'compile-history))

(defun projector-commands--get-command
    (project cmd-type &optional prompt no-error)
  "Retrieve a command to do CMD-TYPE in PROJECT.
Returns a cons cell of the form (PROJECT-TYPE . COMMAND-FOR-TYPE) where
PROJECT-TYPE is the key of the current project type in `projector-types'.

When PROMPT is non-nil then interactively prompt the user for a command
instead of picking one automatically. When NO-ERROR don't throw an error
if no command is configured for the current project."
  (or
   (and prompt
        (projector-commands--read-shell-command project cmd-type))
   (projector--cache-get project cmd-type)
   (let* ((project-config
           (projector-project-type (project-root project)))
          (project-type (car project-config))
          (project-config (cdr project-config))
          (type-command (alist-get cmd-type project-config)))
     (unless no-error
       (when (and (eq project-type t)
                  (not project-config))
         (error "No project type matching project %s found" (project-root project)))
       (unless type-command
         (error "Project of type %s does not support the command: %s"
                (if (eq project-type t)
                    "default"
                  (symbol-name project-type))
                cmd-type)))
     ;; When the command is a function, but not a command, the function should
     ;; return a shell command or interactive function to run instead.
     (when (and (functionp type-command)
                (not (commandp type-command)))
       (setq type-command (funcall type-command)))
     type-command)))

(defvar projector-commands--registered-cmd-types nil
  "Cache of values registered by `projector-commands--register'.")

(defmacro projector-commands--register (type)
  "Define an interactive function to run a TYPE command on the current project."
  (setq type (eval type))
  (let ((var-symbol (intern (concat "projector-project-" (symbol-name type) "-cmd")))
        (cmd-symbol (intern (concat "projector-" (symbol-name type) "-project"))))
    `(progn
       (defvar ,var-symbol nil
         ,(format "The command to use with `%s'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el."
                  cmd-symbol))

       ;; Save the just registered command to an alist to later reference.
       (add-to-list 'projector-commands--registered-cmd-types
                    (list ',type ',var-symbol #',cmd-symbol) t)

       (defun ,cmd-symbol (&optional prompt)
         ,(format "Run %s command for current project." (symbol-name type))
         (interactive "P")
         (when-let ((project (projector--current-project)))
           (let* ((default-directory (project-root project))
                  (command            ; (project-type . type-command)
                   (or ,var-symbol
                       (projector-commands--get-command project ',type prompt))))
             (projector--cache-put project ',type command)
             (cond
              ((stringp command)
               (compile command))
              ((commandp command)
               (call-interactively command))
              (t
               (user-error "Don't know how to run %s command %s" ',type command)))))))))

;;;###autoload (autoload 'projector-configure-project "projector-commands")
(projector-commands--register 'configure)
;;;###autoload (autoload 'projector-build-project "projector-commands")
(projector-commands--register 'build)
;;;###autoload (autoload 'projector-test-project "projector-commands")
(projector-commands--register 'test)
;;;###autoload (autoload 'projector-run-project "projector-commands")
(projector-commands--register 'run)
;;;###autoload (autoload 'projector-package-project "projector-commands")
(projector-commands--register 'package)
;;;###autoload (autoload 'projector-install-project "projector-commands")
(projector-commands--register 'install)



(defun projector-project-command--candidates ()
  "Retrieve all shell-commands configured for the current project.
Returns an alist mapping the command type to the shell command string."
  (let ((project (projector--current-project)))
    (cl-loop for (cmd-type override-var _cmd-function) in
             projector-commands--registered-cmd-types
             with value = nil
             do (setq value (or
                             (symbol-value override-var)
                             (ignore-errors
                               (projector-commands--get-command project cmd-type))))
             when (stringp value)
               collect (cons cmd-type value))))

(defun projector-project-command--read-commands ()
  "Read one-or-more shell-commands configured for the current project."
  (if-let ((cands (projector-project-command--candidates)))
      (cl-loop
       for key in
       (thread-first
         ;; Select keys from cands interactively (note they will be casted to strings).
         (completing-read-multiple
          (projector--prompt "Run commands: " (projector--current-project))
          (lambda (str pred action)
            (if (eq action 'metadata)
                `(metadata (affixation-function . ,#'identity))
              (complete-with-action action cands str pred)))
          nil t)
         ;; Ensure their ordered in the same way as the original cands list.
         (cl-sort #'< :key
                  (lambda (it)
                    (cl-position
                     (intern it) projector-commands--registered-cmd-types :key #'car))))
       ;; Collect the mapped command, not the key.
       collect (alist-get (intern key) cands))
    (error "No shell commands configured for the current project type")))

;;;###autoload
(defun projector-project-command (cmds)
  "Interactively select and run one or command-types for the current project.
CMDS should be a list of shell commands that should be run one after the other.
If at least one of the commands fails then all the commands failed.

WARN At the moment this function only prompts and works with shell-commands."
  (interactive
   (list (projector-project-command--read-commands)))
  (compile (string-join cmds " &&\n  ")))

(provide 'projector-commands)
;;; projector-commands.el ends here
