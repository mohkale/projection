;;; projection-commands.el --- Run project-specific shell commands. -*- lexical-binding: t; -*-

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
(require 'projection-core)
(require 'projection-core-log)

(defcustom projection-cache-dynamic-commands nil
  "When true cache the result of dynamic compilation commands.
Some projects require dynamically generating a compilation command based
on the project configuration. When this is disabled the function to generate
that command is always called. When enabled the function is called once and
the result cached such that subsequent invocations do not refresh the command.

You'll likely want to leave this disabled. If you notice some projects are
slow to generate commands (most likely on remote machines) you may want to
enable this. The only impact is occasionally you'll run the cached command
and have to refresh it by calling `projection-reset-project-cache'."
  :type 'boolean
  :group 'projection)

(defun projection-commands--read-shell-command (project type)
  "Interactively read a shell command for the command TYPE in PROJECT."
  (read-shell-command
   (projection--prompt "%s project: " project
                      (capitalize (symbol-name type)))
   (when-let ((command
               (projection-commands--get-command project type nil 'no-error)))
     (and (stringp command)
          command))
   'compile-history))

(defun projection-commands--get-command
    (project cmd-type &optional prompt no-error)
  "Retrieve a command to do CMD-TYPE in PROJECT.
Returns a cons cell of the form (PROJECT-TYPE . COMMAND-FOR-TYPE) where
PROJECT-TYPE is the key of the current project type in `projection-types'.

When PROMPT is non-nil then interactively prompt the user for a command
instead of picking one automatically. When NO-ERROR don't throw an error
if no command is configured for the current project."
  (or
   (and prompt
        (let ((command
               (projection-commands--read-shell-command project cmd-type)))
          (projection--cache-put project cmd-type command)
          command))
   (projection--cache-get project cmd-type)
   (let* ((project-config
           (projection-project-type (project-root project)))
          (project-type (car project-config))
          (project-config (cdr project-config))
          (type-command
           (alist-get (intern (concat ":" (symbol-name cmd-type)))
                      project-config)))
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
     (cond
      ((or (stringp type-command)
           (commandp type-command))
       (projection--cache-put project cmd-type type-command))
      ((functionp type-command)
       ;; When the command is a function, but not a command, the function should
       ;; return a shell command or interactive function to run instead. To let
       ;; the function adapt to external configuration changes it will not be
       ;; cached by default.
       (setq type-command (funcall type-command))
       (when projection-cache-dynamic-commands
         (projection--cache-put project cmd-type type-command))))
     type-command)))

(defvar projection-commands--registered-cmd-types nil
  "Cache of values registered by `projection-commands--register'.")

(defmacro projection-commands--register (type)
  "Define an interactive function to run a TYPE command on the current project."
  (let ((var-symbol (intern (concat "projection-project-" (symbol-name type) "-cmd")))
        (cmd-symbol (intern (concat "projection-" (symbol-name type) "-project"))))
    `(progn
       (projection--log :debug "Defining project command of type=%s" ',type)

       (defvar ,var-symbol nil
         ,(format "The command to use with `%s'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el."
                  cmd-symbol))

       ;; Save the just registered command to an alist to later reference.
       (add-to-list 'projection-commands--registered-cmd-types
                    (list ',type ',var-symbol #',cmd-symbol) t)

       (defun ,cmd-symbol (&optional prompt)
         ,(format "Run %s command for current project." (symbol-name type))
         (interactive "P")
         (when-let ((project (projection--current-project)))
           (let* ((default-directory (project-root project))
                  (command
                   (or ,var-symbol
                       (projection-commands--get-command project ',type prompt))))
             (cond
              ((stringp command)
               (compile command))
              ((commandp command)
               (call-interactively command))
              (t
               (user-error "Do not know how to run %s command %s" ',type command)))))))))

;;;###autoload (autoload 'projection-configure-project "projection-commands" nil t)
(projection-commands--register configure)
;;;###autoload (autoload 'projection-build-project "projection-commands" nil t)
(projection-commands--register build)
;;;###autoload (autoload 'projection-test-project "projection-commands" nil t)
(projection-commands--register test)
;;;###autoload (autoload 'projection-run-project "projection-commands" nil t)
(projection-commands--register run)
;;;###autoload (autoload 'projection-package-project "projection-commands" nil t)
(projection-commands--register package)
;;;###autoload (autoload 'projection-install-project "projection-commands" nil t)
(projection-commands--register install)



(defun projection-project-command--candidates ()
  "Retrieve all shell-commands configured for the current project.
Returns an alist mapping the command type to the shell command string."
  (let ((project (projection--current-project)))
    (cl-loop for (cmd-type override-var _cmd-function) in
             projection-commands--registered-cmd-types
             with value = nil
             do (setq value (or
                             (symbol-value override-var)
                             (ignore-errors
                               (projection-commands--get-command project cmd-type))))
             when (stringp value)
               collect (cons cmd-type value))))

(defun projection-project-command--read-commands ()
  "Read one-or-more shell-commands configured for the current project."
  (if-let ((cands (projection-project-command--candidates)))
      (cl-loop
       for key in
       (thread-first
         ;; Select keys from cands interactively (note they will be casted to strings).
         (completing-read-multiple
          (projection--prompt "Run commands: " (projection--current-project))
          cands nil t)
         ;; Ensure their ordered in the same way as the original cands list.
         (cl-sort #'< :key
                  (lambda (it)
                    (cl-position
                     (intern it) projection-commands--registered-cmd-types :key #'car))))
       ;; Collect the mapped command, not the key.
       collect (alist-get (intern key) cands))
    (error "No shell commands configured for the current project type")))

;;;###autoload
(defun projection-project-command (cmds)
  "Interactively select and run one or command-types for the current project.
CMDS should be a list of shell commands that should be run one after the other.
If at least one of the commands fails then all the commands failed.

WARN At the moment this function only prompts and works with shell-commands."
  (interactive
   (list (projection-project-command--read-commands)))
  (compile (string-join cmds " &&\n  ")))

(provide 'projection-commands)
;;; projection-commands.el ends here
