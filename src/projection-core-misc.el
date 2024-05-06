;;; projection-core-misc.el --- Miscellaneous helper function for `projection' -*- lexical-binding: t; -*-

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

;; Location for miscellaneous functions used by `projection'.

;;; Code:

(require 'project)
(eval-when-compile (require 'subr-x))

(defun projection--current-project (&optional no-error)
  "Retrieve the current project or raise an error.
If NO-ERROR then don't raise an error if the project could not be resolved."
  ;; TODO: Maybe this is worth caching locally in the current buffer as well.
  (if-let ((project (project-current)))
      project
    (unless no-error
      (user-error "No project found relative to %s" default-directory))))

(defun projection--expand-file-name-in-project (file-name remote)
  "Helper to expand a file-name in the current project.
FILE-NAME is a path to a file optionally under the current project.
If absolute it is returned as is otherwise it is prefixed with the path
to the current project root. REMOTE is an option configuring the behaviour
of remote projects. When set to a path that is considered a remote TRAMP
component used to reach the current project. Otherwise when set to a truthy
value we prefix the remote component of the current project."
  (let ((project-root
         (or (when-let ((project
                         (projection--current-project 'no-error)))
               (project-root project))
             default-directory)))
    (if (file-name-absolute-p file-name)
        (concat
         (cond
          ((stringp remote) remote)
          (remote (file-remote-p project-root)))
         file-name)
      (expand-file-name file-name project-root))))

(defun projection--prompt (prompt project &rest format-args)
  "Generate a prompt string for PROJECT with PROMPT.
FORMAT-ARGS will be used to format PROMPT if provided."
  (if project
      (apply #'format
             (concat "[%s] " prompt)
             (file-name-nondirectory
              (string-remove-suffix
               "/" (project-root project)))
             format-args)
    (apply #'format prompt format-args)))



(defun projection--create-clear-directory-command (directory-gen)
  "Create a interactive callback to clear the directory at DIRECTORY-GEN.
DIRECTORY-GEN should be a:
- String or Symbol pointing to a string. This will be expanded relative to the
  project root.
- A function which returns the directory as a string. It should take a single
  argument which when true should make the function return a path that Emacs
  itself can query (such as with tramp-prefixes). The default return value
  should be a path that a compilation command could reference.

The returned command will check if the directory exists and prompt the user
before running a compilation command to delete it."
  (lambda ()
    (interactive)
    (cl-destructuring-bind (directory . directory-expanded)
        (pcase directory-gen
          ((pred functionp)
           (cons (funcall directory-gen nil)
                 (funcall directory-gen 'expand)))
          ((pred stringp)
           (cons directory-gen (projection--expand-file-name-in-project directory-gen t)))
          ((pred symbolp)
           (let ((directory (eval directory-gen)))
             (cons directory (projection--expand-file-name-in-project directory t))))
          (_ (error "Clear directory command passed unsupported directory-gen type: %S"
                    directory-gen)))
      (cond
       ((or (not directory) (not directory-expanded))
        (user-error "Unable to determine directory to remove using %S" directory-gen))
       ((not (file-exists-p directory-expanded))
        (user-error "Directory %s already does not exist" directory-expanded))
       ((yes-or-no-p (format "Really remove directory at `%s'?" directory-expanded))
        (let ((default-directory (project-root (projection--current-project))))
          (compile (concat "rm -rf " (shell-quote-argument directory)))))
       (t (message "Aborted removal of directory at %s" directory-expanded))))))

(provide 'projection-core-misc)
;;; projection-core-misc.el ends here
