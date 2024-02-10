;;; projection-utils.el --- Helper module for code shared between other `projection' modules. -*- lexical-binding: t; -*-

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

;; This library defines a bunch of common routines shared across other
;; libraries in `projection'.

;;; Code:

(require 'cl-extra)
(require 'project)

(require 'projection-core-log)



;; Shell

(defun projection--command-or-shell (func shell-command)
  "Generate a command function which will run either FUNC or SHELL-COMMAND.
The result is a lambda which, if FUNC is bound and interactive returns FUNC,
otherwise it will return SHELL-COMMAND."
  (lambda ()
    (if (commandp func)
        func
      shell-command)))

(defun projection--join-shell-command (argv)
  "Join quoted arguments from ARGV into a shell command."
  (string-join (mapcar #'shell-quote-argument argv) " "))

(defun projection--join-shell-commands (argvs)
  "Join multiple shell commands ARGVS conditionally chaining them."
  (string-join
   (thread-last
     argvs
     (mapcar #'ensure-list)
     (mapcar #'projection--join-shell-command))
   " && "))

(defun projection--env-shell-command-prefix (env-alist &optional cwd)
  "Construct a shell command prefix for ENV-ALIST and CWD.
If ENV-ALIST and CWD is empty then return nil."
  (when (or env-alist cwd)
    `("env"
      ,@(when cwd
          (list (concat "--chdir=" cwd)))
      ,@(cl-loop for (key . value) in
                     env-alist
                     collect (concat key "=" value)))))

(defun projection--shell-command-to-string (command)
  "Run COMMAND in a subshell and return the standard output."
  (projection--log :debug "Running shell command='%s'" command)
  (cl-destructuring-bind (stdout . stderr)
      (projection--shell-command-to-string-1 command)
    (cl-loop
     for (label . output) in `(("stdout" . ,stdout)
                               ("stderr" . ,stderr))
     do (if (string-empty-p output)
            (projection--log
             :debug "Shell command='%s' produced no %s output" command label)
          (projection--log
           :debug "%s for shell command='%s' was\n%s" label command output)))

    (unless (string-empty-p stdout)
      stdout)))

(defun projection--shell-command-to-string-1 (command)
  "Run COMMAND in a subshell and return (stdout . stderr)."
  (let ((stderr-buffer (generate-new-buffer " *projection-stderr*" 'inhibit-buffer-hooks)))
    (unwind-protect
        (cons
         (with-output-to-string
           (with-current-buffer standard-output
             (save-window-excursion
               (shell-command command standard-output stderr-buffer))))

         (with-current-buffer stderr-buffer
           (buffer-substring-no-properties (point-min) (point-max))))
      (kill-buffer stderr-buffer))))

(defmacro projection--with-shell-command-buffer (command &rest body)
  "Run BODY in a temporary buffer with the output of COMMAND."
  (declare (indent defun))
  ;; We evaluate the command before we move to a temporary buffer to ensure
  ;; if it depends on any buffer local variables then they are available.
  `(let ((projection--with-shell-command-buffer-command ,command))
     (with-temp-buffer
       (insert (or
                (projection--shell-command-to-string
                 projection--with-shell-command-buffer-command)
                ""))
       (goto-char (point-min))
       (unless (string-empty-p (buffer-substring (point-min) (point-max)))
         ,@body))))

(defun projection--shell-command (project cmd)
  "Variant of `project-shell-command' to run CMD in PROJECT."
  ;; (interactive
  ;;  (let ((project (projection--current-project)))
  ;;    (list project
  ;;          (read-shell-command
  ;;           (projection--prompt "Shell command: " project)))))
  (let ((default-directory (project-root project)))
    (funcall-interactively #'shell-command cmd)))



;; Collections

(defun projection--uniquify (collection)
  "Helper to remove duplicates from COLLECTION."
  (thread-last
    collection
    (append (list (make-hash-table :test 'equal)))
    (cl-reduce (lambda (hash-table target)
                 (puthash target t hash-table)
                 hash-table))
    (hash-table-keys)))



;; General

(defun projection--guess-parallelism (jobs)
  "Query the available CPU cores respecting JOBS.
See `projection-build-jobs' for supported values for JOBS."
  (when jobs
    (pcase jobs
      ('ninja
       (pcase (num-processors)
         ((or 0 1) 1)
         (2 3)
         (_ (+ (num-processors) 2))))
      (-1 (num-processors))
      (-2 (/ (num-processors) 2))
      (0 nil)
      ((cl-type integer) jobs)
      (_ (projection--log :warning "Unsupported `jobs' argument: %S." jobs)
         nil))))

(defun projection--all-files-exists (&rest files)
  "Generate a predicate function which is true if all files in FILES exist."
  (apply-partially #'cl-every #'file-exists-p files))

(provide 'projection-utils)
;;; projection-utils.el ends here
