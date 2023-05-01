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
           :debug "Stderr for shell command='%s' was\n%s" command stderr)))

    (unless (string-empty-p stdout)
      stdout)))

(defun projection--shell-command-to-string-1 (command)
  "Run COMMAND in a subshell and return (stdout . stderr)."
  (let ((stderr nil)
        (stdout nil))
    (setq stdout
          (with-output-to-string
            (with-temp-buffer
              (cl-letf (((symbol-function #'display-buffer)
                         (symbol-function #'ignore)))
                (shell-command command standard-output (current-buffer)))
              (setq stderr
                    (buffer-substring-no-properties (point-min) (point-max))))))
    (cons stdout stderr)))

(defmacro projection--with-shell-command-buffer (command &rest body)
  "Run BODY in a temporary buffer with the output of COMMAND."
  (declare (indent defun))
  `(with-temp-buffer
     (insert (or (projection--shell-command-to-string ,command) ""))
     (goto-char (point-min))
     (unless (string-empty-p (buffer-substring (point-min) (point-max)))
       ,@body)))



;; General

(defun projection--all-files-exists (&rest files)
  "Generate a predicate function which is true if all files in FILES exist."
  (apply-partially #'cl-every #'file-exists-p files))

(provide 'projection-utils)
;;; projection-utils.el ends here
