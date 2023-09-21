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

(provide 'projection-core-misc)
;;; projection-core-misc.el ends here
