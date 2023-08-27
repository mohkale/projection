;;; projection-core-cache.el --- Cache definition helpers for `projection' -*- lexical-binding: t; -*-

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

;; Provide helpers to declare and interact with variables that may be cached
;; in the current project.

;;; Code:

(require 'project)

(defmacro projection--define-cache (symbol &optional docstring)
  "Define a new project cache variable and bind to SYMBOL.
Use DOCSTRING as the variable docstring when provided."
  (declare (indent defun))
  `(defvar ,symbol (make-hash-table :test #'equal)
     ,docstring))

(projection--define-cache projection--project-cache
  "Default cache of various values per project.")



;; Cache accessor and modifiers

(defun projection--cache-get (project key &optional cache)
  "Retrieve a value with KEY from the `projection' cache for PROJECT.
When CACHE is given retrieve the entry from CACHE instead of
`projection--project-cache'."
  (or cache (setq cache projection--project-cache))

  (alist-get
   key
   (gethash
    (if (stringp project)
        project
      (project-root project))
    cache)))

(defun projection--cache-put (project key value &optional cache)
  "Update the entry for KEY to VALUE in the `projection' cache for PROJECT.
When CACHE is given retrieve the entry from CACHE instead of
`projection--project-cache'."
  (or cache (setq cache projection--project-cache))

  (let ((root (if (stringp project)
                  project
                (project-root project))))
    (if-let ((existing (gethash root cache)))
        (if-let ((pair (assq key existing)))
            (setcdr pair value)
          (setcdr existing
                  (append `((,key . ,value)
                            ,@(cdr existing)))))
      (puthash root `((,key . ,value)) cache))))

(defun projection--cache-get-with-predicate (project key predicate body)
  "Get a value from the `projection' cache and reset it if stale.
This is a helper function to both retrieve and set a cache entry for KEY in
the current PROJECT. If a cache entry for KEY exists in the current project
and its newer than the PREDICATE value then it will be returned. Otherwise
BODY will be called to determine a new value and it will be saved alongside
PREDICATE for a later invocation.

PREDICATE should be a comparable value that will change when the value
in the cache is stale. For example PREDICATE could be the modification
time of a file used by BODY. If the file is unchanged the cache is up to
date. If the FILE is modified PREDICATE will be updated and the CACHE is
stale. PREDICATE can be set to nil to never cache the result and always
call BODY. It can instead be set to t to always cache.

PROJECT is an optional argument. When set to nil calling this function is
essentially the same as just calling BODY directly."
  (let ((cached-value (when project
                        (projection--cache-get project key))))
    (if (and predicate
             cached-value
             (if (and (numberp predicate)
                      (numberp (car cached-value)))
                 (<= predicate (car cached-value))
               ;; Value is to always be cached.
               predicate))
        (cdr cached-value)
      (let ((resolved-value (funcall body)))
        (when (and predicate project resolved-value)
          (projection--cache-put
           project key (cons predicate resolved-value)))
        resolved-value))))

(defun projection--cache-modtime-predicate (&rest files)
  "Helper function to get the largest modtime of a series of files.
Will loop through each file in FILES and return the one with the most recent
modtime. If any files don't exist then they are expected to be regenerated
and will be set to having a modtime of `current-time'."
  (cl-assert (> (length files) 0) nil "Must supply at least one file. files=%s" files)

  (cl-loop for file in files
           with current-modtime = nil
           if (file-exists-p file)
               do (setq current-modtime
                        (float-time
                         (file-attribute-modification-time
                          (file-attributes file 'integer))))
           else
               do (setq current-modtime (float-time (current-time)))
           maximize current-modtime))



;;;###autoload
(defun projection-reset-project-cache (&optional all-projects hash-table)
  "Reset the cached project-type and commands for the current project.
With ALL-PROJECTS clear the cache for all projects and not just the current
one. Clear HASH-TABLE when given instead of `projection--project-cache'."
  (interactive "P")
  (or hash-table
      (setq hash-table projection--project-cache))
  (if all-projects
      (clrhash hash-table)
    (when-let ((project (project-current)))
      (remhash (project-root project) hash-table))))

(provide 'projection-core-cache)

;;; projection-core-cache.el ends here
