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

(require 'projection-core-completion)
(require 'projection-core-misc)

(defmacro projection--define-cache (symbol &optional docstring)
  "Define a new project cache variable and bind to SYMBOL.
Use DOCSTRING as the variable docstring when provided."
  (declare (indent defun))
  `(defvar ,symbol (make-hash-table :test #'equal)
     ,docstring))

(projection--define-cache projection--project-cache
  "Default cache of various values per project.")

(defvar projection--cache-var-alist nil
  "Alist containing registered cache variable properties.
Any cache variables declared with `projection--declare-cache-var' will be
persisted here. The format is an alist with the car being the variable key
\(as would be passed to `projection--cache-put') and the cdr being a plist
of properties for that key.")

(cl-defun projection--declare-cache-var (key &key title category description hide cache-var)
  "Declare the properties of a cached project variable.
KEY should be a identifier for the variable as would be passed to
`projection--cache-put'. TITLE is what the user will be prompted with to select
KEY. CATEGORY is a minibuffer completion category. DESCRIPTION is a full text
description for the key. HIDE if set will not display this variable when it is
unset. CACHE-VAR is the symbol of the variable where KEY will be persisted. If
unset CACHE-VAR and CATEGORY will receive default values."
  (declare (indent defun))
  (or category (setq category "Project"))
  (or cache-var (setq cache-var 'projection--project-cache))

  (setf (alist-get key projection--cache-var-alist)
        `(,@(when title       (list :title title))
          ,@(when category    (list :category category))
          ,@(when description (list :description description))
          ,@(when hide        (list :hide hide))
          ,@(when cache-var   (list :cache-var cache-var)))))



;; Cache accessor and modifiers

(cl-defsubst projection--cache-key (project)
  "Generate a unique key to associate a cache for PROJECT."
  (if (stringp project)
      project
    (project-root project)))

(defun projection--cache-get (project key &optional cache)
  "Retrieve a value with KEY from the `projection' cache for PROJECT.
When CACHE is given retrieve the entry from CACHE instead of
`projection--project-cache'."
  (or cache (setq cache projection--project-cache))
  (alist-get key (gethash (projection--cache-key project) cache)))

(defun projection--cache-put (project key value &optional cache)
  "Update the entry for KEY to VALUE in the `projection' cache for PROJECT.
When CACHE is given retrieve the entry from CACHE instead of
`projection--project-cache'."
  (or cache (setq cache projection--project-cache))

  (let ((project-key (projection--cache-key project)))
    (if-let ((existing (gethash project-key cache)))
        (if-let ((pair (assq key existing)))
            (setcdr pair value)
          (setcdr existing
                  (append `((,key . ,value)
                            ,@(cdr existing)))))
      (puthash project-key `((,key . ,value)) cache)))
  value)

(defun projection--cache-remove (project key &optional cache)
  "Remove the entry for KEY in PROJECT.
When CACHE is given retrieve the entry from CACHE instead of
`projection--project-cache'."
  (or cache (setq cache projection--project-cache))

  (let ((project-key (projection--cache-key project)))
    (when-let ((existing (gethash project-key cache)))
      (puthash project-key (assoc-delete-all key existing) cache))))

(defun projection--cache-all-cache-tables ()
  "Determine the list of all known cache tables."
  (seq-uniq
   (append
    (list 'projection--project-cache)
    (mapcar
     (lambda (cache-config)
       (plist-get (cdr cache-config) :cache-var))
     projection--cache-var-alist))))

(defun projection--cache-get-with-predicate (project key predicate body)
  "Get a value from the `projection' cache and reset it if stale.
This is a helper function to both retrieve and set a cache entry for KEY in
the current PROJECT. If a cache entry for KEY exists in the current project
and its newer than the PREDICATE value then it will be returned. Otherwise
BODY will be called to determine a new value and it will be saved alongside
PREDICATE for a later retrieval.

PREDICATE should be a comparable value that will change when the value
in the cache is stale. For example PREDICATE could be the modification
time of a file used by BODY. If the file is unchanged the cache is up to
date. If the FILE is modified PREDICATE will be updated and the CACHE is
stale. PREDICATE can be set to nil to never cache the result and always
call BODY. It can instead be set to t to always cache. PREDICATE can also
be a function that is supplied a plist containing a :value and :stamp
field and should return one of the aforementioned PREDICATE values.

PROJECT is an optional argument. When set to nil calling this function is
essentially the same as just calling BODY directly."
  (let ((cached-value (when project
                        (projection--cache-get project key))))
    (when (functionp predicate)
      (setq predicate
            (if cached-value
                (funcall predicate
                         (list :value (cdr cached-value)
                               :stamp (car cached-value)))
              (projection--cache-now))))

    (if (and cached-value
             (if (and (numberp predicate)
                      (numberp (car cached-value)))
                 (<= predicate (car cached-value))
               predicate))
        (cdr cached-value)
      (let ((resolved-value (funcall body)))
        (when (and project predicate resolved-value)
          (projection--cache-put
           project key (cons predicate resolved-value)))
        resolved-value))))

(defun projection--cache-now ()
  "Get the current timestamp."
  (float-time (current-time)))

(defun projection--cache-file-modtime (file)
  "Fetch the last-modtime of FILE as a float."
  (float-time
   (file-attribute-modification-time
    (file-attributes file 'integer))))

(defun projection--cache-modtime-predicate (&rest files)
  "Helper function to get the largest modtime of a series of files.
Will loop through each file in FILES and return the one with the most recent
modtime. If any files don't exist then they are expected to be regenerated
and will be set to having a modtime of `current-time'."
  (cl-assert (> (length files) 0) nil "Must supply at least one file. files=%s" files)

  (cl-loop for file in files
           with current-modtime = nil
           if (file-exists-p file)
               do (setq current-modtime (projection--cache-file-modtime file))
           else
               do (setq current-modtime (projection--cache-now))
           maximize current-modtime))



;; Interactive commands for clearing projection cache variables.

(defun projection--list-cache-vars (project)
  "Fetch a list of cache-vars and properties for PROJECT.
The result of this is intended to be used in a `completing-read' interface."
  (let (result)
    (dolist (cache-config (reverse projection--cache-var-alist))
      (cl-destructuring-bind (&key title category hide cache-var &allow-other-keys)
          (cdr cache-config)
        (when-let ((cache (and (boundp cache-var)
                               (symbol-value cache-var))))
          (let ((value (projection--cache-get project (car cache-config) cache)))
            (when (or (not hide) value)
              (push (list title category value (car cache-config) cache) result))))))
    (nreverse result)))

(defun projection--cache-vars-completion-table (cache-vars)
  "Generate a completion-table for reading CACHE-VARS."
  (projection-completion--completion-table
   :candidates cache-vars
   :category 'projection-cache-vars
   :annotation-function
   (projection-completion--annotation-function
    :key-function (lambda (cand)
                    (thread-last
                      (assoc cand cache-vars)
                      (caddr)
                      (format "%S"))))
   :group-function
   (lambda (cand transform)
     (if transform cand
       (car (alist-get cand cache-vars nil nil #'string-equal))))))

(defun projection-cache-clear--read-cache-var ()
  "Read cache vars for the current project.
Returns a list of the current project the cache-var and the cache table it is
set in."
  (let* ((project (projection--current-project))
         (cache-vars (projection--list-cache-vars project))
         (selected-cache-var (completing-read
                              (projection--prompt "Clear cache: " project)
                              (projection--cache-vars-completion-table cache-vars)
                              nil 'require-match)))
    (append (list project)
            (cddr (alist-get selected-cache-var cache-vars nil nil #'string-equal)))))

(defun projection-cache-clear-single (project key cache)
  "Clear the value of KEY in CACHE for PROJECT.
This command interactively removes a cached project variable."
  (interactive
   (projection-cache-clear--read-cache-var))
  (projection--cache-remove project key cache))

(defun projection-cache-clear-all (project)
  "Clear the value of all keys in all caches for PROJECT."
  (interactive (list (projection--current-project)))
  (let ((key (projection--cache-key project)))
    (dolist (cache-table (projection--cache-all-cache-tables))
      (when-let ((table (and (boundp cache-table)
                             (symbol-value cache-table))))
        (remhash key table)))))

(define-obsolete-function-alias 'projection-reset-project-cache 'projection-cache-clear "0.1")

;;;###autoload
(defun projection-cache-clear (clear-all)
  "Interactively clear cached variables for the current project.
Calls `projection-cache-clear-all' or `projection-cache-clear-single' based on
the value of CLEAR-ALL."
  (interactive "P")
  (call-interactively
   (if clear-all
       #'projection-cache-clear-all
     #'projection-cache-clear-single)))

(provide 'projection-core-cache)
;;; projection-core-cache.el ends here
