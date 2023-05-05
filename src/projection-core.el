;;; projection-core.el --- Core library for `projection' -*- lexical-binding: t; -*-

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

;; Core functions for usage of `projection' including project registration, value
;; caching and helper functions needed across multiple other `projection' Lisp
;; files.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'projection-core-log)

(defcustom projection-types nil
  "Alist of defined project types and metadata for them.
You shouldn't modify this variable directly, instead you should do so
with `projection-register-type'."
  :group 'projection
  :type
  '(list
    (repeat
     (cons
      symbol                                                 ; Project identifier
      (alist :key-type symbol                                ; Command type name
             :value-type                                     ; Command value
             (choice string                                  ; Shell command
                     ;; Either a command or a function returning
                     ;; a shell command or a interactive function.
                     function))))))

(defcustom projection-default-type
  '((build   . "make")
    (test    . "make test")
    (run     . "make run")
    (install . "make install"))
  "Default project type.
Used when no other registered type matches the current project."
  :group 'projection
  :type '(optional (alist :key-type symbol :value-type (choice string function))))

(cl-defun projection-register-type
    (project &rest extra-keys
             &key predicate
             configure build test run package install
             src-dir test-dir test-prefix test-suffix
             &allow-other-keys)
  "Register or update entries in `projection-types'.
PROJECT should be the name of the project entry as a symbol.

PREDICATE is used to assert whether the current project matches PROJECT.
It can be a string matching a file-name accessible from the root of the
project or for more complicated project types it should be a function
returning a boolean. You can also supply a list of either of these and
if a project matches any of them then PROJECT will be matched.

CONFIGURE, BUILD, TEST, RUN, PACKAGE and INSTALL should be commands to
perform the matching operations; if PROJECT doesn't support any of these
then omit them. A command can be a string (for a shell command), a
function which will be called interactively if it's a command (`commandp')
or will be called as is and should return either of the other possible
value types.

TEST-SUFFIX and TEST-PREFIX set the prefix and suffix that are associated
test files. For example a source code file like foo.cpp could have a test
file of foo.t.cpp with a test-suffix of \".t\". Similarly a python module
like foo.py could have a test file of test_foo.py with a test-prefix of
\"test_\". These options configure this for projects of type PROJECT and
will be used for jumping between these related files or otherwise
associating them to each other. This can be supplied as either a single
value or a list of values but it will be saved as a list.

SRC-DIR, and TEST-DIR are currently unused. EXTRA-KEYS is part of the
call signature but will contain all key value arguments. It's usage is
internal to project registration."
  (declare (indent defun))

  ;; All of the standard keys arguments are in extra-keys. Their kept
  ;; as part of the caller interface so the docstring mentions it.
  (ignore predicate configure build test run package install src-dir
          test-dir test-prefix test-suffix)

  (let ((config
         (let ((extra-keys extra-keys) result)
           (while extra-keys
             (let ((key (car extra-keys))
                   (val (cadr extra-keys)))
               (when (member key '(:test-prefix :test-suffix))
                 (setq val (ensure-list val)))
               (push (cons key val) result))
             (setq extra-keys (cddr extra-keys)))
           (nreverse result))))
    (if-let ((existing (assoc project projection-types)))
        (progn
          (projection--log
           :debug "Updating project of type=%s with config=%s" project config)
          (dolist (it config)
            (setf (alist-get (car it) (cdr existing)) (cdr it)))
          existing)
      (projection--log
       :debug "Defining project of type=%s with conf=%s" project config)
      (push (cons project config) projection-types))))
(put 'projection-register-type 'lisp-indent-function 1)



(defmacro projection--define-cache (symbol &optional docstring)
  "Define a new project cache variable and bind to SYMBOL.
Use DOCSTRING as the variable docstring when provided."
  (declare (indent defun))
  `(defvar ,symbol (make-hash-table :test #'equal)
     ,docstring))

(projection--define-cache projection--project-cache
  "Cache of previous various values per project.")

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
  "Helper function to get the larget modtime of a series of files.
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



(defun projection--project-matches-p (root-dir project-type project-config)
  "Assert whether project of type PROJECT-TYPE matches ROOT-DIR.
PROJECT-CONFIG is the configuration for PROJECT-TYPE."
  (let ((default-directory root-dir))
    (if-let ((predicate (alist-get :predicate project-config)))
        (progn
          (setq predicate
                (if (and (consp predicate)
                         (functionp predicate))
                    (list predicate)
                  ;; For some reason this can't handle closures accurately.
                  (ensure-list predicate)))
          (cl-loop for it in predicate
                   when (cond
                         ((stringp it)
                          (file-exists-p it))
                         ((functionp it)
                          (funcall it))
                         (t
                          (user-error "Unknown project predicate type %s: %S" project-type it)))
                   return t
                   finally return nil))
      (warn "Project with no predicate in `projection-types': %s" project-type))))

(defun projection--match-project-type (root-dir)
  "Match project type for ROOT-DIR from `projection-types'."
  (cl-loop
   for (project . config) in projection-types
   when (projection--project-matches-p root-dir project config)
     return (cons project config)))

(defun projection--match-project-types (root-dir)
  "Match all project types for ROOT-DIR from `projection-types'."
  (cl-loop
   for (project . config) in projection-types
   when (projection--project-matches-p root-dir project config)
     collect (cons project config)))

(defun projection-project-type (root-dir &optional must-match)
  "Determine the project type for ROOT-DIR.
With MUST-MATCH an error will be raised if no project type could be matched."
  (or
   (when-let ((type (projection--cache-get root-dir 'type)))
     (assoc type projection-types))
   (when-let ((config (projection--match-project-type root-dir)))
     (projection--cache-put root-dir 'type (car config))
     config)
   (when must-match
     (error "Could not determine current project type for %s" root-dir))
   (cons t projection-default-type)))

(defun projection-project-types (root-dir &optional must-match)
  "Determine all project types matching ROOT-DIR.
With MUST-MATCH an error will be raised if no project types could be matched."
  (list (projection-project-type root-dir must-match))
  (or
   (when-let ((types (projection--cache-get root-dir 'types)))
     (delq nil
           (mapcar (lambda (type)
                     (assoc type projection-types))
                   types)))

   (when-let ((config (projection--match-project-types root-dir)))
     (projection--cache-put root-dir 'types (mapcar #'car config))
     config)

   (when must-match
     (error "Could not determine any project types for %s" root-dir))

   (list (cons t projection-default-type))))



(defun projection--current-project (&optional no-error)
  "Retrieve the current project or raise an error.
If NO-ERROR then don't raise an error if the project could not be resolved."
  ;; TODO: Maybe this is worth caching locally in the current buffer as well.
  (if-let ((project (project-current)))
      project
    (unless no-error
      (user-error "No project found relative to %s" default-directory))))

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



(cl-defmethod projection--project-info (project (_project-type (eql t)))
  "Determine an alist of configurations for the PROJECT-TYPE in PROJECT."
  ;; The default list just contains the project directory.
  `(("Project dir" . ,(project-root project))))

;;;###autoload
(defun projection-show-project-info (project project-types)
  "Display info for the PROJECT with respect to PROJECT-TYPES."
  (interactive
   (let ((project (projection--current-project)))
     (list
      project
      (mapcar #'car (projection-project-types (project-root project))))))

  (when (member t project-types)
    (setq project-types (delq t project-types)))

  (message
   (cl-loop
    for (key . value) in
    (append
     (projection--project-info project t)
     (if project-types
         `(("Project type" . ,(symbol-name (car project-types)))
           ,@(when (cdr project-types)
               `(("Extra project types" .
                  ,(string-join
                    (mapcar #'symbol-name (cdr project-types))
                    ",")))))
       '(("Project type" . "Unknown")))
     (cl-loop
      for project-type in project-types
      append (condition-case err
                 (projection--project-info (project-current) project-type)
               (error (unless (eq (car err) 'cl-no-applicable-method)
                        (signal (car err) (cdr err)))))))
    with first = t
    if first
      do (setq first nil)
    else
      concat " ## "
    concat key
    concat ": "
    concat value)))

(provide 'projection-core)
;;; projection-core.el ends here
