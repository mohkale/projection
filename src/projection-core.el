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
(require 'eieio)
(require 'project)
(require 'subr-x)

(require 'projection-core-log)

(defclass projection-type ()
  ((name
    :initarg :name
    :custom symbol
    :reader projection-type--name
    :documentation "Identifier for the current project type.
This should be unique within the body of the variable `projection-project-types'.
A special name of default is accepted when no specialised project type is applicable.")
   (predicate
    :initarg :predicate
    :custom (choice
             (const t :tag "Always supported")
             (function :tag "Predicate function")
             (string :tag "Marker file for project")
             (repeat (string :tag "Marker files for project")))
    :documentation "Predicate used to assert whether the current project matches this project type.
Predicate can be a string matching a file-name accessible from the root of the
project or for more complicated project types it should be a function returning a
boolean. You can also supply a list of either of these and if a project matches any
of them then PROJECT will be matched.")
   ;; Compilation commands
   (configure
    :initarg :configure
    :initform nil
    :custom (choice
             (const nil :tag "Project does not support configure.")
             (string :tag "Shell command to invoke.")
             (function :tag "Either a command or a function returning a valid command type."))
    :documentation "Compilation ommand used to configure this project.
When nil the project is interpreted as not supporting this command type. When a
string that string will be passed to `compile' as a shell command. When a function
then the behaviour depends on the type of function. If the function is interactive
(`commandp') it will be called interactively as a command. Otherwise the function
will be called and the result should be one of the other supported command types.")
   (build
    :initarg :build
    :initform nil
    :custom (choice
             (const nil :tag "Project does not support build.")
             (string :tag "Shell command to invoke.")
             (function :tag "Either a command or a function returning a valid command type."))
    :documentation "Compilation command used to build this project.
This is of the same type and semantics as the configure slot.")
   (test
    :initarg :test
    :initform nil
    :custom (choice
             (const nil :tag "Project does not support test.")
             (string :tag "Shell command to invoke.")
             (function :tag "Either a command or a function returning a valid command type."))
    :documentation "Command used to test project.
This is of the same type and semantics as the configure slot.")
   (run
    :initarg :run
    :initform nil
    :custom (choice
             (const nil :tag "Project does not support run.")
             (string :tag "Shell command to invoke.")
             (function :tag "Either a command or a function returning a valid command type."))
    :documentation "Command used to run project.
This is of the same type and semantics as the configure slot.")
   (package
    :initarg :package
    :initform nil
    :custom (choice
             (const nil :tag "Project does not support package.")
             (string :tag "Shell command to invoke.")
             (function :tag "Either a command or a function returning a valid command type."))
    :documentation "Command used to package project.
This is of the same type and semantics as the configure slot.")
   (install
    :initarg :install
    :initform nil
    :custom (choice
             (const nil :tag "Project does not support install.")
             (string :tag "Shell command to invoke.")
             (function :tag "Either a command or a function returning a valid command type."))
    :documentation "Command used to install project.
This is of the same type and semantics as the configure slot.")
   ;; File navigation
   (src-dir  :initarg :src-dir  :initform nil :documentation "Currently unused.")
   (test-dir :initarg :test-dir :initform nil :documentation "Currently unused.")
   (test-prefix
    :initarg :test-prefix
    :initform nil
    :custom (choice
             (string :tag "Test file prefix")
             (repeat (string :tag "Test file prefixes")))
    :documentation "Possible prefixes for a file to treat as a test file.
For example foo.cpp could have a related test at test_foo.cpp file with the prefix
being test_.")
   (test-suffix
    :initarg :test-suffix
    :initform nil
    :custom (choice
             (string :tag "Test file prefix")
             (repeat (string :tag "Test file prefixes")))
    :documentation "Possible suffixes for a file to treat as a test file.
For example foo.cpp could have a related test foo_test.cpp file with the suffix being
_test.")
   ;; Multi compile
   (compile-multi-targets
    :initarg :compile-multi-targets
    :initform nil
    :custom '(choice
              function
              (list (repeat function)))
    :documentation "Supported `compile-multi' targets for the current project type.
This is only used with the optional `projection-multi' package. The value is any action
supported by `compile-multi-config'."))
  "Base class for a supported project type in `projection'.")

(cl-defmethod initialize-instance :after ((obj projection-type) &rest _args)
  "Initialise a new projection type object."
  (unless (slot-boundp obj :name)
    (error "Must define the :name slot for a `projection-type' object"))
  (when (slot-boundp obj 'test-prefix)
    (oset obj test-prefix (ensure-list (oref obj test-prefix))))
  (when (slot-boundp obj 'test-suffix)
    (oset obj test-suffix (ensure-list (oref obj test-suffix)))))

(defcustom projection-project-types nil
  "List of defined project types in order of precedence."
  :group 'projection
  :type '(list (repeat projection-type)))

(defcustom projection-default-type
  (projection-type
   :name    'default
   :build   "make"
   :test    "make test"
   :run     "make run"
   :install "make install")
  "Default project type.
Used when no other registered type matches the current project."
  :group 'projection
  :type '(optional projection-type))



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



(defun projection--project-matches-p (root-dir project-type)
  "Assert whether project of type PROJECT-TYPE matches ROOT-DIR."
  (let ((default-directory root-dir))
    (if-let ((predicate (oref project-type predicate)))
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
                          (user-error "Unknown project predicate type %s: %S"
                                      (projection-type--name project-type)
                                      it)))
                   return t
                   finally return nil))
      (warn "Project with no predicate in `projection-project-types': %s"
            (projection-type--name project-type)))))

(defun projection--match-project-type (root-dir)
  "Match project type for ROOT-DIR from variable `projection-project-types'."
  (cl-loop
   for project-type in projection-project-types
   when (projection--project-matches-p root-dir project-type)
     return project-type))

(defun projection--match-project-types (root-dir)
  "Match all project types for ROOT-DIR from variable `projection-project-types'."
  (cl-loop
   for project-type in projection-project-types
   when (projection--project-matches-p root-dir project-type)
     collect project-type))

(defun projection--name-to-type (type)
  "Find the project in `projection-project-types' with name TYPE."
  (or
   (cl-find-if (lambda (project-type)
                 (eq (oref project-type name) type))
               projection-project-types)
   (user-error "Could not find project with name=%s" type)))

(defun projection-project-type (root-dir &optional must-match)
  "Determine the project type for ROOT-DIR.
With MUST-MATCH an error will be raised if no project type could be matched."
  (or
   (when-let ((type (projection--cache-get root-dir 'type)))
     (projection--name-to-type type))

   (when-let ((project-type (projection--match-project-type root-dir)))
     (projection--cache-put root-dir 'type (oref project-type name))
     project-type)

   (when (and must-match
              (not projection-default-type))
     (error "Could not determine current project type for %s" root-dir))

   projection-default-type))

(defun projection-project-types (root-dir &optional must-match)
  "Determine all project types matching ROOT-DIR.
With MUST-MATCH an error will be raised if no project types could be matched."
  (let ((matching-types
         (or
          (when-let ((types (projection--cache-get root-dir 'types)))
            (mapcar #'projection--name-to-type types))

          (when-let ((project-types (projection--match-project-types root-dir)))
            (projection--cache-put root-dir 'types
                                   (mapcar #'projection-type--name project-types))
            project-types))))

    ;; Ensure the default project-type is the first entry in the collection
    ;; of matching types. This is so any functions relying on this result can
    ;; expect on the invariant of (car types) == default-type.
    (when-let ((default-type (projection-project-type root-dir)))
      (when (and
             (not (eq default-type projection-default-type))
             (not (eq default-type (car matching-types))))
        (setq matching-types (append (list default-type)
                                     (delq default-type matching-types)))))

    (or
     matching-types

     (when (and must-match
                (not projection-default-type))
       (error "Could not determine any project types for %s" root-dir))

     (list projection-default-type))))



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

(provide 'projection-core)
;;; projection-core.el ends here
