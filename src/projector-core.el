;;; projector-core.el --- Core library for `projector' -*- lexical-binding: t; -*-

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

;; Core functions for usage of `projector' including project registration, value
;; caching and helper functions needed across multiple other `projector' Lisp
;; files.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)

(defcustom projector-types nil
  "Alist of defined project types and metadata for them.
You shouldn't modify this variable directly, instead you should do so
with `projector-register-type'."
  :group 'projector
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

(defcustom projector-default-type
  '((build   . "make")
    (test    . "make test")
    (run     . "make run")
    (install . "make install"))
  "Default project type.
Used when no other registered type matches the current project."
  :group 'projector
  :type '(optional (alist :key-type symbol :value-type (choice string function))))

(cl-defun projector-register-type
    (project &key predicate
             configure build test run package install
             src-dir test-dir test-prefix test-suffix
             targets)
  "Register or update entries in `projector-types'.
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

TARGETS specifies a collection of `compile-multi' targets for this project
type. See `compile-multi-config' for a description of the supported value.

SRC-DIR, and TEST-DIR are currently unused."
  (declare (indent defun))
  (let ((alist
         (cl-loop for (key . value) in `((predicate . ,predicate)
                                         ;; Compilation commands
                                         (configure . ,configure)
                                         (build . ,build)
                                         (test . ,test)
                                         (run . ,run)
                                         (package . ,package)
                                         (install . ,install)
                                         ;; Test file discovery
                                         (src-dir . ,src-dir)
                                         (test-dir . ,test-dir)
                                         (test-suffix . ,(ensure-list test-suffix))
                                         (test-prefix . ,(ensure-list test-prefix))
                                         ;; Compilation target generation
                                         (targets . ,(if (and targets
                                                              (symbolp targets))
                                                         (list targets)
                                                       targets)))
                  when value
                    collect (cons key value))))
    (if-let ((existing (assoc project projector-types)))
        (progn
          (cl-loop for (key . value) in alist
                   do (if-let ((pair (assq key (cdr existing))))
                          (setcdr pair value)
                        (push (cons key value) (cdr existing))))
          (assoc project projector-types))
      (push `(,project . ,alist) projector-types))))
(put 'projector-register-type 'lisp-indent-function 1)



(defmacro projector--define-cache (symbol &optional docstring)
  "Define a new project cache variable and bind to SYMBOL.
Use DOCSTRING as the variable docstring when provided."
  (declare (indent defun))
  `(defvar ,symbol (make-hash-table :test #'equal)
     ,docstring))

(projector--define-cache projector--project-cache
  "Cache of previous various values per project.")

;;;###autoload
(defun projector-reset-project-cache (&optional all-projects hash-table)
  "Reset the cached project-type and commands for the current project.
With ALL-PROJECTS clear the cache for all projects and not just the current
one. Clear HASH-TABLE when given instead of `projector--project-cache'."
  (interactive "P")
  (or hash-table
      (setq hash-table projector--project-cache))
  (if all-projects
      (clrhash hash-table)
    (when-let ((project (project-current)))
      (remhash (project-root project) hash-table))))

(defun projector--cache-get (project key &optional cache)
  "Retrieve a value with KEY from the `projector' cache for PROJECT.
When CACHE is given retrieve the entry from CACHE instead of
`projector--project-cache'."
  (or cache (setq cache projector--project-cache))

  (alist-get
   key
   (gethash
    (if (stringp project)
        project
      (project-root project))
    cache)))

(defun projector--cache-put (project key value &optional cache)
  "Update the entry for KEY to VALUE in the `projector' cache for PROJECT.
When CACHE is given retrieve the entry from CACHE instead of
`projector--project-cache'."
  (or cache (setq cache projector--project-cache))

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



(defun projector--project-matches-p (root-dir project-type project-config)
  "Assert whether project of type PROJECT-TYPE matches ROOT-DIR.
PROJECT-CONFIG is the configuration for PROJECT-TYPE."
  (let ((default-directory root-dir))
    (if-let ((predicate (alist-get 'predicate project-config)))
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
      (warn "Project with no predicate in `projector-types': %s" project-type))))

(defun projector--match-project-type (root-dir)
  "Match project type for ROOT-DIR from `projector-types'."
  (cl-loop
   for (project . config) in projector-types
   when (projector--project-matches-p root-dir project config)
     return (cons project config)))

(defun projector--match-project-types (root-dir)
  "Match all project types for ROOT-DIR from `projector-types'."
  (cl-loop
   for (project . config) in projector-types
   when (projector--project-matches-p root-dir project config)
     collect (cons project config)))

(defun projector-project-type (root-dir &optional must-match)
  "Determine the project type for ROOT-DIR.
With MUST-MATCH an error will be raised if no project type could be matched."
  (or
   (when-let ((type (projector--cache-get root-dir 'type)))
     (assoc type projector-types))
   (when-let ((config (projector--match-project-type root-dir)))
     (projector--cache-put root-dir 'type (car config))
     config)
   (when must-match
     (error "Could not determine current project type for %s" root-dir))
   (cons t projector-default-type)))

(defun projector-project-types (root-dir &optional must-match)
  "Determine all project types matching ROOT-DIR.
With MUST-MATCH an error will be raised if no project types could be matched."
  (list (projector-project-type root-dir must-match))
  (or
   (when-let ((types (projector--cache-get root-dir 'types)))
     (delq nil
           (mapcar (lambda (type)
                     (assoc type projector-types))
                   types)))

   (when-let ((config (projector--match-project-types root-dir)))
     (projector--cache-put root-dir 'types (mapcar #'car config))
     config)

   (when must-match
     (error "Could not determine any project types for %s" root-dir))

   (list (cons t projector-default-type))))



(defun projector--current-project (&optional no-error)
  "Retrieve the current project or raise an error.
If NO-ERROR then don't raise an error if the project could not be resolved."
  ;; TODO: Maybe this is worth caching locally in the current buffer as well.
  (if-let ((project (project-current)))
      project
    (unless no-error
      (user-error "No project found relative to %s" default-directory))))

(defun projector--prompt (prompt project &rest format-args)
  "Generate a prompt string for PROJECT with PROMPT.
FORMAT-ARGS will be used to format PROMPT if provided."
  (apply #'format
         (concat "[%s] " prompt)
         (file-name-nondirectory
          (string-remove-suffix
           "/" (project-root project)))
         format-args))



;;;###autoload
(defun projector-show-project-info ()
  "Display info for the current project."
  (interactive)
  (when-let* ((project (projector--current-project))
              (project-root (project-root project)))
    (message
     "Project dir: %s ## Project type: %s"
     project-root
     (car
      (projector-project-type project-root)))))

(provide 'projector-core)
;;; projector-core.el ends here
