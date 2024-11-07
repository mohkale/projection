;;; projection-core-type.el --- Project to project-type matchers -*- lexical-binding: t; -*-

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

;; Helpers to query the project type for the current project.

;;; Code:

(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(require 'projection-core-cache)
(require 'projection-core-type)

(defvar-local projection-primary-project-type nil
  "Override for the primary project type of the current project.
Set this to the name of a defined project type to force projection to use it as
the primary project type by default. You can still override the current project
type interactively with `projection-set-primary-project-type'. This is useful
for projects that support multiple possible project-types but the default
matched type isn't the primary type you'd like to use for the project. It's
recommended to set this in a dir-locals file.")

(defun projection--project-matches-p (root-dir project-type)
  "Assert whether project of type PROJECT-TYPE matches ROOT-DIR."
  (let ((default-directory root-dir))
    (if-let* ((predicate (oref project-type predicate)))
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
   (when-let* ((type (projection--cache-get root-dir 'type)))
     (projection--name-to-type type))

   (when-let* ((project-type
                (when projection-primary-project-type
                  (projection--name-to-type projection-primary-project-type))))
     project-type)

   (when-let* ((project-type (projection--match-project-type root-dir)))
     (projection--cache-put root-dir 'type (oref project-type name))
     project-type)

   (when (and must-match
              (not projection-default-type))
     (error "Could not determine current project type for %s" root-dir))

   projection-default-type))

(projection--declare-cache-var
  'type
  :title "Project type"
  :description "The primary type of the current project")

(defun projection-project-types (root-dir &optional must-match)
  "Determine all project types matching ROOT-DIR.
With MUST-MATCH an error will be raised if no project types could be matched."
  (let ((matching-types
         (or
          (when-let* ((types (projection--cache-get root-dir 'types)))
            (mapcar #'projection--name-to-type types))

          (when-let* ((project-types (projection--match-project-types root-dir)))
            (projection--cache-put root-dir 'types
                                   (mapcar #'projection-type--name project-types))
            project-types))))

    ;; Ensure the default project-type is the first entry in the collection
    ;; of matching types. This is so any functions relying on this result can
    ;; expect on the invariant of (car types) == default-type.
    (when-let* ((default-type (projection-project-type root-dir)))
      (when (and
             (not (eq default-type projection-default-type))
             (not (eq default-type (car matching-types))))
        (setq matching-types (append (list default-type)
                                     (delq default-type matching-types)))))

    ;; Ensure the locally overridden project-type is in the collection of matching
    ;; types. It doesn't necessarily have to be the primary type since you can
    ;; override it after the fact but it should always be in the list.
    (when-let* ((local-primary-project-type
               (when projection-primary-project-type
                 (projection--name-to-type projection-primary-project-type))))
      (unless (member local-primary-project-type matching-types)
        (setq matching-types (append (list (car matching-types)
                                           local-primary-project-type)
                                     (cdr matching-types)))))

    (or
     matching-types

     (when (and must-match
                (not projection-default-type))
       (error "Could not determine any project types for %s" root-dir))

     (list projection-default-type))))

(projection--declare-cache-var 'types
  :title "Project types"
  :description "All the project types matching the current project")

(provide 'projection-core-match)

;;; projection-core-match.el ends here
