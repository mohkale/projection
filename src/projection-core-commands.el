;;; projection-core-commands.el --- Helper commands for `projection' -*- lexical-binding: t; -*-

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

;; Collection of commands for projection that don't have a more specific module.

;;; Code:

(require 'projection-core)

;;;###autoload
(defun projection-set-primary-project-type (project project-type)
  "Set the primary project type for PROJECT to PROJECT-TYPE.
With prefix argument prompt for all possible project types not just those
matching the current project."
  (interactive
   (let* ((project (projection--current-project))
          (current-type
           (projection-type--name
            (projection-project-type (project-root project))))
          (matching-types
           (mapcar #'projection-type--name
                   (if current-prefix-arg
                       projection-project-types
                     (projection-project-types (project-root project))))))
     (unless matching-types
       (error "No project types matching %s found" (project-root project)))

     (list
      project
      (intern
       (completing-read
        (projection--prompt "Set project type: " project)
        matching-types nil 'require-match nil current-type)))))

  (unless (eq project-type 'default)
    (projection--cache-put project 'type project-type)))

;;;###autoload
(defun projection-update-extra-project-types (project add-types remove-types)
  "Update the list of project types matching the PROJECT.
ADD-TYPES is a list of project types to start associating with PROJECT.
REMOVE-TYPES is a list of project types to stop associating with PROJECT."
  (interactive
   (let* ((project (projection--current-project))
          (current-types
           (delq 'default (mapcar #'projection-type--name (projection-project-types
                                                           (project-root project)))))
          (all-types
           (delq 'default (mapcar #'projection-type--name projection-project-types)))
          (add nil) (remove nil))
     (dolist (type (completing-read-multiple
                    (projection--prompt "Update project types: " project)
                    (append
                     (cl-loop
                      for type in current-types
                      collect (concat "-" (symbol-name type)))
                     (cl-loop
                      for type in all-types
                      unless (member type current-types)
                        collect (concat "+" (symbol-name type))))
                    nil 'require-match))
       (pcase (aref type 0)
         (?+ (push (intern (substring type 1)) add))
         (?- (push (intern (substring type 1)) remove))))
     (list
      project
      (nreverse add)
      (nreverse remove))))

   ;; Call to ensure the cache has been populated.
  (projection-project-type (project-root project))
  (projection-project-types (project-root project))

  (let* ((current-type (projection--cache-get project 'type))
         (current-types (projection--cache-get project 'types))
         (new-current-types
          (append
           (seq-remove (lambda (type) (member type remove-types))
                       current-types)
           (seq-remove (lambda (type) (member type current-types))
                       add-types))))
    (unless new-current-types
      (user-error "Cannot remove all supported project types"))
    (projection--cache-put project 'types new-current-types)
    (when (member current-type remove-types)
      ;; Cycle forward to the next included project type.
      (projection--cache-put project 'type (car new-current-types)))))



;;;###autoload
(defun projection-show-project-info (project project-types)
  "Display info for the PROJECT with respect to PROJECT-TYPES."
  (interactive
   (let ((project (projection--current-project)))
     (list
      project
      (mapcar #'projection-type--name
              (projection-project-types (project-root project))))))

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

(provide 'projection-core-commands)
;;; projection-core-commands.el ends here
