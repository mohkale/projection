;;; projection-multi.el --- projection integration for `compile-multi'. -*- lexical-binding: t; -*-

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

;; TODO

;;; Code:

(require 'compile-multi)
(require 'projection-commands)

(defgroup projection-multi nil
  "Project type integration for `compile-multi'."
  :group 'projection
  :group 'compile-multi)

(defcustom projection-multi-extend-existing-config t
  "Include existing `compile-multi-config' in `projection-multi-compile'."
  :type 'boolean
  :group 'projection-multi)



(defconst projection-multi--project-type-commands-prefix "project:")
(defun projection-multi--project-type-commands (project-type)
  "Extract `projection-commands' `compile-multi' targets for PROJECT-TYPE."
  (cl-loop
   for (type _ cmd) in projection-commands--registered-cmd-types
   when (alist-get type (cdr project-type))
   collect (cons (concat projection-multi--project-type-commands-prefix
                         (symbol-name type))
                 cmd)))

(defun projection-multi--project-triggers (project)
  "Extract all `compile-multi' triggers for the PROJECT."
  (when-let ((project-types (projection-project-types (project-root project))))
    (cl-loop
     for (type . config) in project-types
     with first = t
     when first
       append (projection-multi--project-type-commands (cons type config))
       and do (setq first nil)
     append (alist-get 'targets config))))

;;;###autoload
(defun projection-multi-compile ()
  "Variant of `compile-multi' which includes project specific targets."
  (interactive)
  (let* ((project (projection--current-project 'no-error))
         ;; Run compilations and generators from the project root.
         (default-directory (or (and project
                                     (project-root project))))
         (compile-multi-default-directory #'ignore)
         ;; Pre-pend the compile-multi triggers for the current project.
         (compile-multi-config
          (append `((t ,@(when project
                           (projection-multi--project-triggers project))))
                  (when projection-multi-extend-existing-config
                    compile-multi-config))))
    (call-interactively #'compile-multi)))

(provide 'projection-multi)
;;; projection-multi.el ends here
