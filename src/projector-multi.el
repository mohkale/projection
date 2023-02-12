;;; projector-multi.el --- projector integration for `compile-multi'. -*- lexical-binding: t; -*-

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
(require 'projector-commands)

(defgroup projector-multi nil
  "Project type integration for `compile-multi'."
  :group 'projector
  :group 'compile-multi)

(defcustom projector-multi-extend-existing-config t
  "Include existing `compile-multi-config' in `projector-multi-compile'."
  :type 'boolean
  :group 'projector-multi)



(defconst projector-multi--project-type-commands-prefix "project:")
(defun projector-multi--project-type-commands (project-type)
  "Extract `projector-commands' `compile-multi' targets for PROJECT-TYPE."
  (cl-loop
   for (type _ cmd) in projector-commands--registered-cmd-types
   when (alist-get type (cdr project-type))
   collect (cons (concat projector-multi--project-type-commands-prefix
                         (symbol-name type))
                 cmd)))

(defun projector-multi--project-triggers (project)
  "Extract all `compile-multi' triggers for the PROJECT."
  (when-let ((project-types (projector-project-types (project-root project))))
    (cl-loop
     for (type . config) in project-types
     with first = t
     when first
       append (projector-multi--project-type-commands (cons type config))
       and do (setq first nil)
     append (alist-get 'targets config))))

;;;###autoload
(defun projector-multi-compile ()
  "Variant of `compile-multi' which includes project specific targets."
  (interactive)
  (let* ((project (projector--current-project 'no-error))
         ;; Run compilations and generators from the project root.
         (default-directory (or (and project
                                     (project-root project))))
         (compile-multi-default-directory #'ignore)
         ;; Pre-pend the compile-multi triggers for the current project.
         (compile-multi-config
          (append `((t ,@(when project
                           (projector-multi--project-triggers project))))
                  (when projector-multi-extend-existing-config
                    compile-multi-config))))
    (call-interactively #'compile-multi)))

(provide 'projector-multi)
;;; projector-multi.el ends here
