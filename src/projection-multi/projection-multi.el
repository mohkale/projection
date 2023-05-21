;;; projection-multi.el --- Projection integration for `compile-multi' -*- lexical-binding: t; -*-

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Keywords: project, convenience
;; Package-Requires: ((emacs "29") (projection "0.1") (compile-multi "0.3"))
;; Version: 0.1
;; Homepage: https://github.com/mohkale/projection

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

;; Helpers for calling `compile-multi' with project specific targets.
;;
;; This library exposes a command `projection-multi-compile' which invokes
;; `compile-multi' with a project specific compilation targets. This includes
;; targets for available commands for the current project type (configure,
;; compile, etc. See `projection-commands') and also for build frameworks
;; included in the current project. For example `projection-multi-compile'
;; will include tox targets when the project matches the tox project type.
;; This latter feature works independently of the main project type associated
;; with the current project. A project matching both tox and CMake will
;; offer both tox and CMake targets with `projection-multi-compile'.

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
     append (ensure-list (alist-get :targets config)))))

(cl-defun projection-multi-compile--run (project triggers)
  "Run `compile-multi' TRIGGERS for PROJECT."
  (let* (;; Run compilations and generators from the project root.
         (default-directory (or (when project
                                  (project-root project))
                                default-directory))
         (compile-multi-default-directory #'ignore)
         (compile-multi-config triggers))
    (call-interactively #'compile-multi)))

;;;###autoload
(defun projection-multi-compile ()
  "Variant of `compile-multi' which includes project specific targets."
  (interactive)
  (let ((project (projection--current-project 'no-error)))
    (projection-multi-compile--run
     project
     (append `((t ,@(when project
                      (projection-multi--project-triggers project))))
             (when projection-multi-extend-existing-config
               compile-multi-config)))))

;;;###autoload
(with-eval-after-load 'projection-types
  (projection-register-type 'cmake
    :targets (list 'projection-multi-cmake-targets
                   'projection-multi-ctest-targets)))

(provide 'projection-multi)
;;; projection-multi.el ends here
