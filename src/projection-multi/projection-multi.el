;;; projection-multi.el --- Projection integration for `compile-multi' -*- lexical-binding: t; -*-

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Keywords: project, convenience
;; Package-Requires: ((emacs "29.1") (projection "0.1") (compile-multi "0.5"))
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



;;;###autoload
(defun projection-multi-projection-targets (&optional project-type-prefix project)
  "`compile-multi' target generator function for `projection-commands' in PROJECT.
When set the generated targets will be prefixed with PROJECT-TYPE-PREFIX. If
not set PROJECT will be determined automatically.

The generated candidates will be of the form
prefix:project-type:project-command."
  (setq project-type-prefix (or project-type-prefix "project"))

  (when-let* ((project (or project (projection--current-project 'no-error)))
              (current-project-types
               (projection-project-types (project-root project))))
    (let (result)
      (dolist (project-config current-project-types)
        (dolist (cmd-type-config projection-commands--registered-cmd-types)
          (when-let ((cmd
                      (projection-commands--ignore-no-command
                       (funcall (cadr cmd-type-config) project project-config :use-cache nil))))
            (push `(,(concat project-type-prefix ":"
                             (symbol-name (oref project-config name)) ":"
                             (symbol-name (car cmd-type-config)))
                    :command ,(lambda () (interactive)
                                (funcall-interactively (caddr cmd-type-config) project cmd))
                    :annotation ,(when (stringp cmd) cmd))
                  result))))
      (nreverse result))))

;;;###autoload
(defun projection-multi-projection ()
  "`compile-multi' wrapper for only projection targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-projection-targets))))



(defun projection-multi--project-triggers (project)
  "Extract all `compile-multi' triggers for the PROJECT."
  (append
   (list #'projection-multi-projection-targets)
   (when-let ((project-types (projection-project-types (project-root project))))
     (cl-loop
      for config in project-types
      with targets = nil
      do (setq targets (ensure-list (oref config compile-multi-targets)))
      when targets
        append targets))))

(defmacro projection-multi--cache-command-helpers (target-functions &rest body)
  "Setup memoisation for each function in TARGET-FUNCTIONS.
Within the body of this macro every invocation of TARGET-FUNCTIONS is cached.
If a previous invocation with the same arguments is already in the cache it
will be returned directly. This function as a simple optimiser. All the
functions in TARGET-FUNCTIONS must be defined when BODY is invoked and the
result of each function should be deterministic."
  (declare (indent 2))
  `(let ((--projection-multi-helper-cache nil))
     (let* ((target-functions (quote (list ,@target-functions)))
            (loaded-target-functions
             (seq-filter #'symbol-function (quote (list ,@target-functions))))
            (missing-target-functions
             (seq-difference target-functions loaded-target-functions)))
       (cl-assert
        (not missing-target-functions)
        'show-args
        "Command helper caching requires all cached helpers to be loaded %s"
        missing-target-functions))
     (cl-letf*
         (,@(cl-loop for function in target-functions
                     with cached-function = nil
                     do (setq cached-function
                              (intern (concat "--project-multi-cached-"
                                              (symbol-name function))))
                     collect
                     `(,cached-function (symbol-function ',function))
                     collect
                     `((symbol-function ',function)
                       (lambda (&rest args)
                         (let ((key (append (list ',function) args)))
                           (if-let ((cached
                                     (assoc key --projection-multi-helper-cache #'equal)))
                               (cdr cached)
                             (let ((value (apply ,cached-function args)))
                               (push (cons key value) --projection-multi-helper-cache)
                               value)))))))
       ,@body)))

(cl-defun projection-multi-compile--run (project triggers)
  "Run `compile-multi' TRIGGERS for PROJECT."
  (let* (;; Run compilations and generators from the project root.
         (default-directory (or (when project
                                  (project-root project))
                                default-directory))
         (compile-multi-default-directory #'ignore)
         (compile-multi-config triggers))
    ;; KLUDGE: We can't cache any functions until they've been loaded.
    (require 'projection-utils-cmake)
    (require 'projection-utils-meson)
    (projection-multi--cache-command-helpers
        (project-current
         projection-cmake--preset
         projection-cmake--build-directory
         projection-meson--build-directory
         projection-cmake-ctest--jobs)
      (call-interactively #'compile-multi))))

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

(provide 'projection-multi)
;;; projection-multi.el ends here
