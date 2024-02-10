;;; projection-type-meson.el --- Helpers for supporting Meson projects. -*- lexical-binding: t; -*-

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

;; Projection project-type helpers for Meson projects.

;;; Code:

(require 'json)

(require 'projection-core)
(require 'projection-utils)

(defgroup projection-type-meson nil
  "Projection Meson project type."
  :group 'projection-types)

(defcustom projection-meson-build-directory "builddir"
  "Build directory for meson project builds."
  :type 'string
  :group 'projection-type-meson)

(defcustom projection-meson-install-directory "install"
  "Install directory for meson project installations."
  :type 'string
  :group 'projection-type-meson)

(defcustom projection-meson-build-directory-remote t
  "Assert whether build directory is on the same host as the project.
This option only has significance when `projection-meson-build-directory' is
absolute. It has the same purpose and usage as
`projection-cmake-build-directory-remote'."
  :type '(choice
          (const :tag "Reuse the remote component of the project" t)
          (string :tag "Specify the remote component directly")
          (const :tag "Do not do remote matching, the build area will always be local" nil))
  :group 'projection-type-meson)



(defun projection-meson--build-directory (&optional expand)
  "Fetch the path for the meson build directory.
When EXPAND return the absolute path to the build directory."
  (if expand
      (projection--expand-file-name-in-project
       projection-meson-build-directory
       projection-meson-build-directory-remote)
    projection-meson-build-directory))

;; See: https://github.com/mesonbuild/meson/blob/master/docs/markdown/IDE-integration.md
(defconst projection-meson--cache-file "meson-info/intro-projectinfo.json"
  "Path within build directory for a file regenerated on meson configuration.")

(defun projection--meson-configure-modtime-p (&rest _)
  "Helper function to return the time Meson was last configured."
  (projection--cache-modtime-predicate
   (expand-file-name
    projection-meson--cache-file
    (projection-meson--build-directory 'expand))))



;;;###autoload (autoload 'projection-meson-set-build-type "projection-type-meson" nil 'interactive)
(projection--declare-project-type-option 'build-type
  :project 'projection-meson
  :options '("plain" "debug" "debugoptimized" "release")
  :category "Meson"
  :title "Meson build type"
  :custom-group 'project-type-meson
  :custom-type '(choice (const :tag "Do not supply" nil)
                        (string :tag "Build type value"))
  :custom-docstring "Build type for a Meson project.")



(defun projection-meson--introspect (targets)
  "Run projection introspect and return the parsed output.
TARGETS are specific commands to pass to meson introspect and determines what
the Meson backend will return."
  (projection--with-shell-command-buffer
    (projection--join-shell-command
     `("meson" "introspect"
       ,(projection-meson--build-directory)
       "--force-object-output"
       ,@targets))
    (condition-case err
        (let ((json-array-type 'list)) (json-read))
      (json-readtable-error
       (projection--log :error "error while querying Meson targets %s." (cdr err))))))



(defcustom projection-meson-cache-code-model 'auto
  "When true cache the Meson code-model of each project."
  :type '(choice
          (const :tag "Cache model and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache code-model"))
  :group 'projection-meson)

(defun projection-meson--code-model ()
  "Query the current Meson projects code-model.
This function respects `projection-meson-cache-code-model'."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-meson-code-model
   (pcase projection-meson-cache-code-model
     ('auto #'projection--meson-configure-modtime-p)
     (_ projection-meson-cache-code-model))
   (apply-partially #'projection-meson--introspect '("--targets" "--tests"))))

(projection--declare-cache-var
  'projection-multi-meson-code-model
  :title "Meson code model"
  :category "Meson"
  :description "Meson build configuration"
  :hide t)



;; Meson project build options.

;; See https://mesonbuild.com/Configuring-a-build-directory.html
;; and https://mesonbuild.com/Build-options.html

(defun projection-meson--build-options ()
  "Query build options for the current Meson build directory."
  (thread-last
    (projection-meson--introspect '("--buildoptions"))
    (alist-get 'buildoptions)
    ;; Emacs doesn't have a nice interface for modifying collections of values
    ;; such as adding or removing from an array. For now we just skip over these.
    (seq-filter (lambda (props)
                  (not (equal (alist-get 'type props)
                              "array"))))))

(defvar projection-meson--build-option-history nil)

(defun projection-meson--build-option-choose (prompt build-options)
  "Choose a Meson build option from BUILD-OPTIONS interactively with PROMPT.
Build options should be a collection of build options queried from the Meson
backend."
  (let* ((build-option-alist
          (cl-loop for it in build-options collect (cons (alist-get 'name it) it)))
         (completion-table
          (projection-completion--completion-table
           :candidates build-option-alist
           :annotation-function
           (projection-completion--annotation-function
            :key-function (lambda (cand)
                            (thread-last
                              (assoc cand build-option-alist)
                              (cdr)
                              (alist-get 'description))))
           :group-function
           (lambda (cand transform)
             (if transform cand
               (if-let* ((props (alist-get cand build-option-alist nil nil #'string-equal))
                         (section (alist-get 'section props)))
                   (concat (capitalize section) " options")
                 "Unknown options")))))
         (build-option
          (completing-read
           prompt
           completion-table
           nil 'require-match nil
           'projection-meson--build-option-history)))
    (alist-get build-option build-option-alist nil nil #'string-equal)))

(defvar projection-meson--build-option-value-history nil)

(cl-defgeneric projection-meson--build-option-read (type prompt props)
  "Helper to read a new value for build-option PROPS of type TYPE with PROMPT."
  (ignore type prompt)
  (error "Cannot read build option with unknown type: %S" props))

(cl-defmethod projection-meson--build-option-read ((_type (eql combo)) prompt props)
  "Helper to read a new value for build-option PROPS of type combo with PROMPT."
  (completing-read
   prompt
   (alist-get 'choices props)
   nil 'require-match nil 'projection-meson--build-option-value-history
   (alist-get 'value props)))

(cl-defmethod projection-meson--build-option-read ((_type (eql boolean)) prompt _props)
  "Helper to read a new value for build-option PROPS of type bool with PROMPT."
  (if (yes-or-no-p prompt) "true" "false"))

(cl-defmethod projection-meson--build-option-read ((_type (eql integer)) prompt props)
  "Helper to read a new value for build-option PROPS of type integer with PROMPT."
  (number-to-string
   (read-number
    prompt
    (alist-get 'value props)
    'projection-meson--build-option-value-history)))

(cl-defmethod projection-meson--build-option-read ((_type (eql string)) prompt props)
  "Helper to read a new value for build-option PROPS of type string with PROMPT."
  (read-string
   prompt
   nil
   (alist-get 'value props)
   'projection-meson--build-option-value-history))

;;;###autoload
(defun projection-meson-set-build-option (project option value)
  "Interactively set a Meson build-option OPTION for PROJECT to VALUE."
  (interactive
   (let ((project (projection--current-project))
         (build-options (projection-meson--build-options))
         build-option option-value)
     (unless build-options
       (error "Failed to query any project options for %S" project))
     ;; (message "%S" build-options)
     (setq build-option (projection-meson--build-option-choose
                         (projection--prompt "Set build option: " project)
                         build-options)
           option-value (projection-meson--build-option-read
                         (intern (alist-get 'type build-option))
                         (projection--prompt "Set Meson build option `%s': "
                                             project (alist-get 'name build-option))
                         build-option))
     (list project (alist-get 'name build-option) option-value)))
  (projection--shell-command
   project
   (projection--join-shell-command
    `("meson"
      "configure"
      ,(projection-meson--build-directory)
      "-D" ,(concat option "=" value)))))



;; Meson compilation commands.

(defun projection-meson-get-configure-command ()
  "Generate a shell command to run a Meson configure."
  (projection--join-shell-command
   `("meson"
     "setup"
     ,(projection-meson--build-directory)
     ;; Meson requires this to allow reconfiguring but versions older than 2.0
     ;; will fail the first time configuration when this flag is supplied.
     ,@(when-let* ((build-directory (projection-meson--build-directory 'expand))
                   (build-directory-exists (file-exists-p build-directory)))
         (list "--reconfigure"))
     ,@(when-let ((build-type (projection-meson--build-type)))
         (list (concat "--buildtype=" build-type))))))

(defun projection-meson-get-build-command (&optional target)
  "Generate a shell command to run a Meson build optionally for TARGET."
  (projection--join-shell-command
   `("meson"
     "compile"
     "-C" ,(projection-meson--build-directory)
     ,@(when target (list target)))))

(defun projection-meson-get-test-command (&optional name)
  "Generate a shell command to run a Meson test.
When NAME is provided the test command will only run the test with NAME."
  (projection--join-shell-command
   `("meson"
     "test"
     "-C" ,(projection-meson--build-directory)
     ,@(when name (list "--" name)))))

(defun projection-meson-get-install-command ()
  "Generate a shell command to run a Meson installation."
  (projection--join-shell-command
   `("meson"
     "install"
     "-C" ,(projection-meson--build-directory)
     "--destdir" ,projection-meson-install-directory)))

(provide 'projection-type-meson)
;;; projection-type-meson.el ends here
