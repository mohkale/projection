;;; projection-utils-meson.el --- Helpers for supporting Meson projects. -*- lexical-binding: t; -*-

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

(require 'projection-core-cache)
(require 'projection-core-misc)
(require 'projection-core-type)
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

(defun projection--meson-configure-modtime-p ()
  "Helper function to return the time Meson was last configured."
  (projection--cache-modtime-predicate
   (expand-file-name
    projection-meson--cache-file
    (projection-meson--build-directory 'expand))))



;;;###autoload (autoload 'projection-meson-set-build-type "projection-utils-meson" nil 'interactive)
(projection--declare-project-type-option 'build-type
  :project 'projection-meson
  :options '("plain" "debug" "debugoptimized" "release")
  :category "Meson"
  :title "Meson build type"
  :custom-group 'project-type-meson
  :custom-type '(choice (const :tag "Do not supply" nil)
                        (string :tag "Build type value"))
  :custom-docstring "Build type for a Meson project.")



;; Meson compilation commands.

(defun projection-meson-get-configure-command ()
  "Generate a shell command to run a Meson configure."
  (projection--join-shell-command
   `("meson"
     "setup"
     ,(projection-meson--build-directory)
     "--reconfigure"                                        ; Meson requires this to allow reconfiguring.
     ,@(when-let ((build-type (projection-meson--build-type)))
         (list (concat "--buildtype=" build-type))))))

(defun projection-meson-get-build-command (&optional target)
  "Generate a shell command to run a Meson build optionally for TARGET."
  (projection--join-shell-command
   `("meson"
     "compile"
     "-C" ,(projection-meson--build-directory)
     ,@(when target (list target)))))

(defun projection-meson-get-test-command ()
  "Generate a shell command to run a Meson test."
  (projection--join-shell-command
   `("meson"
     "test"
     "-C" ,(projection-meson--build-directory))))

(defun projection-meson-get-install-command ()
  "Generate a shell command to run a Meson installation."
  (projection--join-shell-command
   `("meson"
     "install"
     "-C" ,(projection-meson--build-directory)
     "--destdir" ,projection-meson-install-directory)))

(provide 'projection-utils-meson)
;;; projection-utils-meson.el ends here
