;;; projection-types-cmake.el --- Projection project type definition for CMake. -*- lexical-binding: t; -*-

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

(require 'projection-core)
(require 'projection-utils)
(require 'projection-multi-cmake)

(defcustom projection-cmake-build-directory "build"
  "Build directory for cmake project builds."
  :type 'string
  :group 'projection-types)

(defcustom projection-cmake-configure-options nil
  "Default CMake options when configured with projection.
Place any -D options or extra flags you always want to use (for example
-DCMAKE_EXPORT_COMPILE_COMMANDS) in this option variable."
  :type '(list (repeat string))
  :group 'projection-types)

;; TODO: Support [[https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html][cmake-presets]].

(defun projection--cmake-command (&optional target)
  "Generate a CMake command optionally to run TARGET."
  (projection--join-shell-command
   `("cmake"
     "--build" ,projection-cmake-build-directory
     ,@(when target (list "--target" target)))))

(projection-register-type 'cmake
  :predicate "CMakeLists.txt"
  ;; The configure step takes the source directory and the output build
  ;; directory.
  :configure (defun projection-cmake-run--configure ()
               (projection--join-shell-command
                `("cmake"
                  "-S" "."
                  "-B" ,projection-cmake-build-directory
                  ,@projection-cmake-configure-options)))
  ;; The remaining commands take the build directory and an optional target
  ;; with it.
  :build   (defun projection-cmake-run--build   () (projection--cmake-command))
  :test    (defun projection-cmake-run--test    () (projection--cmake-command "ctest"))
  :install (defun projection-cmake-run--install () (projection--cmake-command "install"))
  :package (defun projection-cmake-run--package () (projection--cmake-command "package"))
  :targets #'projection-multi-cmake-targets)

(provide 'projection-types-cmake)
;;; projection-types-cmake.el ends here
