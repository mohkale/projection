;;; projection-utils.el --- Helper module for code shared between other projection modules. -*- lexical-binding: t; -*-

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

(require 'cl-extra)



;; General

(defun projection--command-or-shell (func shell-command)
  "Generate a command function which will run either FUNC or SHELL-COMMAND.
The result is a lambda which, if FUNC is bound and interactive returns FUNC,
otherwise it will return SHELL-COMMAND."
  (lambda ()
    (if (commandp func)
        func
      shell-command)))

(defun projection--join-shell-command (argv)
  "Join quoted arguments from ARGV into a shell command."
  (string-join (mapcar #'shell-quote-argument argv) " "))

(defun projection--all-files-exists (&rest files)
  "Generate a predicate function which is true if all files in FILES exist."
  (apply-partially #'cl-every #'file-exists-p files))



;; CMake

(defcustom projection-cmake-build-directory "build"
  "Build directory for cmake project builds."
  :type 'string
  :group 'projection)

(defcustom projection-cmake-configure-options nil
  "Default CMake options when configured with projection.
Place any -D options or extra flags you always want to use (for example
-DCMAKE_EXPORT_COMPILE_COMMANDS) in this option variable."
  :type '(list (repeat string))
  :group 'projection)

;; TODO: Support [[https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html][cmake-presets]].

(defun projection--cmake-command (&optional target)
  "Generate a CMake command optionally to run TARGET."
  (projection--join-shell-command
   `("cmake"
     "--build" ,projection-cmake-build-directory
     ,@(when target (list "--target" target)))))

(provide 'projection-utils)
;;; projection-utils.el ends here
