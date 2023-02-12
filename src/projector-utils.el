;;; projector-utils.el --- Helper module for code shared between other projector modules. -*- lexical-binding: t; -*-

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

(defun projector--command-or-shell (func shell-command)
  "Generate a command function which will run either FUNC or SHELL-COMMAND.
The result is a lambda which, if FUNC is bound and interactive returns FUNC,
otherwise it will return SHELL-COMMAND."
  (lambda ()
    (if (commandp func)
        func
      shell-command)))

(defun projector--join-shell-command (argv)
  "Join quoted arguments from ARGV into a shell command."
  (string-join (mapcar #'shell-quote-argument argv) " "))

(defun projector--all-files-exists (&rest files)
  "Generate a predicate function which is true if all files in FILES exist."
  (apply-partially #'cl-every #'file-exists-p files))



;; CMake

(defcustom projector-cmake-build-directory "build"
  "Build directory for cmake project builds."
  :type 'string
  :group 'projector)

(defcustom projector-cmake-configure-options nil
  "Default CMake options when configured with projector.
Place any -D options or extra flags you always want to use (for example
-DCMAKE_EXPORT_COMPILE_COMMANDS) in this option variable."
  :type '(list (repeat string))
  :group 'projector)

;; TODO: Support [[https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html][cmake-presets]].

(defun projector--cmake-command (&optional target)
  "Generate a CMake command optionally to run TARGET."
  (projector--join-shell-command
   `("cmake"
     "--build" ,projector-cmake-build-directory
     ,@(when target (list "--target" target)))))

(provide 'projector-utils)
;;; projector-utils.el ends here
