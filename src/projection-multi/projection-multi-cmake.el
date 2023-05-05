;;; projection-multi-cmake.el --- Projection integration for `compile-multi' and the CMake project type. -*- lexical-binding: t; -*-

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

;; This library exposes a target generation function for `compile-multi' which
;; sources the list of available targets from a CMake projects build config.
;;
;; This functionality is supported by parsing the set of available targets
;; from the output of the help target (this assumes the project has already
;; passed the configure stage). If invoked prior to this target resolution
;; will return no targets.

;;; Code:

(require 'projection-utils)
(require 'projection-core-log)
(require 'projection-utils-cmake)
(require 'projection-multi)

(defconst projection-multi-cmake--help-regex
  (rx
   bol
   (or
    (and
     (group-n 1 (minimal-match (one-or-more any)))
     ": " (one-or-more any))
    (and
     (one-or-more ".") " "
     (group-n 1 (minimal-match (one-or-more any)))
     (optional " (the default if no target is provided)")))
   eol)
  "Regexp to match targets from the CMake help output.")

(defun projection-multi-cmake--targets-from-command ()
  "Determine list of available CMake targets from the help target."
  (projection--log :debug "Resolving available CMake targets")

  (projection--with-shell-command-buffer (projection--cmake-command nil "help")
    (let (res)
      (save-match-data
        (while (re-search-forward projection-multi-cmake--help-regex nil 'noerror)
          (let ((target (match-string 1)))
            (push target res))))
      (nreverse res))))

;;;###autoload
(defun projection-multi-cmake-targets (&optional project-type)
  "`compile-multi' target generator function for CMake projects.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "cmake"))

  (cl-loop
   for target in (projection-multi-cmake--targets-from-command)
   collect (cons (concat project-type ":" target)
                 (projection--cmake-command nil target))))

;;;###autoload
(defun projection-multi-compile-cmake ()
  "`compile-multi' wrapper for only CMake targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-cmake-targets))))

(provide 'projection-multi-cmake)
;;; projection-multi-cmake.el ends here
