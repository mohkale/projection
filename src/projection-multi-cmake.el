;;; projection-multi-cmake.el --- projection integration for `compile-multi' and the CMake project type. -*- lexical-binding: t; -*-

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

(require 'projection-utils)

(defconst compile-multi-cmake--help-regex
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

(autoload 'projection--cmake-command "projection-types-cmake.el")

(defun projection-multi-cmake-targets (&optional project-type)
  "`compile-multi' target generator function for CMake projects.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "cmake"))

  (with-temp-buffer
    (insert
     (projection--shell-command-to-string
      (projection--cmake-command nil "help")))
    (goto-char (point-min))

    (let (res)
      (save-match-data
        (while (re-search-forward compile-multi-cmake--help-regex nil 'noerror)
          (let ((target (match-string 1)))
            (push (cons (concat project-type ":" target)
                        (projection--cmake-command nil target))
                  res))))
      (nreverse res))))

(provide 'projection-multi-cmake)
;;; projection-multi-cmake.el ends here
