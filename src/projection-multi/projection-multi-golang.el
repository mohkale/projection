;;; projection-multi-golang.el --- Projection integration for `compile-multi' and the Golang project type. -*- lexical-binding: t; -*-

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

;; This library exposes a target generation function for `compile-multi' allows
;; building, running and testing go packages.

;;; Code:

(require 'projection-multi)
(require 'projection-types)
(require 'projection-type-golang)

;;;###autoload
(defun projection-multi-golang-targets (&optional project-type)
  "`compile-multi' target generator function for Golang projects.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "golang"))

  (cl-loop
   for (short-name . package) in (projection-golang--list-packages)
   collect (cons (concat project-type ":" short-name)
                 (projection-golang--command 'run package))))

;;;###autoload
(defun projection-multi-compile-golang ()
  "`compile-multi' wrapper for only golang targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-golang-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (projection-type-append-compile-multi-targets projection-project-type-golang
    #'projection-multi-golang-targets))

(provide 'projection-multi-golang)
;;; projection-multi-golang.el ends here
