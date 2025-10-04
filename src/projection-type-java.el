;;; projection-type-java.el --- Helpers for supporting Java projects. -*- lexical-binding: t; -*-

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

;; Projection project-type helpers for all projects building with Java.

;;; Code:

(require 'projection-core-match)
(require 'projection-utils)

(defgroup projection-type-java nil
  "Projection utils for Java project types."
  :group 'projection-types)

(defcustom projection-java-cache-directories t
  "When true cache the list of directories containing Java files with each project."
  :type '(boolean :tag "Always/Never cache presets")
  :group 'projection-type-java)

(projection--declare-cache-var
  'projection-java-directories
  :title "Java directories"
  :category "Java"
  :description "Directories containing java files"
  :hide t)

(defun projection-java-directories ()
  "Query directories in project containing java files respecting cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-java-directories
   projection-java-cache-directories
   #'projection--java-directories2))

(defun projection--java-directories2 ()
  "Query directories in project containing java files."
  (when-let* ((project (projection--current-project 'no-error)))
    (thread-last
      project
      (project-files)
      (seq-filter (apply-partially #'string-match-p (rx ".java" eol)))
      (mapcar #'file-name-directory)
      (projection--uniquify))))

(provide 'projection-type-java)
;;; projection-type-java.el ends here
