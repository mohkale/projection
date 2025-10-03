;;; projection-type-gradle.el --- Helpers for supporting Gradle projects. -*- lexical-binding: t; -*-

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

;; Projection project-type helpers for Gradle projects.

;;; Code:

(require 'project)
(require 'f)
(require 'projection)
(require 'projection-core-misc)
(require 'projection-core-type)
(require 'projection-core-match)
(require 'projection-utils)

(defgroup projection-type-gradle nil
  "Projection Gradle project type."
  :group 'projection-types)

(defcustom projection-gradle-use-daemon t
  "Whether to enable gradle daemon support."
  :type 'boolean)

(defcustom projection-gradle-cache-java-directories t
  "When true cache the list of directories containing Java files with each project."
  :type '(boolean :tag "Always/Never cache presets")
  :group 'projection-type-gradle)

(projection--declare-cache-var
  'projection-gradle-java-directories
  :title "Gradle java directories"
  :category "Gradle"
  :description "Directories containing java files"
  :hide t)

(defun projection-gradle-java-directories ()
  "Query directories in project containing java files respecting cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-gradle-java-directories
   projection-gradle-cache-java-directories
   #'projection-gradle--java-directories2))

(defun projection-gradle--java-directories2 ()
  "Query directories in project containing java files."
  (when-let* ((project (projection--current-project 'no-error)))
    (thread-last
      project
      (project-files)
      (seq-filter (apply-partially #'string-match-p (rx ".java" eol)))
      (mapcar #'file-name-directory)
      (projection--uniquify))))

(defun projection-gradle--command (&rest args)
  "Generate a Gradle command with ARGS."
  (projection--join-shell-command
   `(,(if (file-exists-p "gradlew")
          "./gradlew"
        "gradle")
     ,@(when projection-gradle-use-daemon
         (list "--daemon"))
     ,@(when-let* ((job-count projection-build-jobs))
         (list "--parallel"))
     ,@args)))

;; Gradle compilation commands.

(defun projection-gradle-run-build ()
  "Build command generator for Gradle projects."
  ;; See https://stackoverflow.com/a/4714118
  (projection-gradle--command "build" "-x" "test"))

(defun projection-gradle-run-test ()
  "Test command generator for Gradle projects."
  (projection-gradle--command "test"))

(provide 'projection-type-gradle)
;;; projection-type-gradle.el ends here
