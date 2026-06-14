;;; projection-type-maven.el --- Helpers for supporting Maven projects. -*- lexical-binding: t; -*-

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

;; Projection project-type helpers for Maven projects.

;;; Code:

(require 'f)
(require 'projection-utils)
(require 'projection-core-cache)
(require 'projection-core-log)

(defgroup projection-type-maven nil
  "Projection Maven project type."
  :group 'projection-types)

(defcustom projection-maven-extra-options '("--color=always")
  "Arguments always passed to the maven compilation commands."
  :type '(list (repeat (string :tag "Argument"))))

(defcustom projection-maven-fetch-sources-on-configure t
  "Whether to download sources when configuring a maven project."
  :type 'boolean)

(defcustom projection-maven-fetch-javadoc-on-configure nil
  "Whether to download javadoc when configuring a maven project."
  :type 'boolean)



;; Locally downloaded sources for `compilation-search-path'.

(defcustom projection-maven-cache-local-source-directories t
  "When true cache the list of locally cloned Java files with each project."
  :type '(boolean :tag "Always/Never cache presets")
  :group 'projection-type-java)

(defvar projection-local-cache-directory)
(cl-defsubst projection-maven--local-source-directory ()
  "Root directory in which `projection-maven' should leave extracted source files."
  (f-join
   (if-let* ((project (projection--current-project 'no-error)))
       (project-root project)
     default-directory)
   projection-local-cache-directory
   "maven"
   "extracted-sources"))

(projection--declare-cache-var
  'projection-maven-local-source-directories
  :title "Maven local source directories"
  :category "Maven"
  :description "Directories containing java files of maven dependencies"
  :hide t)

(defun projection-maven-local-source-directories ()
  "Query directories of extracted maven source files, respecting cache.
This is a best effort attempt to make external dependencies findable from
`compilation-mode'. It is not package aware so if the two different namespaces
have the same class name then this may jump to the wrong target."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-maven-local-source-directories
   projection-maven-cache-local-source-directories
   #'projection-maven--local-source-directories2))

(defun projection-maven--local-source-directories2 ()
  "Query directories of extracted maven source files."
  (thread-last
    (directory-files-recursively (projection-maven--local-source-directory)
                                 (rx ".java" eol))
    (mapcar #'file-name-directory)
    (projection--uniquify)))



;; Maven source download.

(defun projection-maven-resolve-and-extract-source-files ()
  "Resolve, download, and extract all dependencies of the currrent project.
Artifacts will be dumped to a subdirectory of
`projection-local-cache-directory'. When invoked this function will clear any
cached value for `projection-maven-local-source-directories'."
  (unless (executable-find "unzip")
    (user-error "Extracting java source files requires unzip to be installed"))
  (let ((output-file (make-temp-file "projection-maven")))
    (unwind-protect
        (progn
          (message "Resolving maven sources, this may take a while.")
          (projection--log :info "Resolving maven sources")

          (when (projection-maven--call-resolve-sources output-file)
            (with-temp-buffer
              (insert-file-contents output-file)
              (if (search-forward-regexp (rx "The following files have been resolved:") nil 'noerror)
                  (while-let ((resolved-source (projection-maven--next-resolved-source-line)))
                    (cl-destructuring-bind (group-id artifact-id artifact-format
                                                     artifact-type version location)
                        resolved-source
                      (when (and (string-equal artifact-format "jar")
                                 (string-equal artifact-type "sources"))
                        (message "Extracting %s" location)
                        (let ((dest-dir (projection-maven--resolved-source-local-source-directory
                                         group-id artifact-id version)))
                          (make-directory dest-dir 'parents)
                          (projection--shell-command-to-string
                           (projection--join-shell-command
                            `("unzip" "-d" ,dest-dir "-o" ,location)))))))
                (error "Unable to find start of resolved file list in output: %s" (buffer-substring (point-min) (point-max)))))))
      (delete-file output-file)
      (projection-cache-clear-single
       (projection--current-project 'no-error)
       'projection-maven-local-source-directories
       projection--project-cache))))

(defun projection-maven--call-resolve-sources (output-file)
  "Call mvn dependency:resolve-sources and return non-nil on success.
maven will write output to OUTPUT-FILE."
  (projection--with-shell-command-buffer
    (projection--join-shell-command
     `("mvn"
       "--color=never"
       "-B"
       "dependency:resolve-sources"
       "-DoutputAbsoluteArtifactFilename"
       ,(concat "-DoutputFile=" output-file)))
    (if (search-forward-regexp (rx "ERROR") nil 'noerror)
        (progn
          (projection--log :error "Failed to resolve maven sources")
          nil)
      t)))

(defun projection-maven--next-resolved-source-line ()
  "Move and parse the next line in a mvn dependency:resolve-sources output.
Returns nil when all lines have been parsed before this function call."
  (forward-line)
  (let* ((line (buffer-substring (line-beginning-position) (line-end-position)))
         (line (string-trim-left line))
         (line (substring line 0 (or (s-index-of " " line)
                                     (length line)))))
    (if (or (eobp) (string-empty-p line))
        nil
      (string-split line ":"))))

(defun projection-maven--resolved-source-local-source-directory (group-id artifact-id version)
  "Calculate a destination directory for extracting mvn source files.
GROUP-ID, ARTIFACT-ID and VERSION are properties a parsed source line."
  (f-join
   (projection-maven--local-source-directory)
   group-id
   artifact-id
   version))



;; Maven compilation commands.

(defun projection-maven-run-configure ()
  "Configure command generator for Maven projects."
  (projection--join-shell-command
   `("mvn"
     ,@projection-maven-extra-options
     "-B"
     ,@(when projection-maven-fetch-sources-on-configure
         (list "dependency:resolve-sources"))
     ,@(when projection-maven-fetch-javadoc-on-configure
         (list "dependency:resolve"
               "-Dclassifier=javadoc")))))

(defun projection-maven-run-build ()
  "Build command generator for Maven projects."
  (projection--join-shell-command
   `("mvn"
     ,@projection-maven-extra-options
     "-B"
     "package"
     "-Dmaven.test.skip")))

(defun projection-maven-run-test ()
  "Test command generator for Maven projects."
  (projection--join-shell-command
   `("mvn"
     ,@projection-maven-extra-options
     "-B"
     "test")))

(provide 'projection-type-maven)
;;; projection-type-maven.el ends here
