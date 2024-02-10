;;; projection-type-golang.el --- Helpers for supporting Golang projects. -*- lexical-binding: t; -*-

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

;; Projection project-type helpers for Golang projects.

;;; Code:

(require 'projection-core)
(require 'projection-utils)

(defgroup projection-type-golang nil
  "Projection Golang project type."
  :group 'projection-types)



;; Golang target package.

(defcustom projection-golang-package 'prompt-once-when-multiple
  "Active project that go commands should target.
This is used to infer the package argument passed to go tools like build or
test."
  :type '(choice
          (const :tag "Ask user to set package and then reuse chosen package" prompt-once)
          (const :tag "Ask user to set package and then cache only when there are multiple"
                 prompt-once-when-multiple)
          (const :tag "Always use ./..." all)
          (const :tag "Never supply an explicit package arg" nil)
          (string :tag "Explicit package path"))
  :group 'projection-type-golang)

(defun projection-golang--package ()
  "Fetch the target Golang package for this project."
  (let ((project (projection--current-project)))
    (or
     (projection--cache-get project 'projection-golang-package)
     (pcase projection-golang-package
       ('all "./...")
       ((pred stringp) projection-golang-package)
       ((pred null))
       ((or 'prompt-once 'prompt-once-when-multiple)
        (thread-last
          (if-let ((packages (projection-golang--list-packages)))
              (if (and (eq (length packages) 1)
                       (eq projection-golang-package
                           'prompt-once-when-multiple))
                  (cdar packages)
                (projection-golang--read-package project packages))
            ;; Treat projects you can't query the modules as of as 'all.
            "./...")
          (projection-golang-set-package project)))))))

;;;###autoload
(defun projection-golang-set-package (project package)
  "Set the target package for PROJECT to PACKAGE."
  (interactive
   (let ((project (projection--current-project)))
     (list project
           (projection-golang--read-package
            project
            (or (projection-golang--list-packages)
                (user-error "Failed to determine any packages in project"))))))
  (projection--cache-put project 'projection-golang-package package))

(projection--declare-cache-var
  'projection-golang-package
  :title "Golang target package"
  :category "Golang"
  :description "The package to build and test for in this project"
  :hide t)

(defun projection-golang--read-package (project packages)
  "Interactively read a golang package from PACKAGES for PROJECT."
  (setq packages (append '(("*All*" . "./...")) packages))

  (let* ((completion-table
          (projection-completion--completion-table
           :candidates packages
           :category 'projection-golang-packages
           :annotation-function
           (projection-completion--annotation-function
            :key-function (lambda (cand) (cdr (assoc cand packages))))
           :group-function
           (lambda (cand transform)
             (if transform cand
               (if (string-prefix-p "*" cand) "Builtin" "Package")))))
         (package
          (completing-read
           (projection--prompt "Set Golang package: " project)
           completion-table)))
    (or (alist-get package packages nil nil #'string-equal)
        package)))

(defun projection-golang--list-packages ()
  "List packages for the current project."
   (projection--with-shell-command-buffer "go list -f '{{.Root}};{{.Dir}};{{.ImportPath}}' ./..."
     (let ((result nil))
       (save-match-data
         (while (not (eobp))
           (let ((items (split-string (buffer-substring (line-beginning-position) (line-end-position))
                                      ";")))
             (push (cons (string-remove-prefix (concat (nth 0 items) "/") (nth 1 items))
                         (nth 2 items))
                   result))
           (forward-line)))
       (seq-sort-by #'car #'string< result))))



(defun projection-golang-list-artifacts ()
  "List go packages as artifacts."
  (cl-loop
   for (short-name . package) in (projection-golang--list-packages)
   collect `((name . ,short-name)
             (type . go-package)
             (category . "Go package")
             (go-package . ,package)
             (debuggable . t))))



;; Golang compilation commands.

(defun projection-golang-run-run ()
  "Run command generator for Golang projects."
  (projection--join-shell-command
   `("go"
     "run"
     ,@(when-let ((package (projection-golang--package)))
         (list package)))))

(defun projection-golang-run-build ()
  "Build command generator for Golang projects."
  (projection--join-shell-command
   `("go"
     "build"
     ,@(when-let ((package (projection-golang--package)))
         (list package)))))

(defun projection-golang-run-test ()
  "Test command generator for Golang projects."
  (projection--join-shell-command
   `("go"
     "test"
     ,@(when-let ((package (projection-golang--package)))
         (list package)))))

(provide 'projection-type-golang)
;;; projection-type-golang.el ends here
