;;; projection-artifacts.el --- List artifacts produced by a project -*- lexical-binding: t; -*-

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

;; Functions for listing artifacts produced by a project. This could be physical
;; files such as executables or libraries or ephemeral artifacts like tests or
;; modules. A project can expose a series of artifacts by setting the :artifacts-list
;; property on the project type object. These functions when set should return a list
;; of alist entries each containing metadata for an artifact.

;;; Code:

(require 'project)
(require 'projection-core)
(require 'projection-utils)

(defconst projection-artifacts--default-completion-category "Project")

(defgroup projection-artifacts nil
  "Project artifact interface."
  :group 'projection)

(defcustom projection-artifacts-default-list-action
  #'projection-artifacts-show-and-copy-artifact
  "Default action to run with `projection-artifacts-list'."
  :type 'function)

(defun projection-artifacts--all-artifacts (project)
  "Query all artifacts for project types matching PROJECT."
  (when-let ((project-types (projection-project-types (project-root project))))
    (cl-loop
     for config in project-types
     with artifacts-generator = nil
     do (setq artifacts-generator (ensure-list (oref config artifacts-list)))
     append (apply #'append (mapcar #'funcall artifacts-generator)))))

(defun projection-artifacts--format-artifacts-for-completion (artifacts)
  "Convert ARTIFACTS into an alist for Emacs completion."
  (cl-loop
   for artifact in artifacts
   with name = nil
   do (setq name (alist-get 'name artifact))
   with category = nil
   do (setq category (or (alist-get 'category artifact)
                         projection-artifacts--default-completion-category))
   (setq name (concat category ": " name))
   unless name
     do (error "Encountered artifact with no name")
   collect (cons name artifact)))

(defun projection-artifacts--read (&optional prompt filter)
  "Interactively list and prompt for artifacts in the current project.
PROMPT is the prompt to use when asking the user. FILTER is a function used to
restrict the artifacts the user is presented with."
  (or prompt (setq prompt "Choose artifact: "))
  (or filter (setq filter (lambda (_) t)))
  (let ((project (projection--current-project)))
    (setq prompt (projection--prompt prompt project))
    (if-let ((artifacts
              (thread-last
                project
                (projection-artifacts--all-artifacts)
                (projection-artifacts--format-artifacts-for-completion)
                (seq-filter filter))))
        (let* ((completion-table
                (projection-completion--completion-table
                 :candidates artifacts
                 :group-function
                 (lambda (cand transform)
                   (when-let ((artifact (alist-get cand artifacts nil nil #'string-equal)))
                     (let ((category (or (alist-get 'category artifact)
                                         projection-artifacts--default-completion-category)))
                       (setq cand (substring cand (+ 2 (length category))))
                       (if transform cand category))))))
               (chosen-artifact (completing-read prompt completion-table nil 'require-match)))
          (or (alist-get chosen-artifact artifacts nil nil #'string-equal)
              (user-error "Chose unsupported artifact")))
      (user-error (projection--prompt "Failed to find any artifacts" project)))))



(cl-defgeneric projection-artifacts--serialise-artifact (artifact _type)
  "Helper function to serialise a artifact after presenting to the user.
ARTIFACT is the artifact alist."
  (format "%S" artifact))

(cl-defmethod projection-artifacts--serialise-artifact (artifact (_type (eql executable)))
  "Serialise an executable ARTIFACT into a command line."
  (let-alist artifact
    (projection--join-shell-command
     `(,@(projection--env-shell-command-prefix .environment .working-directory)
       ,.arg0
       ,@.argv))))

(cl-defmethod projection-artifacts--serialise-artifact (artifact (_type (eql library)))
  "Serialise a library ARTIFACT into the library path."
  (alist-get 'arg0 artifact))

(defun projection-artifacts-show-and-copy-artifact (artifact)
  "Default action for listing artifacts.
Will serialise ARTIFACT into a string. Copy the result to the clipboard and
message it."
  (let* ((type (alist-get 'type artifact))
         (string (projection-artifacts--serialise-artifact artifact type)))
    (message "%s" string)
    (kill-new string)))



;;;###autoload
(defun projection-artifacts-read-debug-targets (&optional prompt)
  "Helper function to read artifacts that are debuggable.
Optional PROMPT is passed to `projection-artifacts--read'."
  (projection-artifacts--read
   prompt
   (lambda (artifact)
     (alist-get 'debuggable artifact))))

;;;###autoload
(defun projection-artifacts-list (artifact)
  "Interactive function to select artifacts.
Calls `projection-artifacts-default-list-action' on ARTIFACT."
  (interactive
   (list (projection-artifacts--read)))
  (funcall projection-artifacts-default-list-action artifact))

(provide 'projection-artifacts)
;;; projection-artifacts.el ends here
