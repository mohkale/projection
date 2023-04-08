;;; projection-ibuffer.el --- Project integration for ibuffer. -*- lexical-binding: t; -*-

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

;; Expose an ibuffer filter that shows files and buffers in the current project
;; and a command to switch to an ibuffer exclusively for the current project.

(require 'projection-core)
(require 'ibuf-ext)

;;; Code:

(defgroup projection-ibuffer nil
  "Project `ibuffer' integration."
  :group 'projection)

(defcustom projection-ibuffer-globally-ignored-buffers
  `(,(rx "*scratch*")
    ,(rx "*lsp-log*"))
  "A list of `buffer-name' regexps ignored by `projection-ibuffer'."
  :group 'projection-ibuffer
  :type '(repeat string))

(defcustom projection-ibuffer-globally-ignored-modes
  '(erc-mode
    help-mode
    completion-list-mode
    Buffer-menu-mode
    occur-mode
    ;; GNUs modes
    gnus-article-edit-mode
    gnus-article-mode
    gnus-binary-mode
    gnus-bookmark-bmenu-mode
    gnus-browse-mode
    gnus-category-mode
    gnus-custom-mode
    gnus-dead-summary-mode
    gnus-dired-mode
    gnus-draft-mode
    gnus-edit-form-mode
    gnus-group-mode
    gnus-kill-file-mode
    gnus-mailing-list-mode
    gnus-message-citation-mode
    gnus-mode
    gnus-pick-mode
    gnus-server-mode
    gnus-sticky-article-mode
    gnus-summary-mode
    gnus-topic-mode
    gnus-tree-mode
    gnus-undo-mode)
  "A list of major-mode symbols ignored by `projection-ibuffer'.

If a buffer is using a given major mode, projection will exclude it
from ibuffer buffers created by `projection-ibuffer'."
  :group 'projection-ibuffer
  :type '(repeat symbol))

(defcustom projection-ibuffer-prefix "Project: "
  "Default prefix string for generated filter groups.
Used by `projection-ibuffer-filter-group-name-function'."
  :group 'projection-ibuffer
  :type 'string)

(defun projection-ibuffer-filter-group-name-function (project)
  "Default function used to produce the name of a filter group for PROJECT."
  (concat projection-ibuffer-prefix (project-name project)))

(defcustom projection-ibuffer-filter-group-name-function
  #'projection-ibuffer-filter-group-name-function
  "Function used to produce the name of a project filter group.
This function is supplied the project object and should return a string."
  :group 'projection-ibuffer
  :type 'function)



(defun projection-ibuffer--ignored-buffer-p (buffer)
  "Whether to exclude BUFFER from `projection-ibuffer' based on customisations."
  (or
   ;; Buffer is one of the modes we explicitly exclude.
   (cl-some
    (apply-partially
     #'eq
     (buffer-local-value 'major-mode buffer))
    projection-ibuffer-globally-ignored-modes)
   ;; Buffer has one of the ignored buffer-file-names.
   (let ((buffer-name (buffer-name buffer)))
     (cl-some
      (lambda (name)
        (string-match-p name buffer-name))
      projection-ibuffer-globally-ignored-buffers))))

(defun projection-ibuffer--project-buffer-p (buffer &optional project)
  "Whether to include BUFFER in project ibuffer for PROJECT.
If PROJECT is unset then a project will automatically be determined for
BUFFER. This function returns the final project for buffer when true."
  (with-current-buffer buffer
    (let ((directory (if buffer-file-name
                         (file-name-directory buffer-file-name)
                       default-directory)))
      (and (not (string-prefix-p " " (buffer-name buffer)))
           (not (projection-ibuffer--ignored-buffer-p buffer))
           directory
           (when-let* ((project (or project
                                    (projection--current-project 'no-error)))
                       (project-root (file-truename (project-root project))))
             (and
              (string-equal (file-remote-p directory)
                            (file-remote-p project-root))
              (not (string-match-p "^http\\(s\\)?://" directory))
              (string-prefix-p project-root
                               (file-truename directory)
                               (eq system-type 'windows-nt))
              project))))))



;;;###autoload (autoload 'ibuffer-make-column-projection-name "projection-ibuffer.el")
(define-ibuffer-column projection-name
  (:name "Project")
  (when-let ((project
              (projection-ibuffer--project-buffer-p (current-buffer))))
    (project-name project)))

;;;###autoload (autoload 'ibuffer-filter-by-projection-files "projection-ibuffer.el")
(define-ibuffer-filter projection-root
    "Ibuffer filter to only show buffers in a given project QUALIFIER."
  (:reader (project-current 'prompt)
   :description nil)
  (projection-ibuffer--project-buffer-p buf qualifier))

;;;###autoload (autoload 'ibuffer-do-sort-by-projection-name "projection-ibuffer.el")
(define-ibuffer-sorter projection-name
  "Ibuffer sorter to sort the buffers by their project name."
  (:description "Project")
  (let ((project1 (projection-ibuffer--project-buffer-p (car a)))
        (project2 (projection-ibuffer--project-buffer-p (car b))))
    (if (and project1 project2)
        (string-lessp
         (project-name project1)
         (project-name project2))
      project1)))

;;;###autoload (autoload 'ibuffer-make-column-projection-relative-file "projection-ibuffer.el")
(define-ibuffer-column projection-relative-file
  (:name "Filename")
  (when-let ((file-name (ibuffer-buffer-file-name))
             (project (projection-ibuffer--project-buffer-p (current-buffer))))
    (if-let ((root (project-root project)))
        (file-relative-name buffer-file-name root)
      (abbreviate-file-name buffer-file-name))))



;; Ibuffer current project.

(defun projection-ibuffer--current-project (project)
  "Open an IBuffer window showing only buffers in PROJECT."
  (ibuffer nil (format "*%s Buffers*" (project-name project))
           (list (cons 'projection-files project))))

;;;###autoload
(defun ibuffer-projection-current-project (prompt-for-project)
  "Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PROMPT-FOR-PROJECT is supplied."
  (interactive "P")
  (let ((project (or (and prompt-for-project
                          (project-current nil (project-prompt-project-dir)))
                     (project-current 'prompt))))
    (projection-ibuffer--current-project project)))



;; Filter groups for all projects in ibuffer.

(defun projection-ibuffer--filter-group-p (group)
  "Assert whether the ibuffer filter GROUP is a project filter group."
  (cl-destructuring-bind (name . filters) group
    (and
     (eql (length filters) 1)
     (eq (caar filters) 'projection-root)
     (string-equal
      name
      (funcall projection-ibuffer-filter-group-name-function
               (cdar filters))))))

(defun projection-ibuffer--filter-groups ()
  "Determine filter groups for all projects with open buffers."
  (cl-loop
   for buffer in (buffer-list)
   with cache = (make-hash-table :test #'equal)
   with project = nil
   do (setq project
            (with-current-buffer buffer
              (projection--current-project 'no-error)))
   when (and project (not (gethash (project-root project) cache)))
     collect (cons (funcall projection-ibuffer-filter-group-name-function project)
                   `((projection-root . ,project)))
     and do (puthash (project-root project) t cache)))

;;;###autoload
(defun ibuffer-projection-set-filter-groups ()
  "Set ibuffer filter groups for all projects that have open buffers."
  (interactive)
  (setq ibuffer-filter-groups
        (append (projection-ibuffer--filter-groups)
                ibuffer-filter-groups))
  (when (called-interactively-p 'interactive)
    (call-interactively #'ibuffer)))

;;;###autoload
(defun ibuffer-projection-unset-filter-groups ()
  "Remove ibuffer filter groups set by `ibuffer-projection-set-filter-groups'.
Returns t if any such groups were removed otherwise returns nil."
  (interactive)
  (let ((removed nil))
    (setq ibuffer-filter-groups
          (cl-loop
           for group in ibuffer-filter-groups
           with is-projection-group = nil
           do (setq is-projection-group
                    (projection-ibuffer--filter-group-p group))
           if is-projection-group
             do (setq removed t)
           else
             collect group))

    (when (called-interactively-p 'interactive)
      (call-interactively #'ibuffer))

    removed))

;;;###autoload
(defun ibuffer-projection-toggle-filter-groups ()
  "Toggle the inclusion of project-filter-groups in ibuffer.
This calls `ibuffer-projection-set-filter-groups' or
`ibuffer-projection-unset-filter-groups' depending on the
current value of `ibuffer-filter-groups'."
  (interactive)
  (unless (ibuffer-projection-unset-filter-groups)
    (ibuffer-projection-set-filter-groups))

  (when (called-interactively-p 'interactive)
    (call-interactively #'ibuffer)))

(provide 'projection-ibuffer)
;;; projection-ibuffer.el ends here
