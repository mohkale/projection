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

(defun projection-ibuffer--project-buffer-p (buffer project)
  "Whether to include BUFFER in project ibuffer for PROJECT."
  (with-current-buffer buffer
    (let ((directory (if buffer-file-name
                         (file-name-directory buffer-file-name)
                       default-directory))
          (project-root (file-truename (project-root project))))
      (and (not (string-prefix-p " " (buffer-name buffer)))
           (not (projection-ibuffer--ignored-buffer-p buffer))
           directory
           (string-equal (file-remote-p directory)
                         (file-remote-p project-root))
           (not (string-match-p "^http\\(s\\)?://" directory))
           (string-prefix-p project-root
                            (file-truename directory)
                            (eq system-type 'windows-nt))))))

(define-ibuffer-filter projection-files
    "Show Ibuffer with all buffers in the current project."
  (:reader (project-current 'prompt)
   :description nil)
  (with-current-buffer buf
    (projection-ibuffer--project-buffer-p buf qualifier)))



(defun projection-ibuffer-by-project (project)
  "Open an IBuffer window showing all buffers in PROJECT."
  (ibuffer nil (format "*%s Buffers*" (project-name project))
           (list (cons 'projection-files project))))

;;;###autoload
(defun projection-ibuffer (prompt-for-project)
  "Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PROMPT-FOR-PROJECT is supplied."
  (interactive "P")
  (let ((project (or (and prompt-for-project
                          (project-current nil (project-prompt-project-dir)))
                     (project-current 'prompt))))
    (projection-ibuffer-by-project project)))

;; TODO: Support some variant of `ibuffer-projectile-mode'.

(provide 'projection-ibuffer)
;;; projection-ibuffer.el ends here
