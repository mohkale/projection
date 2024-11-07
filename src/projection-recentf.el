;;; projection-recentf.el --- Find recent files in the current project. -*- lexical-binding: t; -*-

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

;; Wrapper around `recentf' that focuses specifically on recent files for the
;; current project.

;;; Code:

(require 'projection-core)
(require 'recentf)

(defun projection-recentf--list (project)
  "Return the subset of `recentf-list' in the current PROJECT."
  ;; We use abbreviate-file-name instead of expand-file-name because all that's
  ;; wanted here is to respect tilde expansion. Emacs doesn't have an
  ;; `expand-file-name' variant that only expands users home directories, and
  ;; we don't want to follow symlinks or perform any other expansion steps.
  (let* ((abbreviate-file-name nil)
         (root (project-root project))
         (root-abbreviated (abbreviate-file-name root)))
    (ignore abbreviate-file-name)
    (mapcar
     (lambda (file) (file-relative-name file root))
     (seq-filter
      (lambda (file)
        (string-prefix-p root-abbreviated (abbreviate-file-name file)))
      recentf-list))))

(defun projection-recentf--read-file ()
  "Interactively read a recent-file from the current project.."
  (let ((project (projection--current-project)))
    (unless recentf-mode (recentf-mode 1))
    (if-let* ((recent-project-files (projection-recentf--list project)))
        (expand-file-name
         (completing-read
          (projection--prompt "Open recent file: " project)
          (projection-completion--completion-table
           :candidates recent-project-files
           :category 'project-file)
          nil t)
         (project-root project))
      (user-error "No recent-files found in the current project"))))

;;;###autoload
(defun projection-recentf (file)
  "Variant of `recentf-open' for project files.
Will call `recentf-open' with FILE."
  (interactive
   (list (projection-recentf--read-file)))
  (if (functionp 'recentf-open)
      (recentf-open file)
    (when file
      (funcall recentf-menu-action file))))

(provide 'projection-recentf)
;;; projection-recentf.el ends here
