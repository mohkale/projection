;;; projection-customize-compilation.el --- Integration between `projection' and `compile' -*- lexical-binding: t; -*-

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

;; This file exposes utilities for integrating `projection' and `compile'.

;;; Code:

(require 'compile)
(require 'projection-core-type)
(require 'projection-core-match)

(defgroup projection-customize-compilation nil
  "Adapt `compilation-mode' to work better with projects."
  :group 'projection)

(defcustom projection-customize-compilation-stay-out-of nil
  "List of features to disable from `projection-customize-compilation-mode'."
  :type '(list
          (choice
           (const :tag "Compilation search path" compilation-search-path)
           (const :tag "Compilation error regexps" compilation-error-regexp-alist)))
  :group 'projection-customize-compilation)



;;;###autoload
(defun projection-customize-compilation-buffer-name-function (name-of-mode)
  "Sample project specific `compilation-buffer-name-function'.
To use set either `compilation-buffer-name-function' or
`project-compilation-buffer-name-function' to this function.

See `compilation-buffer-name-function' for a description of NAME-OF-MODE."
  (if-let* ((project (projection--current-project 'no-error)))
      (concat "*" (capitalize name-of-mode) ": "
              (file-name-nondirectory
               (string-remove-suffix
                "/" (project-root project)))
              "*")
    (let ((compilation-buffer-name-function))
      (funcall #'compilation-buffer-name name-of-mode nil nil))))



;; `compilation-search-path'

(defun projection-customize-compilation--project-aware-search-path ()
  "Construct a `compilation-search-path' including per-project paths."
  (append
   compilation-search-path
   (cl-loop
    for config in
    (when-let* ((project (projection--current-project 'no-error)))
      (projection-project-types (project-root project)))
    append (cl-loop
            for path in (ensure-list (oref config compilation-search-paths))
            if (stringp path)
            collect path
            else if (functionp path)
            collect (funcall path)
            else
            do (user-error "Invalid `compilation-search-paths' in project type: %S" config)))))



;; `compilation-error-regexp-alist'

(defun projection-customize-compilation-all-project-regexp-alist ()
  "Access all per-project `compilation-error-regexp-alist'.
This can be used to add symbol definitions to
`compilation-error-regexp-alist-alist'.

  (setq compilation-error-regexp-alist-alist
        (append (projection-customize-compilation-all-project-regexp-alist)
                compilation-error-regexp-alist-alist))

If set then `projection-customize-compilation-mode' will prefer the symbol
instead of the alist value in `compilation-error-regexp-alist'."
  (cl-loop
   for config in projection-project-types
   append (oref config compilation-error-regexp-alist)))

(defun projection-customize-compilation--project-aware-regexp-alist ()
  "Construct a `compilation-error-regexp-alist' including per-project patterns."
  (append
   (cl-loop
    for config in
    (when-let* ((project (projection--current-project 'no-error)))
      (projection-project-types (project-root project)))
    append
    (cl-loop
     for error-regexp in (oref config compilation-error-regexp-alist)
     if (alist-get (car error-regexp) compilation-error-regexp-alist-alist)
       collect (car error-regexp)
     else
       collect (cdr error-regexp)))
   compilation-error-regexp-alist))



;;;###autoload
(define-minor-mode projection-customize-compilation-mode
  "Minor mode to customize `compilation-mode' with `projection' features."
  :lighter projection-customize-compilation-lighter
  (cond
   (projection-customize-compilation-mode
    (unless (member 'compilation-search-path projection-customize-compilation-stay-out-of)
      (setq-local compilation-search-path (projection-customize-compilation--project-aware-search-path)))
    (unless (member 'compilation-error-regexp-alist projection-customize-compilation-stay-out-of)
      (setq-local compilation-error-regexp-alist (projection-customize-compilation--project-aware-regexp-alist))))
   (t
    (kill-local-variable 'compilation-search-path)
    (kill-local-variable 'compilation-error-regexp-alist))))

(provide 'projection-customize-compilation)
;;; projection-customize-compilation.el ends here
