;;; projection-core-completion.el --- Completion helpers for `projection' -*- lexical-binding: t; -*-

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

;; Helpers for constructing an interacting with Emacs completion-tables.

;;; Code:

(require 'cl-lib)
(require 's)

(cl-defun projection-completion--annotation-function
    (&key (key-function #'identity))
  "Generate an annotation-function for `completing-read'.
KEY-FUNCTION will be used to query the annotation for each candidate."
  (let ((max-width (max 40 (/ (frame-width) 2))))
    (lambda (cand)
      (when-let* ((annotation (funcall key-function cand))
                  (annotation (s-truncate max-width annotation "…")))
        (concat (propertize
                 " " 'display
                 `(space :align-to (- right 1 ,(length annotation))))
                (propertize annotation 'face 'completions-annotations))))))

(cl-defun projection-completion--completion-table
    (&key candidates category annotation-function group-function
                cycle-sort-function display-sort-function)
  "Construct a completion-table for CANDIDATES.
CATEGORY, ANNOTATION-FUNCTION, GROUP-FUNCTION, CYCLE-SORT-FUNCTION, and
DISPLAY-SORT-FUNCTION are optional metadata for the completion-table."
  (let ((metadata
         `(metadata
           ,@(when category `((category . ,category)))
           ,@(when annotation-function `((annotation-function . ,annotation-function)))
           ,@(when group-function `((group-function . ,group-function)))
           ,@(when cycle-sort-function `((cycle-sort-function . ,cycle-sort-function)))
           ,@(when display-sort-function `((display-sort-function . ,display-sort-function))))))
    (lambda (string predicate action)
      (if (eq action 'metadata)
          metadata
        (complete-with-action action candidates string predicate)))))

(provide 'projection-core-completion)
;;; projection-core-completion.el ends here
