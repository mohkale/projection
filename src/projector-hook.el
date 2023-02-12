;;; projector-hook.el --- Hook functions directly into all buffers in a project. -*- lexical-binding: t; -*-

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

;; This file exposes helpers for hooking functions into entire projects.
;; This could be used, for example, to mark all files in a project as
;; read-only while exploring it.

;;; Code:

(require 'projector-core)

(defgroup projector-hook nil
  "Setup hooks for all files in a project."
  :group 'projector)

(defcustom projector-hook-functions
  (list #'read-only-mode)
  "List of possible functions that can be hooked into project buffers.
Each entry should either be a mode-like function (enables when called and
disables when called again) or a cons of a function to enable the feature
and another function to disable it."
  :type '(list
          (choice
           (cons function function)
           function))
  :group 'projector-hook)

(defcustom projector-hook-predicate
    (apply-partially #'symbol-value 'buffer-file-name)
  "Function used prior to enabling `projector-hook-mode' in the current buffer.
By default it ensures hooks are only enabled in file buffers (not just any
project buffer)."
  :type '(optional function)
  :group 'projector-hook)

(defcustom projector-hook-lighter nil
  "Mode line lighter for `projector-hook-mode'."
  :type 'sexp
  :group 'projector-hook)

(projector--define-cache projector-hook--cache
  "Cache of hooked functions per project.")



(defun projector-hook--execute-hook-list (msg-string hook-list silent)
  "Run all hooks in HOOK-LIST for the current project.
MSG-STRING should be a string explaining the kind of hooks in HOOK-LIST (ex:
enabling or disabling). HOOK-LIST should be a list of function symbols to call
one after the other. When SILENT is true this function will not produce any
output.

The return value of this function is the number of functions in hook-list that
failed."
  (let ((error-count 0)
        (orig-buffer-name (buffer-name)))
    (dolist (it hook-list)
      (condition-case-unless-debug err
          (funcall it)
        (error
         (with-current-buffer (get-buffer-create " *projector-hook*")
           (insert "%s :: error while %s hook %s in %s :: %S\n"
                   (current-time-string)
                   msg-string
                   it
                   orig-buffer-name
                   err))
         (setq error-count (1+ error-count)))))
    (when (and (not silent)
               (> error-count 0))
      (message "Error while %s projector-hooks in this buffer" msg-string))
    error-count))

(defun projector-hook--enable (&optional hook-list silent)
  "Enable all configured hooks for the current project.
HOOK-LIST if given will be enabled instead of the entries configured
for the current project. With SILENT no message will be printed if
there was an error enabling the hooks."
  (or hook-list
      (setq hook-list (projector--cache-get
                       (projector--current-project)
                       'hooks projector-hook--cache)))

  (projector-hook--execute-hook-list "enabling" hook-list silent))

(defun projector-hook--disable (&optional hook-list silent)
  "Disable all configured hooks for the current project.
See `projector-hook--enable' for a description of HOOK-LIST and SILENT."
  (when-let ((project (projector--current-project 'no-error)))
    (or hook-list
        (setq hook-list (projector--cache-get
                         project
                         'hooks projector-hook--cache)))

    ;; If there's a mapped disable function for each hook, use it instead
    ;; of the hook function again.
    (setq hook-list
          (cl-loop for hook in hook-list
                   with disable-func = nil
                   do (setq disable-func
                            (alist-get hook projector-hook-functions))
                   when disable-func
                     collect disable-func
                   else collect hook))

    (projector-hook--execute-hook-list "disabling" hook-list silent)))

;;;###autoload
(defun projector-hook (hook-entries &optional no-update)
  "Update the hooked functions for the current project.
HOOK-ENTRIES should be a list of entries from `projector-hook-functions'.
Unless NO-UPDATE is given the changes to the hook functions will be
retroactively applied to every buffer in the current project with
`projector-hook-mode' enabled."
  (interactive
   (list
    (mapcar
     #'intern
     ;; TODO: Auto enable any functions that are already enabled.
     (completing-read-multiple
      (projector--prompt "Project hook: " (projector--current-project))
      (lambda (str pred action)
        (if (eq action 'metadata)
            '(metadata (category . function))
          (complete-with-action action projector-hook-functions str pred)))
      nil t))
    current-prefix-arg))

  (let* ((project (projector--current-project))
         (existing-hook-entries
          (projector--cache-get project 'hooks projector-hook--cache))
         (buffers
          (seq-filter
           (apply-partially #'buffer-local-value 'projector-hook-mode)
           (if no-update
               (list (current-buffer))
             (project-buffers project)))))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (let ((error-count
               (+
                (or (when (not existing-hook-entries)
                      0)
                    (projector-hook--disable existing-hook-entries 'silent))
                (projector-hook--enable hook-entries 'silent))))
          (when (> error-count 0)
            (message "Encountered %d errors while processing new hook list"
                     error-count)))))
    (projector--cache-put project 'hooks hook-entries projector-hook--cache)))

(defun projector-hook-clear (&optional no-update)
  "Clear all project hooks enabled for the current project.
See `projector-hook' for a description of NO-UPDATE."
  (interactive "P")
  (projector-hook nil no-update))



(defun projector-hook--predicate (&optional skip-project-check)
  "Assert whether current buffer satisfies `projector-hook-predicate'.
With SKIP-PROJECT-CHECK do not ensure current buffer is within a project."
  (and (or (not projector-hook-predicate)
           (funcall projector-hook-predicate))
       (if skip-project-check
           t
         (projector--current-project 'no-error))))

;;;###autoload
(define-minor-mode projector-hook-mode
  "Minor mode to enable any functions hooked into the current project."
  :lighter projector-hook-lighter
  (cond
   ;; Disable when predicates invalidate the current buffer.
   ((and projector-hook-mode
         (not (projector-hook--predicate)))
    (projector-hook-mode -1))
   ;; Enable or disable hooks for current buffer in project.
   (projector-hook-mode
    (projector-hook--enable))
   (t
    (projector-hook--disable))))

(defun projector-hook--global-turn-on ()
  "Turn on `projector-hook-mode' if predicates pass."
  (when (and
         (not projector-hook-mode)
         (projector-hook--predicate 'skip-project-check))
    (projector-hook-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-projector-hook-mode
  projector-hook-mode
  projector-hook--global-turn-on
  :group 'projector-hook-mode)

(provide 'projector-hook)
;;; projector-hook.el ends here
