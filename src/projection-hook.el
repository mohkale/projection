;;; projection-hook.el --- Hook functions directly into all buffers in a project. -*- lexical-binding: t; -*-

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

(require 'projection-core)

(defgroup projection-hook nil
  "Setup hooks for all files in a project."
  :group 'projection)

(defcustom projection-hook-functions
  (list #'read-only-mode)
  "List of possible functions that can be hooked into project buffers.
Each entry should either be a mode-like function (enables when called and
disables when called again) or a cons of a function to enable the feature
and another function to disable it."
  :type '(list
          (choice
           (cons function function)
           function))
  :group 'projection-hook)

(defcustom projection-hook-predicate
    (apply-partially #'symbol-value 'buffer-file-name)
  "Function used prior to enabling `projection-hook-mode' in the current buffer.
By default it ensures hooks are only enabled in file buffers (not just any
project buffer)."
  :type '(optional function)
  :group 'projection-hook)

(defcustom projection-hook-lighter nil
  "Mode line lighter for `projection-hook-mode'."
  :type 'sexp
  :group 'projection-hook)

(projection--define-cache projection-hook--cache
  "Cache of hooked functions per project.")



(defun projection-hook--execute-hook-list (msg-string hook-list silent)
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
         (with-current-buffer (get-buffer-create " *projection-hook*")
           (insert "%s :: error while %s hook %s in %s :: %S\n"
                   (current-time-string)
                   msg-string
                   it
                   orig-buffer-name
                   err))
         (setq error-count (1+ error-count)))))
    (when (and (not silent)
               (> error-count 0))
      (message "Error while %s projection-hooks in this buffer" msg-string))
    error-count))

(defun projection-hook--enable (&optional hook-list silent)
  "Enable all configured hooks for the current project.
HOOK-LIST if given will be enabled instead of the entries configured
for the current project. With SILENT no message will be printed if
there was an error enabling the hooks."
  (or hook-list
      (setq hook-list (projection--cache-get
                       (projection--current-project)
                       'hooks projection-hook--cache)))

  (projection-hook--execute-hook-list "enabling" hook-list silent))

(defun projection-hook--disable (&optional hook-list silent)
  "Disable all configured hooks for the current project.
See `projection-hook--enable' for a description of HOOK-LIST and SILENT."
  (when-let ((project (projection--current-project 'no-error)))
    (or hook-list
        (setq hook-list (projection--cache-get
                         project
                         'hooks projection-hook--cache)))

    ;; If there's a mapped disable function for each hook, use it instead
    ;; of the hook function again.
    (setq hook-list
          (cl-loop for hook in hook-list
                   with disable-func = nil
                   do (setq disable-func
                            (alist-get hook projection-hook-functions))
                   when disable-func
                     collect disable-func
                   else collect hook))

    (projection-hook--execute-hook-list "disabling" hook-list silent)))

;;;###autoload
(defun projection-hook (hook-entries &optional no-update)
  "Update the hooked functions for the current project.
HOOK-ENTRIES should be a list of entries from `projection-hook-functions'.
Unless NO-UPDATE is given the changes to the hook functions will be
retroactively applied to every buffer in the current project with
`projection-hook-mode' enabled."
  (interactive
   (list
    (mapcar
     #'intern
     ;; TODO: Auto enable any functions that are already enabled.
     (completing-read-multiple
      (projection--prompt "Project hook: " (projection--current-project))
      (lambda (str pred action)
        (if (eq action 'metadata)
            '(metadata (category . function))
          (complete-with-action action projection-hook-functions str pred)))
      nil t))
    current-prefix-arg))

  (let* ((project (projection--current-project))
         (existing-hook-entries
          (projection--cache-get project 'hooks projection-hook--cache))
         (buffers
          (seq-filter
           (apply-partially #'buffer-local-value 'projection-hook-mode)
           (if no-update
               (list (current-buffer))
             (project-buffers project)))))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (let ((error-count
               (+
                (or (when (not existing-hook-entries)
                      0)
                    (projection-hook--disable existing-hook-entries 'silent))
                (projection-hook--enable hook-entries 'silent))))
          (when (> error-count 0)
            (message "Encountered %d errors while processing new hook list"
                     error-count)))))
    (projection--cache-put project 'hooks hook-entries projection-hook--cache)))

;;;###autoload
(defun projection-hook-clear (&optional no-update)
  "Clear all project hooks enabled for the current project.
See `projection-hook' for a description of NO-UPDATE."
  (interactive "P")
  (projection-hook nil no-update))



(defun projection-hook--predicate (&optional skip-project-check)
  "Assert whether current buffer satisfies `projection-hook-predicate'.
With SKIP-PROJECT-CHECK do not ensure current buffer is within a project."
  (and (or (not projection-hook-predicate)
           (funcall projection-hook-predicate))
       (if skip-project-check
           t
         (projection--current-project 'no-error))))

;;;###autoload
(define-minor-mode projection-hook-mode
  "Minor mode to enable any functions hooked into the current project."
  :lighter projection-hook-lighter
  (cond
   ;; Disable when predicates invalidate the current buffer.
   ((and projection-hook-mode
         (not (projection-hook--predicate)))
    (projection-hook-mode -1))
   ;; Enable or disable hooks for current buffer in project.
   (projection-hook-mode
    (projection-hook--enable))
   (t
    (projection-hook--disable))))

(defun projection-hook--global-turn-on ()
  "Turn on `projection-hook-mode' if predicates pass."
  (when (and
         (not projection-hook-mode)
         (projection-hook--predicate 'skip-project-check))
    (projection-hook-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-projection-hook-mode
  projection-hook-mode
  projection-hook--global-turn-on
  :group 'projection-hook-mode)

(provide 'projection-hook)
;;; projection-hook.el ends here
