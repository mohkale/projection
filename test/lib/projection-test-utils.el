;;; projection-test-utils.el --- Utility functions for projection tests -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mohsin Kaleem

;;; Code:

(require 'projection-core-cache)



(defun +projection-clear-all-cache ()
  "Clear the defined cache for all projects."
  (dolist (cache-table (projection--cache-all-cache-tables))
    (when-let ((cache (and (boundp cache-table)
                           (symbol-value cache-table))))
      (clrhash cache))))



;; Project setup

(defun +projection-setup-project-tree (file-tree &optional base)
  "Test helper function to setup a project directory based on FILE-TREE.
FILE-TREE is a list of files in either BASE or `default-directory'. If any of
the entries in FILE-TREE is a list then the car of that list is a subdirectory
of BASE and the cdr is the FILE-TREE of that sub-directory."
  (or base (setq base default-directory))
  (cl-loop for file in file-tree
           when (stringp file)
             do (f-touch (f-join base file))
           else
             if (consp file)
               if (stringp (cdr file))
                 do (f-write (cdr file) 'utf-8 (f-join base (car file)))
               else
                 do (let ((sub (f-join base (car file))))
                      (mkdir sub)
                      (+projection-setup-project-tree (cdr file) sub))
               else
                 do (error "Unexpected argument type" file)))

(defun +projection-setup-project (file-tree)
  "Test helper to setup a project file-tree and then create a git project from it.
See `+projection-setup-project-tree' for a description of FILE-TREE."
  (+projection-setup-project-tree file-tree)
  (with-temp-buffer
    (let ((exit-code
           (save-excursion
             (call-process
              shell-file-name nil (current-buffer) nil shell-command-switch
              "git init && git add -A && git commit -m \"Initial commit\""))))
      ;; (display-warning :debug
      ;;                  (format "Output of git init is: %s"
      ;;                          (buffer-substring (point-min) (point-max))) )
      (expect exit-code :to-equal 0))))



;; Test helpers

(defun +fake-completing-read (&rest return-values)
  "Mock out completing-read with RETURN-VALUES for each call.
Use this like so:
    (spy-on #'completing-read :and-call-fake
            (apply #'+fake-completing-read call-list))
"
  (let ((count 0))
    (lambda (&rest _)
      (prog1 (nth (min (1- (length return-values)) count)
                  return-values)
        (setq count (1+ count))))))

(defun +completion-table-candidates (call-args)
  (let ((completion-table (cadr call-args)))
    (if (functionp completion-table)
        (funcall completion-table "" nil t)
      completion-table)))

(defmacro +with-completing-read-not-called (&rest body)
  "Run BODY with the assertion that `completing-read' was not called."
  (declare (indent defun))
  `(progn
    (spy-on #'completing-read)
    ,@body
    (expect 'completing-read :to-have-been-called-times 0)))

(defun +expect-interactive-command-calls-compile-with (command shell-command)
  "Call COMMAND and assert `compile' was called with SHELL-COMMAND."
  (spy-on #'compile)

  (call-interactively command)

  (expect 'compile :to-have-been-called-times 1)
  (expect 'compile :to-have-been-called-with shell-command))

(defun +projection-project-matches-p (type)
  (let* ((project (projection--current-project))
         (project-types (projection-project-types (project-root project)))
         (project-type-names (mapcar #'projection-type--name project-types)))
    (expect project-type-names :to-contain type)))

;; Projection state modifiers

(defun +interactively-set-cmake-preset (build-type preset)
  (spy-on #'completing-read :and-return-value (concat (symbol-name build-type) ":" preset))
  (call-interactively #'projection-cmake-set-preset)
  (expect 'completing-read :to-have-been-called-times 1))

(defun +interactively-set-cmake-build-type (build-type)
  (spy-on #'completing-read :and-return-value build-type)

  (call-interactively #'projection-cmake-set-build-type)

  (expect 'completing-read :to-have-been-called-times 1))

(defun +interactively-set-meson-build-type (build-type)
  (spy-on #'completing-read :and-return-value build-type)

  (call-interactively #'projection-meson-set-build-type)

  (expect 'completing-read :to-have-been-called-times 1))



(defmacro +projection--cache-set (variable value)
  (let ((original-value-var (intern (concat "--project-test-" (symbol-name variable)))))
    `(progn
       (before-each
         (setq ,original-value-var ,variable
               ,variable ,value))
       (after-each
         (setq ,variable ,original-value-var)))))

(defmacro +projection-test-setup ()
  `(progn
     ;; Create and start running each test in a temporary directory.
     (before-each
       (setq --projection-test-original-directory default-directory
             --projection-test-directory (make-temp-file "buttercup-test-" t)
             default-directory (concat --projection-test-directory "/")))
     (after-each
       (delete-directory --projection-test-directory t)
       (setq default-directory --projection-test-original-directory))

     ;; Make compile synchronously block.
     (before-each
       (spy-on #'compile :and-call-fake
               (lambda (command &rest _)
                 (projection--shell-command-to-string command))))

     ;; Ensure any cached variables are pruned after a test finishes.
     (after-each (+projection-clear-all-cache))

     ;; Ensure the following properties are in a clean state between tests.
     (+projection--cache-set projection-project-types projection-project-types)
     (+projection--cache-set projection-primary-project-type nil)))

(provide 'projection-test-utils)
;;; projection-test-utils.el ends here
