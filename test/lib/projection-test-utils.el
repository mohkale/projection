;;; projection-test-utils.el --- Utility functions for projection tests -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mohsin Kaleem

;;; Code:

(defun projection-find-test--setup-project-tree (file-tree &optional base)
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
               do (let ((sub (f-join base (car file))))
                    (mkdir sub)
                    (projection-find-test--setup-project-tree (cdr file) sub))
             else
               do (error "Unexpected argument type" file)))

(defun projection-find-test--setup-project (file-tree)
  "Test helper to setup a project file-tree and then create a git project from it.
See `projection-find-test--setup-project-tree' for a description of FILE-TREE."
  (projection-find-test--setup-project-tree file-tree)
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

(provide 'projection-test-utils)
;;; projection-test-utils.el ends here
