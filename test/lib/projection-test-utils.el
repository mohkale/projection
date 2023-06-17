;;; projection-test-utils.el --- Utility functions for projection tests -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mohsin Kaleem

;;; Code:

;; Project setup

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
               if (stringp (cdr file))
                 do (f-write (cdr file) 'utf-8 (f-join base (car file)))
               else
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

;; Test helpers

(defun fake-completing-read (&rest return-values)
  "Mock out completing-read with RETURN-VALUES for each call.
Use this like so:
    (spy-on #'completing-read :and-call-fake
            (apply #'fake-completing-read call-list))
"
  (let ((count 0))
    (lambda (&rest _)
      (prog1 (nth (min (1- (length return-values)) count)
                  return-values)
        (setq count (1+ count))))))

(defun get-completion-table-candidates (call-args)
  (let ((completion-table (cadr call-args)))
    (funcall completion-table "" nil t)))

(defmacro with-completing-read-not-called (&rest body)
  "Run BODY with the assertion that `completing-read' was not called."
  (declare (indent defun))
  `(progn
    (spy-on #'completing-read)
    ,@body
    (expect 'completing-read :to-have-been-called-times 0)))

(defun interactively-call-compile-command (command shell-command)
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

(defun interactively-set-cmake-preset (build-type preset)
  (let ((call-list
         (if build-type
             (list (symbol-name build-type) preset)
           (list preset))))
    (spy-on #'completing-read :and-call-fake
            (apply #'fake-completing-read call-list))

    (let ((current-prefix-arg (unless build-type
                                '(4))))
      (call-interactively #'projection-cmake-set-preset))

    (expect 'completing-read :to-have-been-called-times (length call-list))))

(defun interactively-set-cmake-build-type (build-type)
  (spy-on #'completing-read :and-return-value build-type)

  (call-interactively #'projection-cmake-set-build-type)

  (expect 'completing-read :to-have-been-called-times 1))

(provide 'projection-test-utils)
;;; projection-test-utils.el ends here
