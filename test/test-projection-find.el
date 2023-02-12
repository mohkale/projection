;; -*- lexical-binding: t -*-

(require 'f)
(require 'projection-types)
(require 'projection-find)

(setq python-indent-guess-indent-offset-verbose nil)

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
  (expect
   (call-process-shell-command "git init && git add -A && git commit -m \"Initial commit\"")
   :to-equal 0))

(describe "Projection find other file"
  :var (original-directory
        test-directory)
  ;; Save the original directory before we run any tests.
  (before-all
    (setq original-directory default-directory))
  ;; For each test change to a temporary working directory.
  (before-each
    (cd (setq test-directory (make-temp-file "buttercup-test-" t)))
    (projection-reset-project-cache t))
  ;; And change back to the original directory and delete the test directory
  ;; after the test finishes.
  (after-each
    (when (file-equal-p default-directory test-directory)
      (cd default-directory)
      (delete-directory test-directory t)))

  (before-each
    ;; Note: buttercup doesn't process lexical vars correctly so modifications to
    ;; `projection-types' from other tests will impact the tests in this module. To
    ;; work around this I just copied the relevant project definitions from the
    ;; related file. See https://github.com/jorgenschaefer/emacs-buttercup/issues/127
    (setq projection-types nil)
    (projection-register-type 'python-pip
      :predicate "requirements.txt"
      :test-prefix "test_"
      :test-suffix "_test"))

  (it "Can jump between related files in a project based on file extension"
    ;; GIVEN
    ;;   * A Makefile project with a matching header and cpp file pair in the same
    ;;   directory.
    ;;   * My Emacs being open on src/foo.h.
    (projection-find-test--setup-project
     '("Makefile"
       ("src"
        "foo.h"
        "foo.cpp"
        "bar.h"
        "bar.cpp")))

    (find-file "src/foo.h")
    (expect buffer-file-name :to-match "src/foo.h")

    ;; WHEN
    ;;   I run `projection-find-other-file'.
    (projection-find-other-file)
    ;; THEN
    ;;   I have moved from the header file to the cpp file with the same
    ;;   base-name.
    (expect buffer-file-name :to-match "src/foo.cpp")

    ;; WHEN
    ;;   I run `projection-find-other-file'.
    (projection-find-other-file)
    ;; THEN
    ;;   I have moved from the cpp file back to the header file with the same
    ;;   base-name.
    (expect buffer-file-name :to-match "src/foo.h"))

  (it "Can jump between related files across directories"
    ;; GIVEN
    ;;   * A Makefile project with a matching header and cpp file pair in separate
    ;;   directories.
    ;;   * My Emacs being open on src/foo.cpp.
    (projection-find-test--setup-project
     '("Makefile"
       ("src"
        "foo.cpp"
        "bar.cpp")
       ("include"
        "foo.h"
        "bar.h")))

    (find-file "src/foo.cpp")
    (expect buffer-file-name :to-match "src/foo.cpp")

    ;; WHEN
    ;;   I run `projection-find-other-file'.
    (projection-find-other-file)
    ;; THEN
    ;;   I have moved from the cpp file to the header file with the same
    ;;   base-name in a different directory.
    (expect buffer-file-name :to-match "include/foo.h")

    ;; WHEN
    ;;   I run `projection-find-other-file'.
    (projection-find-other-file)
    ;; THEN
    ;;   I have moved from the header file back to the cpp file with the same
    ;;   base-name in the original directory.
    (expect buffer-file-name :to-match "src/foo.cpp"))

  (it "Can jump between files with identical names across directories"
    ;; GIVEN
    ;;   * A Makefile project with a cpp file having the same in 2 directories.
    ;;   * My Emacs being open on src/foo.cpp.
    (projection-find-test--setup-project
     '("Makefile"
       ("src"
        "foo.cpp"
        "bar.cpp")
       ("src2"
        "foo.cpp"
        "bar.cpp")))

    (find-file "src/foo.cpp")
    (expect buffer-file-name :to-match "src/foo.cpp")

    ;; WHEN
    ;;   I run `projection-find-other-file'.
    (projection-find-other-file)
    ;; THEN
    ;;   I have moved to the cpp file in the other directory with the same
    ;;   base-name.
    (expect buffer-file-name :to-match "src2/foo.cpp")

    ;; WHEN
    ;;   I run `projection-find-other-file'.
    (projection-find-other-file)
    ;;   I have moved back to the original cpp file.
    (expect buffer-file-name :to-match "src/foo.cpp"))

  (it "Can jump between test and implementation files"
    (projection-find-test--setup-project
     '("requirements.txt"
       ("src"
        "foo.py"
        "bar.py")
       ("test"
        "test_foo.py"
        "foo_test.py")))
    (expect (car (projection-project-type default-directory)) :to-equal 'python-pip)

    (find-file "src/foo.py")
    (expect buffer-file-name :to-match "src/foo.py")

    ;; WHEN
    ;;   I run `projection-find-other-file'.
    (projection-find-other-file)
    ;; THEN
    ;;   I have moved from the source file to a file with the test prefix.
    (expect buffer-file-name :to-match "test/test_foo.py")

    ;; WHEN
    ;;   I run `projection-find-other-file'.
    (projection-find-other-file)
    ;; THEN
    ;;   I have moved from another test file with the test suffix.
    (expect buffer-file-name :to-match "test/foo_test.py")

    ;; WHEN
    ;;   I run `projection-find-other-file'.
    (projection-find-other-file)
    ;; THEN
    ;;   I have returned to the original file.
    (expect buffer-file-name :to-match "src/foo.py")
    )

  (it "Cannot jump between files not in a project"
    ;; GIVEN
    ;;   * A project with a matching cpp header and implementation file but the header
    ;;   file is excluded from the project.
    ;;   * My Emacs being open on src/foo.cpp.
    (projection-find-test--setup-project
     '("Makefile"
       ("src"
        "foo.cpp"
        "bar.cpp")
       ("include"
        "bar.h")))
    (f-touch "include/foo.h")
    (f-write-text "include/foo.h" 'utf-8 ".gitignore")

    (find-file "src/foo.cpp")
    (expect buffer-file-name :to-match "src/foo.cpp")

    ;; WHEN
    ;;   I run `projection-find-other-file'.
    ;; THEN
    ;;   The command failed because there's no other files in the project.
    (let ((err (should-error (projection-find-other-file))))
      (expect (cadr err) :to-equal "No other files found"))))
