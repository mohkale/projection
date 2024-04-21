;; -*- lexical-binding: t -*-

(require 'f)

(require 'projection-find)
(require 'projection-types)
(require 'projection-core-type)

(require 'projection-test-utils)

(setq python-indent-guess-indent-offset-verbose nil)

(describe "Projection find other file"
  (+projection-test-setup)

  (before-each
    (setq projection-project-types
          (list projection-project-type-python-pip
                projection-project-type-cmake)))

  (describe "Extension match"
    (it "Can jump between related files in a project based on file extension"
      ;; GIVEN
      (+projection-setup-project
       '("Makefile"
         ("src"
          "foo.h"
          "foo.cpp"
          "bar.h"
          "bar.cpp")))

      (find-file "src/foo.h")
      (expect buffer-file-name :to-match "src/foo.h")

      ;; WHEN
      (projection-find-other-file)
      ;; THEN
      (expect buffer-file-name :to-match "src/foo.cpp")

      ;; WHEN
      (projection-find-other-file)
      ;; THEN
      (expect buffer-file-name :to-match "src/foo.h"))

    (it "Can jump between related files across directories"
      ;; GIVEN
      (+projection-setup-project
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
      (projection-find-other-file)
      ;; THEN
      (expect buffer-file-name :to-match "include/foo.h")

      ;; WHEN
      (projection-find-other-file)
      ;; THEN
      (expect buffer-file-name :to-match "src/foo.cpp"))

    (it "Can jump between files with identical names across directories"
      ;; GIVEN
      (+projection-setup-project
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
      (projection-find-other-file)
      ;; THEN
      (expect buffer-file-name :to-match "src2/foo.cpp")

      ;; WHEN
      (projection-find-other-file)
      (expect buffer-file-name :to-match "src/foo.cpp")))

  (describe "Test prefix suffix"
    (it "Can jump between test and implementation files"
      (+projection-setup-project
       '("requirements.txt"
         ("src"
          "foo.py"
          "bar.py")
         ("test"
          "test_foo.py"
          "foo_test.py")))
      (expect (projection-type--name (projection-project-type default-directory))
              :to-equal 'python-pip)

      (find-file "src/foo.py")
      (expect buffer-file-name :to-match "src/foo.py")

      ;; WHEN
      (projection-find-other-file)
      ;; THEN
      (expect buffer-file-name :to-match "test/test_foo.py")

      ;; WHEN
      (projection-find-other-file)
      ;; THEN
      (expect buffer-file-name :to-match "test/foo_test.py")

      ;; WHEN
      (projection-find-other-file)
      ;; THEN
      (expect buffer-file-name :to-match "src/foo.py"))
    )

  (it "Cannot jump between files not in a project"
    ;; GIVEN
    (+projection-setup-project
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

    ;; WHEN/THEN
    (let ((err (should-error (projection-find-other-file))))
      (expect (cadr err) :to-equal "No other files found"))))
