;; -*- lexical-binding: t -*-

(require 'f)

(require 'projection-find)
(require 'projection-types)
(require 'projection-core-type)

(require 'projection-test-utils)

(setq python-indent-guess-indent-offset-verbose nil)

(defun +projection-find-related-files-alist (file-name)
  (save-excursion
    (expect (file-exists-p file-name) :to-be-truthy)
    (find-file file-name)
    (expect buffer-file-name :to-match file-name)

    (let* ((root (project-root (projection--current-project)))
           (start-file (buffer-file-name))
           (files (list (f-relative (buffer-file-name) root))))
      (projection-find-other-file)
      (while (not (string-equal (buffer-file-name) start-file))
        (push (f-relative (buffer-file-name) root) files)
        (projection-find-other-file))
      files)))

(describe "Projection find other file"
  :var (related-files)
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

      ;; WHEN
      (setq related-files (+projection-find-related-files-alist "src/foo.h"))

      ;; THEN
      (expect related-files :to-equal '("src/foo.cpp" "src/foo.h")))

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

      ;; WHEN
      (setq related-files (+projection-find-related-files-alist "src/foo.cpp"))

      ;; THEN
      (expect related-files :to-equal '("include/foo.h" "src/foo.cpp")))

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

      ;; WHEN
      (setq related-files (+projection-find-related-files-alist "src/foo.cpp"))

      ;; THEN
      (expect related-files :to-equal '("src2/foo.cpp" "src/foo.cpp"))))

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
      (+projection-project-matches-p 'python-pip)

      ;; WHEN
      (setq related-files (+projection-find-related-files-alist "src/foo.py"))

      ;; THEN
      (expect related-files :to-equal '("test/foo_test.py" "test/test_foo.py" "src/foo.py")))

    (it "Can jump between test and implementation files for all matching project types"
      (+projection-setup-project
       '("requirements.txt"
         "CMakeLists.txt"
         ("src"
          "foo.cpp"
          "foo.t.cpp"
          "foo.py"
          "bar.py")
         ("test"
          "foo.t.py"
          "test_foo.py"
          "test_foo.cpp")))
      (+projection-project-matches-p 'python-pip)
      (+projection-project-matches-p 'cmake)

      ;; WHEN
      (setq related-files (+projection-find-related-files-alist "src/foo.cpp"))
      ;; THEN
      (expect related-files :to-equal '("src/foo.t.cpp" "test/test_foo.cpp" "src/foo.cpp"))

      ;; WHEN
      (setq related-files (+projection-find-related-files-alist "src/foo.py"))
      ;; THEN
      (expect related-files :to-equal '("test/foo.t.py"
                                        "test/test_foo.py"
                                        "src/foo.py")))
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
