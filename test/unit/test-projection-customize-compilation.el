;; -*- lexical-binding: t -*-

(require 'projection-customize-compilation)
(require 'projection-test-utils)

(describe "Projection customize compilation"
  (+projection-test-setup)

  (before-each
    (setq foo-project (projection-type :name 'foo :predicate ".foo"))
    (push foo-project
          projection-project-types)
    (+projection-setup-project '(".foo" "foo" "bar" "baz")))

  (describe "Buffer name function"
    (it "Includes project name in name"
      ;; WHEN
      (let ((result
             (projection-customize-compilation-buffer-name-function "compilation")))
        ;; THEN
        (expect result :to-match (rx "*Compilation: buttercup-test" (+ any) "*"))))

    (it "Reverts to default project name when not in a project"
      ;; GIVEN
      (f-delete ".git" 'force)

      ;; WHEN
      (let ((result
             (projection-customize-compilation-buffer-name-function "compilation")))
        ;; THEN
        (expect result :to-match (rx "*compilation*"))))
    )

  (describe "Major mode"
    (before-each
      (find-file ".foo"))

    (it "Appends search path to `compilation-search-path'"
      ;; GIVEN
      (oset foo-project compilation-search-paths "build")

      ;; WHEN
      (projection-customize-compilation-mode)

      ;; THEN
      (expect compilation-search-path :to-contain "build"))

    (it "Appends result of search path function to `compilation-search-path'"
      ;; GIVEN
      (oset foo-project compilation-search-paths (lambda () "build"))

      ;; WHEN
      (projection-customize-compilation-mode)

      ;; THEN
      (expect compilation-search-path :to-contain "build"))

    (it "Appends multiple search paths to `compilation-search-path'"
      ;; GIVEN
      (oset foo-project compilation-search-paths
            (list "build1"
                  (lambda () "build2")))

      ;; WHEN
      (projection-customize-compilation-mode)

      ;; THEN
      (expect compilation-search-path
              :to-equal '(nil "build1" "build2")))

    (it "Appends error-regexp to `compilation-error-regexp-alist'"
      ;; GIVEN
      (oset foo-project compilation-error-regexp-alist
            `((bar . ("the-regexp" 1 2 3))))

      ;; WHEN
      (projection-customize-compilation-mode)

      ;; THEN
      (expect (car compilation-error-regexp-alist) :to-equal '("the-regexp" 1 2 3)))

    (it "Appends error-regexp symbol to `compilation-error-regexp-alist' when symbol is in `compilation-error-regexp-alist-alist'"
      ;; GIVEN
      (oset foo-project compilation-error-regexp-alist
            `((bar . ("the-regexp" 1 2 3))))
      (setq compilation-error-regexp-alist-alist
            (append (projection-customize-compilation-all-project-regexp-alist)
                    compilation-error-regexp-alist-alist))

      ;; WHEN
      (projection-customize-compilation-mode)

      ;; THEN
      (expect (car compilation-error-regexp-alist) :to-equal 'bar))
    )
  )
