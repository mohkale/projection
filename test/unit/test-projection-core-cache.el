;; -*- lexical-binding: t -*-

(require 'projection-core-cache)

(require 'projection-test-utils)

(describe "Projection cache"
  (+projection-test-setup)

  ;; Setup current directory as a project.
  (before-each
    (+projection-setup-project '(("README.txt" . ""))))
  (after-each (+projection-clear-all-cache))

  (it "Can cache and fetch from cache"
    ;; GIVEN
    (projection--cache-put (project-current) 'foo "foo")

    ;; WHEN
    (let ((value (projection--cache-get (project-current) 'foo)))
      (expect value :to-equal "foo")))

  (it "Cach commands can take a project-root path as a key"
    ;; GIVEN
    (let ((project-root (project-root (project-current))))
      (projection--cache-put project-root 'foo "foo")

      ;; WHEN
      (let ((value (projection--cache-get project-root 'foo)))
        (expect value :to-equal "foo"))))

  (it "Cannot get an entry that was not cached"
    (expect (projection--cache-get (project-current) 'foo) :to-be nil))

  (it "Can clear a cached variable"
    ;; GIVEN
    (projection--cache-put (project-current) 'foo "foo")

    ;; WHEN
    (projection--cache-remove (project-current) 'foo)

    ;; THEN
    (expect (projection--cache-get (project-current) 'foo) :to-be nil))

  (describe "With dynamic cache predicate"
    :var* ((get-foo-function (lambda () "foo"))
           (get-bar-function (lambda () "bar"))
           (static-predicate (lambda () 10)))

    (it "Sets the cache from the body function initially"
      ;; GIVEN
      (let ((project (project-current)))
        (expect (projection--cache-get project 'foo) :to-be nil)

        ;; WHEN
        (let ((value (projection--cache-get-with-predicate project 'foo 10 get-foo-function)))
          ;; THEN
          (expect (projection--cache-get project 'foo) :not :to-be nil)
          (expect value :to-equal "foo"))))

    (it "Reuses the cached value while the predicate is unchanged"
      ;; GIVEN
      (let ((project (project-current)))
        (projection--cache-get-with-predicate project 'foo 10 get-foo-function)

        ;; WHEN
        (let ((value (projection--cache-get-with-predicate project 'foo 10 get-bar-function)))
          ;; THEN
          ;;   Value is still foo, `get-bar-function' was not called.
          (expect value :to-equal "foo"))))

    (it "Invalidates and updates the cache if the predicate does change"
      ;; GIVEN
      (let ((project (project-current)))
        (projection--cache-get-with-predicate project 'foo 10 get-foo-function)

        ;; WHEN
        (let ((value (projection--cache-get-with-predicate project 'foo 20 get-bar-function)))
          ;; THEN
          (expect value :to-equal "bar"))

        ;; WHEN
        (let ((value (projection--cache-get-with-predicate project 'foo 20 get-foo-function)))
          ;; THEN
          ;;   Value is still bar, `get-foo-function' was not called.
          (expect value :to-equal "bar"))))

    (it "Accepts a boolean to mean always cache"
      ;; GIVEN
      (let ((project (project-current)))
        (projection--cache-get-with-predicate project 'foo t get-foo-function)

        ;; WHEN
        (let ((value (projection--cache-get-with-predicate project 'foo t get-bar-function)))
          ;; THEN
          (expect value :to-equal "foo")))))

  (describe "Modtime cache helper"
    :var (latest-modtime)
    ;; Create two files with guaranteed different modtimes.
    (before-each
      (f-touch ".foo")
      (f-touch ".bar")
      (while (eq (projection--cache-file-modtime ".foo")
                 (projection--cache-file-modtime ".bar"))
        (f-touch ".bar"))
      (setq latest-modtime (projection--cache-file-modtime ".bar")))

    (it "Fetches the latest modtime of a single supplied file"
      ;; WHEN/THEN
      (expect (projection--cache-modtime-predicate ".foo") :to-equal
              (projection--cache-file-modtime ".foo")))

    (it "Fetches the latest modtime of multiple files"
      ;; WHEN/THEN
      (expect (projection--cache-modtime-predicate ".foo" ".bar") :to-equal
              (projection--cache-file-modtime ".bar")))

    (it "Treats a missing file-path as always the newest"
      ;; WHEN/THEN
      ;;   We check comparing .foo and non-existent .baz returns a later time then .foo.
      (expect (projection--cache-modtime-predicate ".foo" ".baz") :to-be-weakly-greater-than
              latest-modtime)))

  (describe "With registered project cache variables"
    :var (original-projection--cache-var-alist)
    ;; Cache the existing registered cache vars.
    (before-all
      (setq original-projection--cache-var-alist projection--cache-var-alist
            projection--cache-var-alist nil))
    (after-all
      (setq projection--cache-var-alist original-projection--cache-var-alist))

    (before-all
      (projection--declare-cache-var 'foo :title "Foo")
      (projection--declare-cache-var 'bar :title "Bar")
      (projection--declare-cache-var 'hidden :title "Hidden" :hide t))

    (it "Can clear a cached project variable"
      ;; GIVEN
      (let ((project (project-current)))
        (projection--cache-put project 'foo "Value for Foo")
        (spy-on #'completing-read-multiple :and-return-value '("Foo"))

        ;; WHEN
        (call-interactively 'projection-cache-clear)

        ;; THEN
        (expect (projection--cache-get project 'foo) :to-be nil)
        (expect 'completing-read-multiple :to-have-been-called-times 1)
        (expect (+completion-table-candidates
                 (spy-calls-args-for 'completing-read-multiple 0))
                :to-equal '("Foo" "Bar"))))

    (it "Includes hidden variables when it has a value"
      ;; GIVEN
      (let ((project (project-current)))
        (projection--cache-put project 'hidden "Value for Hidden")
        (spy-on #'completing-read-multiple :and-return-value '("Hidden"))

        ;; WHEN
        (call-interactively 'projection-cache-clear)

        ;; THEN
        (expect (projection--cache-get project 'hidden) :to-be nil)
        (expect 'completing-read-multiple :to-have-been-called-times 1)
        (expect (+completion-table-candidates
                 (spy-calls-args-for 'completing-read-multiple 0))
                :to-equal '("Foo" "Bar" "Hidden"))))

    (it "Clears all cache variables when invoked with a prefix-arg"
      ;; GIVEN
      (let ((project (project-current)))
        (projection--cache-put project 'foo "Value for Foo")
        (projection--cache-put project 'hidden "Value for Hidden")
        (spy-on #'completing-read-multiple)

        ;; WHEN
        (+with-completing-read-not-called
         (let ((current-prefix-arg '(4)))
           (call-interactively 'projection-cache-clear)))

        ;; THEN
        (expect (projection--cache-get project 'foo) :to-be nil)
        (expect (projection--cache-get project 'hidden) :to-be nil)))))
