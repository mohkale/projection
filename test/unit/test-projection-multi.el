;; -*- lexical-binding: t -*-

(require 'projection-multi)
(require 'projection-multi-embark)
(require 'projection-test-utils)

(describe "Multi compile"
  :var (projection-project-types
        project-type-foo
        project-type-bar
        (real-call-interactively (symbol-function #'call-interactively)))

  (+projection-test-setup)

  (before-each
    (setq project-type-foo (projection-type :name 'foo :predicate ".foo" :run "foo-run"   :compile-multi-targets (lambda () '(("foo-cmd" . "cmd-1"))))
          project-type-bar (projection-type :name 'bar :predicate ".bar" :test "bar-test" :compile-multi-targets (lambda () '(("bar-cmd-1" . "cmd-2")
                                                                                                                              ("bar-cmd-2" . "cmd-3"))))
          project-type-baz (projection-type :name 'foo :predicate ".baz" :compile-multi-targets (lambda () '(("baz-cmd" . "cmd-4"))))
          projection-project-types (list project-type-foo project-type-bar))

    (f-touch ".git")
    (f-touch ".foo")
    (f-touch ".bar")

    (spy-on #'compile :and-return-value nil)
    (spy-on #'completing-read :and-return-value "foo-cmd")
    (spy-on #'read-shell-command :and-return-value ""))

  (it "Offers targets from all project types"
    ;; WHEN
    (call-interactively #'projection-multi-compile)

    ;; THEN
    (dolist (target '("foo-cmd" "bar-cmd-1" "bar-cmd-2"))
      (expect (+completion-table-candidates
               (spy-calls-args-for 'completing-read 0))
              :to-contain target)))

  (it "Run selected command from project type"
    ;; GIVEN
    (spy-on #'completing-read :and-return-value "foo-cmd")

    ;; WHEN
    (call-interactively #'projection-multi-compile)

    ;; THEN
    (expect #'compile :to-have-been-called-with "cmd-1" nil))

  (it "Does not offer targets from unmatched project types"
    ;; WHEN
    (call-interactively #'projection-multi-compile)

    ;; THEN
    (expect (+completion-table-candidates
             (spy-calls-args-for 'completing-read 0))
            :not :to-contain "baz-cmd"))

  (describe "Raw compile-multi targets"
    (it "Offers targets"
      ;; GIVEN
      (oset project-type-foo compile-multi-targets '(("raw-list" . "cmd-for-raw-list")))
      (spy-on #'completing-read :and-return-value "raw-list")

      ;; WHEN
      (call-interactively #'projection-multi-compile)

      ;; THEN
      (expect (+completion-table-candidates
               (spy-calls-args-for 'completing-read 0))
              :to-contain "raw-list"))

    (it "Run selected command"
      ;; GIVEN
      (oset project-type-foo compile-multi-targets '(("raw-list" . "cmd-for-raw-list")))
      (spy-on #'completing-read :and-return-value "raw-list")

      ;; WHEN
      (call-interactively #'projection-multi-compile)

      ;; THEN
      (expect #'compile :to-have-been-called-with "cmd-for-raw-list" nil)))

  (describe "Project type commands"
    (it "Offers targets"
      ;; WHEN
      (call-interactively #'projection-multi-compile)

      ;; THEN
      (dolist (target '("project:foo:run" "project:bar:test"))
        (expect (+completion-table-candidates
                 (spy-calls-args-for 'completing-read 0))
                :to-contain target)))

    (it "Run selected command"
      ;; GIVEN
      (spy-on #'completing-read :and-return-value "project:foo:run")

      ;; WHEN
      (call-interactively #'projection-multi-compile)

      ;; THEN
      (expect #'compile :to-have-been-called-with "foo-run")))

  (describe "Embark"
    :var (test-set-command-callback)
    (before-all
      (defun +compile-multi-target-from-completing-read (target)
        (cdr (compile-multi-embark-transformer 'compile-multi target))))
    (before-each
      (setf (symbol-function 'test-set-command-callback)
            (lambda (&rest _) (error "Spy function called")))
      (spy-on 'test-set-command-callback))

    (it "Can interactively set commands"
      ;; GIVEN
      (let ((target (+compile-multi-embark-target "foo-cmd")))
        (funcall-interactively #'projection-multi-embark-set-build-command-dwim target))

      ;; WHEN
      (call-interactively #'projection-commands-build-project)

      ;; THEN
      (expect #'compile :to-have-been-called-with "cmd-1"))

    (it "Can interactively set commands specific to the compile-multi target"
      ;; GIVEN
      (oset project-type-foo compile-multi-targets
            (lambda ()
              `(("foo-cmd" . ,(propertize "cmd-1" 'projection-set-command-callback 'test-set-command-callback)))))
      (let ((target (+compile-multi-embark-target "foo-cmd")))
        ;; WHEN
        (funcall-interactively #'projection-multi-embark-set-build-command-dwim target))

      ;; THEN
      (expect 'test-set-command-callback :to-have-been-called-times 1))

    (it "Can interactively set commands disregarding setter specific to the compile-multi target"
      ;; GIVEN
      (oset project-type-foo compile-multi-targets
            (lambda ()
              `(("foo-cmd" . ,(propertize "cmd-1" 'projection-set-command-callback 'test-set-command-callback)))))
      (let ((target (+compile-multi-embark-target "foo-cmd")))
        ;; WHEN
        (funcall-interactively #'projection-multi-embark-set-build-command target))

      ;; THEN
      (expect 'test-set-command-callback :to-have-been-called-times 0)

      ;; WHEN
      (call-interactively #'projection-commands-build-project)

      ;; THEN
      (expect #'compile :to-have-been-called-with "cmd-1")))
  )
