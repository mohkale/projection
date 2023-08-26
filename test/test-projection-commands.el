;; -*- lexical-binding: t -*-

(require 'f)
(require 'vc)
(require 'projection-commands)

(describe "Projection registered commands"
  :var (projection-project-types dir default-directory
        project-type-foo
        (real-call-interactively (symbol-function #'call-interactively)))
  ;; Create a temporary project directory that will be re-used across each run.
  (before-all
    (setq dir (make-temp-file "buttercup-test-" t)
          default-directory dir))
  (after-all (delete-directory dir t))

  ;; Register a new project type that matches the current project directory.
  (before-each
    (setq
     project-type-foo  (projection-type :name 'foo :predicate ".foo" :run "foo")
     projection-project-types (list project-type-foo))
    (f-touch ".git")
    (f-touch ".foo"))

  ;; Bust the project cache for each test-case, it isn't needed for tests.
  (after-each (projection-reset-project-cache 'all-projects))

  (describe "Project compile commands"
    (before-each
      (spy-on #'compile)
      ;; (spy-on #'read-shell-command)
      (spy-on #'call-interactively))

    (it "Runs configured shell-command for current project type"
      ;; WHEN
      ;;   I try to run the 'run command for the current project.
      (funcall real-call-interactively #'projection-run-project)

      ;; THEN
      ;;   I expect `compile' to have been called with the configured
      ;;   shell command for the current project.
      (expect 'compile :to-have-been-called-with "foo")
      (expect 'call-interactively :not :to-have-been-called))

    (it "Runs configured command function for current project type"
      (let ((func (lambda () (interactive) t)))
        ;; GIVEN
        ;;   A project-type matching the current project with an interactive
        ;;   run function.
        (oset project-type-foo run func)

        ;; WHEN
        ;;   I try to run the 'run command for the current project.
        (funcall real-call-interactively #'projection-run-project)

        ;; THEN
        ;;   The registered command for this project was run interactively.
        (expect 'compile :not :to-have-been-called)
        (expect 'call-interactively :to-have-been-called-with func)))

    (it "Fails early if no project could be found relative to current directory"
      ;; GIVEN
      ;;   There's no project dominating the current directory.
      (f-delete ".git")
      (vc-clear-context)

      ;; WHEN
      ;;   I try to run the run command for the current project.
      ;; THEN
      ;;   I get an error because the current project could not be found.
      (let ((err (should-error (funcall real-call-interactively #'projection-run-project)
                               :type 'error)))
        (expect (cadr err) :to-equal
                (concat "No project found relative to " default-directory))))

    (it "Fails if no project-type could be matched and no default-commands configured"
      ;; GIVEN
      ;;   No project types have been configured.
      (let (projection-project-types projection-default-type)
        ;; WHEN
        ;;   I try to run the run command for the current project.
        ;; THEN
        ;;   I get an error because no project type matching the current project
        ;;   could be found.
        (expect (funcall real-call-interactively #'projection-package-project)
              :to-throw 'projection-command-error
              (list (format "No project type matching project %s/ found and the default \
project-type does not support the command: package"
                          default-directory)))))

    (it "Fails if registered command doesn't exist for current project type"
      (expect (funcall real-call-interactively #'projection-package-project)
              :to-throw 'projection-command-error
              '("Project of type foo does not support the command: package")))

    (it "Fails if registered command isn't valid"
      ;; GIVEN
      ;;   Current project has an invalid run command type.
      (oset project-type-foo run 'foo)

      (let ((err (should-error (funcall real-call-interactively #'projection-run-project)
                               :type 'error)))
        ;; WHEN
        ;;   I try to run the run command for the current project.
        ;; THEN
        ;;   I get an error because projection doesn't know how to run the
        ;;   invalid run command.
        (expect (cadr err) :to-equal
                "Do not know how to run run command foo")))

    (it "Uses configured command for initial-input when prompting for a registered command and cache is empty"
      ;; GIVEN
      ;;   A mock on read-shell-command which will return result.
      (spy-on #'read-shell-command :and-return-value "result")

      ;; WHEN
      ;;   I try to run the run command for the current project with prompt.
      (let ((current-prefix-arg '(4)))
        (funcall real-call-interactively #'projection-run-project 'prompt))

      ;; THEN
      ;;   * Prompt used the initial input from the project configuration and
      ;;   passed it through to compile.
      ;;   * The cached command for the run task for the current project is
      ;;   equal to the command we just ran.
      (expect 'read-shell-command :to-have-been-called-with
              (format "[%s] Run project: " (file-name-nondirectory default-directory))
              "foo"
              'compile-history)
      (expect 'compile :to-have-been-called-with "result")
      (expect (projection--cache-get (project-current) 'run) :to-equal "result"))

    (it "Uses the cached command when prompting for a run command"
      ;; GIVEN
      ;;   We've run the run command for the current project with prompt
      ;;   and cached a value of "result".
      (projection-set-run-command "result" (project-current))
      (expect (projection--cache-get (project-current) 'run) :to-equal "result")
      (spy-on #'read-shell-command :and-return-value "prompted")

      ;; WHEN
      ;;   I try to run the run command for the current project with prompt.
      (let ((current-prefix-arg '(4)))
       (funcall real-call-interactively #'projection-run-project))

      ;; THEN
      ;;   Prompt used the initial input from the cached command value and
      ;;   passed it through to compile.
      (expect 'read-shell-command :to-have-been-called-with
              (format "[%s] Run project: " (file-name-nondirectory default-directory))
              "result"
              'compile-history)
      (expect 'compile :to-have-been-called-with "prompted"))

    (it "Uses the directory-local variable as the command instead of the project-type"
      ;; GIVEN
      (let ((projection-project-run-cmd "bar"))
        ;; WHEN
        (funcall real-call-interactively #'projection-run-project)

        ;; THEN
        (expect 'compile :to-have-been-called-with "bar")))

    (it "Uses the cached command instead of the directory-local variable when set"
      ;; GIVEN
      (let ((projection-project-run-cmd "bar"))
        (projection-set-run-command "baz" (project-current))

        ;; WHEN
        (funcall real-call-interactively #'projection-run-project)

        ;; THEN
        (expect 'compile :to-have-been-called-with "baz")))

    (describe "With dynamic commands"
              (it "Doesn't cache the generated compilation shell-command by default"
                  ;; GIVEN
                  ;;   A run command configured as a function which returns the shell
                  ;;   command "foo".
                  (oset project-type-foo run (lambda () "foo"))

                  ;; WHEN
                  ;;   I try to run the run command for the current project.
                  (funcall real-call-interactively #'projection-run-project)

                  ;; THEN
                  ;;   No command was cached for the current project.
                  (expect (projection--cache-get (project-current) 'run) :to-be nil)))))
