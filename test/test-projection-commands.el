;; -*- lexical-binding: t -*-

(require 'f)
(require 'vc)
(require 'projection-commands)

(describe "Projection registered commands"
  :var (projection-project-types dir default-directory)
  ;; Create a temporary project directory that will be re-used across each run.
  (before-all
    (setq dir (make-temp-file "buttercup-test-" t)
          default-directory dir))
  (after-all (delete-directory dir t))

  ;; Register a new project type that matches the current project directory.
  (before-each
    (setq projection-project-types nil)
    (projection-register-type 'foo :predicate ".foo" :run "foo")
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
      (projection-run-project)

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
        (projection-register-type 'foo :run func)

        ;; WHEN
        ;;   I try to run the 'run command for the current project.
        (projection-run-project)

        ;; THEN
        ;;   The registered command for this project was run interactively.
        (expect 'compile :not :to-have-been-called)
        (expect 'call-interactively :to-have-been-called-with func)))

    ;; Note: This is failing on snapshot emacs with github ci/cd for reasons I don't
    ;; understand. Will investigate later.
    (it "Fails early if no project could be found relative to current directory"
      ;; GIVEN
      ;;   There's no project dominating the current directory.
      (f-delete ".git")
      (vc-clear-context)

      ;; WHEN
      ;;   I try to run the run command for the current project.
      ;; THEN
      ;;   I get an error because the current project could not be found.
      (let ((err (should-error (projection-run-project) :type 'user-error)))
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
        (let ((err (should-error (projection-run-project) :type 'error)))
          (expect (cadr err) :to-equal
                  (format "No project type matching project %s/ found"
                          default-directory)))))

    (it "Fails if registered command doesn't exist for current project type"
      ;; GIVEN
      ;;   Current project doesn't have a package command.
      (let ((err (should-error (projection-package-project) :type 'error)))
        ;; WHEN
        ;;   I try to run the package command for the current project.
        ;; THEN
        ;;   I get an error because the current project doesn't have a
        ;;   package command configured.
        (expect (cadr err) :to-equal
                "Project of type foo does not support the command: package")))

    (it "Fails if registered command isn't valid"
      ;; GIVEN
      ;;   Current project has an invalid run command type.
      (projection-register-type 'foo :run 'foo)

      (let ((err (should-error (projection-run-project) :type 'user-error)))
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
      (projection-run-project 'prompt)

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
      (spy-on #'read-shell-command :and-return-value "result")
      (projection-run-project 'prompt)
      (expect (projection--cache-get (project-current) 'run) :to-equal "result")

      ;; WHEN
      ;;   I try to run the run command for the current project with prompt.
      (projection-run-project 'prompt)

      ;; THEN
      ;;   Prompt used the initial input from the cached command value and
      ;;   passed it through to compile.
      (expect 'read-shell-command :to-have-been-called-with
              (format "[%s] Run project: " (file-name-nondirectory default-directory))
              "result"
              'compile-history)
      (expect 'compile :to-have-been-called-with "result"))

    (it "Caches the current project-type after the first run"
      ;; GIVEN
      ;;   I run the run command for the current project and have the
      ;;   current project type cached.
      (spy-on #'projection--match-project-type :and-return-value
              (assoc 'foo projection-project-types))

      (expect (projection--cache-get (project-current) 'type) :to-be nil)
      (projection-run-project)
      (expect (projection--cache-get (project-current) 'type) :to-be 'foo)

      ;; WHEN
      ;;   I run the run command for the current project again.
      (projection-run-project)

      ;; THEN
      ;;   `projection' didn't try to re-determine the current project type.
      ;;   It loaded the result from the cache.
      )

    (describe "With dynamic commands"
      (it "Doesn't cache the generated compilation shell-command by default"
        ;; GIVEN
        ;;   A run command configured as a function which returns the shell
        ;;   command "foo".
        (projection-register-type 'foo :run (lambda () "foo"))

        ;; WHEN
        ;;   I try to run the run command for the current project.
        (projection-run-project)

        ;; THEN
        ;;   No command was cached for the current project.
        (expect (projection--cache-get (project-current) 'run) :to-be nil))

      (it "Can cache the compilation shell-command not the function which generates it"
        (let ((projection-cache-dynamic-commands t))
          ;; GIVEN
          ;;   * A run command configured as a function which returns the shell
          ;;   command "foo".
          ;;   * Caching of dynamic commands is enabled.
          (projection-register-type 'foo :run (lambda () "foo"))

          ;; WHEN
          ;;   I try to run the run command for the current project.
          (projection-run-project)

          ;; THEN
          ;;   The command cached for the run command is the result of calling the
          ;;   configured function, not the configured function itself.
          (expect (projection--cache-get (project-current) 'run) :to-equal "foo")))

      (it "Doesn't cache the generated compilation function by default"
        ;; GIVEN
        ;;   A run command configured as a function which returns another
        ;;   interactive function which should actually be run for compilation.
        (projection-register-type 'foo :run
                                  (lambda ()
                                    (lambda ()
                                      (interactive)
                                      "my-interactive-function")))

        ;; WHEN
        ;;   I try to run the run command for the current project.
        (projection-run-project)

        ;; THEN
        ;;   No command was cached for the current project.
        (expect (projection--cache-get (project-current) 'run) :to-be nil))

      (it "Can caches the compilation function not the function which generates it"
        (let ((projection-cache-dynamic-commands t))
          ;; GIVEN
          ;;   * A run command configured as a function which returns another
          ;;   interactive function which should actually be run for compilation.
          ;;   * Caching of dynamic commands is enabled.
          (projection-register-type 'foo :run
                                    (lambda ()
                                      (lambda ()
                                        (interactive)
                                        "my-interactive-function")))

          ;; WHEN
          ;;   I try to run the run command for the current project.
          (projection-run-project)

          ;; THEN
          ;;   The value cached for the run command is the result of calling the
          ;;   configured function, not the configured function itself.
          (let ((run-cached (projection--cache-get (project-current) 'run)))
            (expect (funcall run-cached) :to-equal "my-interactive-function"))))))
  )
