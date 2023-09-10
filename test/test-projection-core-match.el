;; -*- lexical-binding: t -*-

(require 'f)

(require 'projection-core)
(require 'projection-core-match)
(require 'projection-core-commands)

(require 'projection-test-utils)

(describe "Projection match"
  :var (projection-project-types config config2)
  (+projection-test-setup)

  ;; Reset defined project-types list for each test.
  (before-each
    (setq config nil projection-project-types nil)
    (+projection-setup-project '(("README.txt" . ""))))

  (describe "With some defined project types"
    :var ((project-type-foo (projection-type :name 'foo :predicate ".foo" :run "foo"))
          (project-type-bar (projection-type :name 'bar :predicate ".bar" :run "bar"))
          (project-type-baz (projection-type :name 'baz :predicate ".baz" :run "baz")))
    ;; Register a few basic project types to test against.
    (before-each
      (push project-type-foo projection-project-types)
      (push project-type-bar projection-project-types)
      (push project-type-baz projection-project-types))

    (it "Can determine project type using file-name predicate"
      ;; GIVEN
      (f-touch ".bar")

      ;; WHEN
      (setq config (projection-project-type default-directory))

      ;; THEN
      (expect config :to-equal project-type-bar))

    (it "Can match using any entry in a list of predicates"
      (let ((test-project-type
             (projection-type :name 'bar :predicate '(".bar" ".bar2" ".bar3"))))
        ;; GIVEN
        (push test-project-type projection-project-types)
        (f-touch ".bar3")

        ;; WHEN
        (setq config (projection-project-type default-directory))

        ;; THEN
        (expect config :to-equal test-project-type)))

    (it "Can match using a function entry in a list of predicates"
      (let ((test-project-type
             (projection-type :name 'bar :predicate `(".bar" (lambda () t) ".bar3"))))
        ;; GIVEN
        (push test-project-type projection-project-types)

        ;; WHEN
        (setq config (projection-project-type default-directory))

        ;; THEN
        (expect config :to-equal test-project-type)))

    (it "Can determine project type using function predicate"
      (let ((test-project-type
             (projection-type :name 'bar :predicate (lambda () t))))
        ;; GIVEN
        (push test-project-type projection-project-types)

        ;; WHEN
        (setq config (projection-project-type default-directory))

        ;; THEN
        (expect config :to-equal test-project-type)))

    (it "Picks earliest entry in `projection-project-types' when multiple project types are valid"
      ;; GIVEN
      (f-touch ".foo")
      (setq config (projection-project-type default-directory))
      (expect config :to-equal project-type-foo)
      (+projection-clear-all-cache)

      (f-touch ".baz")

      ;; WHEN
      (setq config (projection-project-type default-directory))

      ;; THEN
      (expect config :to-equal project-type-baz))

    (it "Falls back to default project type when project type cannot be matched"
      ;; WHEN
      (setq config (projection-project-type default-directory))

      ;; THEN
      (expect config :to-equal projection-default-type))

    (it "Can update primary project type interactively"
      ;; GIVEN
      (f-touch ".foo")
      (f-touch ".bar")

      ;; WHEN
      (setq config (projection-project-type default-directory))

      ;; THEN
      (expect config :to-equal project-type-bar)

      ;; WHEN
      ;;   I interactively try to set the primary project type to 'foo.
      (spy-on #'completing-read :and-call-fake (+fake-completing-read "foo"))
      (call-interactively #'projection-set-primary-project-type)

      ;; THEN
      ;;   I was prompted to select the type from both of the applicable project types.
      (expect 'completing-read :to-have-been-called-times 1)
      (expect (cadr (spy-calls-args-for 'completing-read 0))
              :to-equal '(bar foo))

      ;; WHEN
      (setq config (projection-project-type default-directory))

      ;; THEN
      ;;   It now matches the 'foo project type because it was chosen.
      (expect config :to-equal project-type-foo))

    (it "Can update the primary project type to one not matching the project with a prefix arg"
      ;; GIVEN
      (f-touch ".foo")
      ;; WHEN
      (setq config (projection-project-type default-directory))
      ;; THEN
      (expect config :to-equal project-type-foo)

      ;; WHEN
      ;;   I interactively try to set the primary project type to 'bar.
      (spy-on #'completing-read :and-call-fake (+fake-completing-read "bar"))
      (let ((current-prefix-arg '(4)))
        (call-interactively #'projection-set-primary-project-type))

      ;; THEN
      ;;   I was prompted to select the type from all defined project types.
      (expect 'completing-read :to-have-been-called-times 1)
      (expect (cadr (spy-calls-args-for 'completing-read 0))
              :to-equal '(baz bar foo))

      ;; WHEN
      (setq config (projection-project-type default-directory))

      ;; THEN
      ;;   It now matches the 'bar project type because it was chosen.
      (expect config :to-equal project-type-bar))

    (it "Can add and remove extra project types interactively"
      ;; GIVEN
      ;;   A project with a marker file for the 'foo project type.
      (f-touch ".foo")

      ;; WHEN
      ;;   I determine the default project type/s for the current project.
      (setq config (projection-project-type default-directory)
            config2 (projection-project-types default-directory))

      ;; THEN
      ;;   It matches the 'foo project type.
      (expect config :to-equal project-type-foo)
      (expect config2 :to-equal (list project-type-foo))

      ;; WHEN
      ;;   I interactively try to add an extra 'bar project-type.
      (spy-on #'completing-read-multiple :and-call-fake (+fake-completing-read (list "+bar")))
      (call-interactively #'projection-update-extra-project-types)

      ;; THEN
      ;;   I was prompted to select the type from all defined project types.
      (expect 'completing-read-multiple :to-have-been-called-times 1)
      (expect (cadr (spy-calls-args-for 'completing-read-multiple 0))
              :to-equal '("-foo" "+baz" "+bar"))

      ;; WHEN
      ;;   I determine the default project type/s for the current project.
      (setq config (projection-project-type default-directory)
            config2 (projection-project-types default-directory))

      ;; THEN
      ;;   * It still matches the 'foo project type by default
      ;;   * Now also matches the 'bar project type.
      (expect config :to-equal project-type-foo)
      (expect config2 :to-equal (list project-type-foo project-type-bar))

      ;; WHEN
      ;;   I interactively try to remove the primary 'foo project-type.
      (spy-on #'completing-read-multiple :and-call-fake (+fake-completing-read (list "-foo")))
      (call-interactively #'projection-update-extra-project-types)

      ;; THEN
      ;;   I was prompted to select the type from all defined project types.
      (expect 'completing-read-multiple :to-have-been-called-times 1)
      (expect (cadr (spy-calls-args-for 'completing-read-multiple 0))
              :to-equal '("-foo" "-bar" "+baz"))

      ;; WHEN
      ;;   I determine the default project type/s for the current project.
      (setq config (projection-project-type default-directory)
            config2 (projection-project-types default-directory))

      ;; THEN
      ;;   * It no longer matches the 'foo project type at all.
      ;;   * Has updated the primary project type to match the 'bar project type
      ;;   because it is the only applicable one left.
      (expect config :to-equal project-type-bar)
      (expect config2 :to-equal (list project-type-bar))

      ;; WHEN
      ;;   I interactively try to add an extra 'baz project-type.
      (spy-on #'completing-read-multiple :and-call-fake (+fake-completing-read (list "+baz")))
      (call-interactively #'projection-update-extra-project-types)

      ;; THEN
      ;;   I was prompted to select the type from all defined project types.
      (expect 'completing-read-multiple :to-have-been-called-times 1)
      (expect (cadr (spy-calls-args-for 'completing-read-multiple 0))
              :to-equal '("-bar" "+baz" "+foo"))

      ;; WHEN
      ;;   I determine the default project type/s for the current project.
      (setq config (projection-project-type default-directory)
            config2 (projection-project-types default-directory))

      ;; THEN
      ;;   * It still matches the 'bar project type by default
      ;;   * Now also matches the 'baz project type.
      (expect config :to-equal project-type-bar)
      (expect config2 :to-equal (list project-type-bar project-type-baz))

      ;; WHEN
      ;;   I interactively try to set the primary project type to 'baz.
      (spy-on #'completing-read :and-call-fake (+fake-completing-read "baz"))
      (call-interactively #'projection-set-primary-project-type)

      ;; THEN
      ;;   I was prompted to select the type from all matching project types.
      (expect 'completing-read :to-have-been-called-times 1)
      (expect (cadr (spy-calls-args-for 'completing-read 0))
              :to-equal '(bar baz))

      ;; WHEN
      ;;   I determine the default project type/s for the current project.
      (setq config (projection-project-type default-directory)
            config2 (projection-project-types default-directory))

      ;; THEN
      ;;   * It now matches the 'baz project type by default
      ;;   * It still matches the 'baz project type as an extra type.
      (expect config :to-equal project-type-baz)
      (expect config2 :to-equal (list project-type-baz project-type-bar))

      ;; WHEN
      ;;   I interactively try to remove all project types.
      (spy-on #'completing-read-multiple :and-call-fake (+fake-completing-read (list "-bar" "-baz")))

      ;; THEN
      ;;   An error was raised because at least one project type has to be active.
      (expect
       (call-interactively #'projection-update-extra-project-types)
       :to-throw 'user-error (list "Cannot remove all supported project types"))

      ;; THEN
      ;;   I was prompted to select the type from all defined project types.
      (expect 'completing-read-multiple :to-have-been-called-times 1)
      (expect (cadr (spy-calls-args-for 'completing-read-multiple 0))
              :to-equal '("-baz" "-bar" "+foo")))

    (describe "Locally overridden primary project type"
      (it "Matches the overridden primary project type"
        ;; GIVEN
        ;;   * The current project matches the baz project type by default.
        ;;   * The primary project type has then been locally overridden to foo.
        (f-touch ".baz")
        (setq projection-primary-project-type 'foo)

        ;; WHEN
        (setq config (projection-project-type default-directory))
        (setq config2 (projection-project-types default-directory))

        ;; THEN
        (expect config :to-equal project-type-foo)
        (expect config2 :to-equal (list project-type-foo project-type-baz)))

      (it "Can override the primary project type interactively"
        ;; GIVEN
        ;;   * The current project matches the baz project type by default.
        ;;   * The primary project type has been overridden locally to foo.
        ;;   * The primary project type has been interactively overridden locally to bar.
        (f-touch ".baz")
        (setq projection-primary-project-type 'foo)

        (spy-on #'completing-read :and-call-fake (+fake-completing-read "bar"))
        (call-interactively #'projection-set-primary-project-type)

        ;; WHEN
        (setq config (projection-project-type default-directory))
        (setq config2 (projection-project-types default-directory))

        ;; THEN
        (expect config :to-equal project-type-bar)
        (expect config2 :to-equal (list project-type-bar project-type-foo project-type-baz))
        (expect projection-primary-project-type :to-equal 'foo))))

  (it "Falls back to default project type when no project types are defined"
    ;; GIVEN
    ;;   No defined project-types.
    (expect projection-project-types :to-be nil)

    ;; WHEN
    ;;   I determine the project-type for the current directory.
    (setq config (projection-project-type default-directory))

    ;; THEN
    ;;   I determine the project-type for the current directory.
    (expect config :to-equal projection-default-type)))
