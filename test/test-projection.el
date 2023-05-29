;; -*- lexical-binding: t -*-

(require 'f)
(require 'projection-core)

(describe "Projection determine project type"
  :var (projection-project-types config dir default-directory)
  ;; Create and start running each test in a temporary directory.
  (before-each
    (setq dir (make-temp-file "buttercup-test-" t)
          default-directory dir))
  (after-each (delete-directory dir t))

  ;; Reset defined project-types list for each test.
  (before-each
    (setq projection-project-types nil config nil))

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
      ;;   A project containing the file marker for project type 'bar.
      (f-touch ".bar")

      ;; WHEN
      ;;   I try to determine the current project type.
      (setq config (projection-project-type default-directory))

      ;; THEN
      ;;   The current project matches the definition for 'bar.
      (expect config :to-equal project-type-bar))

    (it "Can match using any entry in a list of predicates"
      (let ((test-project-type
             (projection-type :name 'bar :predicate '(".bar" ".bar2" ".bar3"))))
        ;; GIVEN
        ;;   A project containing one of the file markers for project type
        ;;   'bar.
        (push test-project-type projection-project-types)
        (f-touch ".bar3")

        ;; WHEN
        ;;   I try to determine the current project type.
        (setq config (projection-project-type default-directory))

        ;; THEN
        ;;   The current project matches the definition for 'bar.
        (expect config :to-equal test-project-type)))

    (it "Can match using a function entry in a list of predicates"
      (let ((test-project-type
             (projection-type :name 'bar :predicate `(".bar" (lambda () t) ".bar3"))))
        ;; GIVEN
        ;;   A list of predicates for project-type 'bar with a function
        ;;   predicate that evaluates to true.
        (push test-project-type projection-project-types)

        ;; WHEN
        ;;   I try to determine the current project type.
        (setq config (projection-project-type default-directory))

        ;; THEN
        ;;   The current project matches the definition for 'bar.
        (expect config :to-equal test-project-type)))

    (it "Can determine project type using function predicate"
      (let ((test-project-type
             (projection-type :name 'bar :predicate (lambda () t))))
        ;; GIVEN
        ;;   A project list with a predicate function for 'bar that returns
        ;;   true.
        (push test-project-type projection-project-types)

        ;; WHEN
        ;;   I try to determine the current project type.
        (setq config (projection-project-type default-directory))

        ;; THEN
        ;;   The current project matches the definition for 'bar.
        (expect config :to-equal test-project-type)))

    (it "Picks earliest entry in `projection-project-types' when multiple project types are valid"
      ;; GIVEN
      ;;   2 Marker files matching both the 'foo and 'baz
      ;;   project types.
      (f-touch ".foo")
      (setq config (projection-project-type default-directory))
      (expect config :to-equal project-type-foo)
      (projection-reset-project-cache t)

      (f-touch ".baz")

      ;; WHEN
      ;;   I try to determine the current project type.
      (setq config (projection-project-type default-directory))

      ;; THEN
      ;;   The current project matches the definition for 'bar
      ;;   because it appears earlier in the project type list
      ;;   compared to 'foo.
      (expect config :to-equal project-type-baz))

    (it "Falls back to default project type when project type cannot be matched"
      ;; GIVEN
      ;;   An empty repository directory which won't match any project definition.
      ;; WHEN
      ;;   I determine the project-type for the current directory.
      (setq config (projection-project-type default-directory))

      ;; THEN
      ;;   I determine the project-type for the current directory.
      (expect config :to-equal projection-default-type)))

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
