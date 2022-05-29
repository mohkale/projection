;; -*- lexical-binding: t -*-

(require 'f)
(require 'projector-core)

(describe "Projector register type"
  :var (projector-types)
  (before-each
    (setq projector-types nil))

  (it "Registers new projects"
    ;; GIVEN
    ;;   An empty project type list.
    (expect projector-types :to-be nil)
    ;; WHEN
    ;;   I register a new project of type 'foo.
    (projector-register-type 'foo
      :predicate "foobar"
      :configure "configure"
      :build     "build"
      :test      "test"
      :run       "run"
      :package   "package"
      :install   "install")
    ;; THEN
    ;;   `projector-types' now contains the new project entry.
    (expect projector-types :to-equal
            '((foo
               (predicate . "foobar")
               (configure . "configure")
               (build . "build")
               (test . "test")
               (run . "run")
               (package . "package")
               (install . "install")))))

  (it "Can update an existing project"
    ;; GIVEN
    ;;   An existing project definition for 'foo.
    (projector-register-type 'foo :predicate "foobar" :test "test")
    (expect projector-types :to-equal '((foo (predicate . "foobar") (test . "test"))))
    ;; WHEN
    ;;   I update the existing definition with a new :test value and
    ;;   an initial value for the build field.
    (projector-register-type 'foo :test "test2" :build "build")
    ;; THEN
    ;;   The final project definition for 'foo includes the updated
    ;;   field values.
    (expect projector-types :to-equal
            '((foo
               (build . "build")
               (predicate . "foobar")
               (test . "test2"))))))

(describe "Projector determine project type"
  :var (projector-types config dir default-directory)
  ;; Create and start running each test in a temporary directory.
  (before-each
    (setq dir (make-temp-file "buttercup-test-" t)
          default-directory dir))
  (after-each (delete-directory dir t))

  ;; Reset defined project-types list for each test.
  (before-each
    (setq projector-types nil config nil))

  (describe "With some defined project types"
    ;; Register a few basic project types to test against.
    (before-each
      (projector-register-type 'foo :predicate ".foo" :run "foo")
      (projector-register-type 'bar :predicate ".bar" :run "bar")
      (projector-register-type 'baz :predicate ".baz" :run "baz"))

    (it "Can determine project type using file-name predicate"
      ;; GIVEN
      ;;   A project containing the file marker for project type 'bar.
      (f-touch ".bar")

      ;; WHEN
      ;;   I try to determine the current project type.
      (setq config (projector-project-type default-directory))

      ;; THEN
      ;;   The current project matches the definition for 'bar.
      (expect config :to-equal (assoc 'bar projector-types)))

    (it "Can match using any entry in a list of predicates"
      ;; GIVEN
      ;;   A project containing one of the file markers for project type
      ;;   'bar.
      (projector-register-type 'bar :predicate '(".bar" ".bar2" ".bar3"))
      (f-touch ".bar3")

      ;; WHEN
      ;;   I try to determine the current project type.
      (setq config (projector-project-type default-directory))

      ;; THEN
      ;;   The current project matches the definition for 'bar.
      (expect config :to-equal (assoc 'bar projector-types)))

    (it "Can match using a function entry in a list of predicates"
      ;; GIVEN
      ;;   A list of predicates for project-type 'bar with a function
      ;;   predicate that evaluates to true.
      (projector-register-type 'bar :predicate `(".bar" (lambda () t) ".bar3"))

      ;; WHEN
      ;;   I try to determine the current project type.
      (setq config (projector-project-type default-directory))

      ;; THEN
      ;;   The current project matches the definition for 'bar.
      (expect config :to-equal (assoc 'bar projector-types)))

    (it "Can determine project type using function predicate"
      ;; GIVEN
      ;;   A project list with a predicate function for 'bar that returns
      ;;   true.
      (projector-register-type 'bar :predicate (lambda () t))

      ;; WHEN
      ;;   I try to determine the current project type.
      (setq config (projector-project-type default-directory))

      ;; THEN
      ;;   The current project matches the definition for 'bar.
      (expect config :to-equal (assoc 'bar projector-types)))

    (it "Picks earliest entry in `projector-types' when multiple project types are valid"
      ;; GIVEN
      ;;   2 Marker files matching both the 'foo and 'baz
      ;;   project types.
      (f-touch ".foo")
      (setq config (projector-project-type default-directory))
      (expect config :to-equal (assoc 'foo projector-types))
      (projector-reset-project-cache t)

      (f-touch ".baz")

      ;; WHEN
      ;;   I try to determine the current project type.
      (setq config (projector-project-type default-directory))

      ;; THEN
      ;;   The current project matches the definition for 'bar
      ;;   because it appears earlier in the project type list
      ;;   compared to 'foo.
      (expect config :to-equal (assoc 'baz projector-types)))

    (it "Falls back to default project type when project type cannot be matched"
      ;; GIVEN
      ;;   An empty repository directory which won't match any project definition.
      ;; WHEN
      ;;   I determine the project-type for the current directory.
      (setq config (projector-project-type default-directory))

      ;; THEN
      ;;   I determine the project-type for the current directory.
      (expect config :to-equal (cons t projector-default-type))))

  (it "Falls back to default project type when no project types are defined"
    ;; GIVEN
    ;;   No defined project-types.
    (expect projector-types :to-be nil)

    ;; WHEN
    ;;   I determine the project-type for the current directory.
    (setq config (projector-project-type default-directory))

    ;; THEN
    ;;   I determine the project-type for the current directory.
    (expect config :to-equal (cons t projector-default-type))))
