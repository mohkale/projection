;; -*- lexical-binding: t -*-

(require 'f)
(require 'projection-core)

(describe "Projection register type"
  :var (projection-types)
  (before-each
    (setq projection-types nil))

  (it "Registers new projects"
    ;; GIVEN
    ;;   An empty project type list.
    (expect projection-types :to-be nil)
    ;; WHEN
    ;;   I register a new project of type 'foo.
    (projection-register-type 'foo
      :predicate "foobar"
      :configure "configure"
      :build     "build"
      :test      "test"
      :run       "run"
      :package   "package"
      :install   "install")
    ;; THEN
    ;;   `projection-types' now contains the new project entry.
    (expect projection-types :to-equal
            '((foo
               (:predicate . "foobar")
               (:configure . "configure")
               (:build . "build")
               (:test . "test")
               (:run . "run")
               (:package . "package")
               (:install . "install")))))

  (it "Can update an existing project"
    ;; GIVEN
    ;;   An existing project definition for 'foo.
    (projection-register-type 'foo :predicate "foobar" :test "test")
    (expect projection-types :to-equal '((foo (:predicate . "foobar") (:test . "test"))))
    ;; WHEN
    ;;   I update the existing definition with a new :test value and
    ;;   an initial value for the build field.
    (projection-register-type 'foo :test "test2" :build "build")
    ;; THEN
    ;;   The final project definition for 'foo includes the updated
    ;;   field values.
    (expect projection-types :to-equal
            '((foo
               (:build . "build")
               (:predicate . "foobar")
               (:test . "test2")))))

  (it "Always saves test-prefix or test-suffix as a list"
    ;; GIVEN
    ;;   An empty collection of project types.
    (expect projection-types :to-be nil)
    ;; WHEN
    ;;   I register a project type with :test-prefix and :test-suffix.
    ;;   One being a list and the other not.
    (projection-register-type 'foo
      :predicate "foobar"
      :test-prefix "prefix"
      :test-suffix (list "suffix"))
    ;; THEN
    ;;   The properties I registered are saved correctly as lists.
    (expect projection-types :to-equal
            '((foo
               (:predicate . "foobar")
               (:test-prefix . ("prefix"))
               (:test-suffix . ("suffix")))))
    ;; WHEN
    ;;   I register a project type with :test-prefix and :test-suffix.
    ;;   Alternating the list status of each compared to before.
    (projection-register-type 'foo
      :test-prefix (list "prefix2")
      :test-suffix "suffix2")
    ;; THEN
    ;;   The properties I registered are saved correctly as lists.
    (expect projection-types :to-equal
            '((foo
               (:predicate . "foobar")
               (:test-prefix . ("prefix2"))
               (:test-suffix . ("suffix2")))))))

(describe "Projection determine project type"
  :var (projection-types config dir default-directory)
  ;; Create and start running each test in a temporary directory.
  (before-each
    (setq dir (make-temp-file "buttercup-test-" t)
          default-directory dir))
  (after-each (delete-directory dir t))

  ;; Reset defined project-types list for each test.
  (before-each
    (setq projection-types nil config nil))

  (describe "With some defined project types"
    ;; Register a few basic project types to test against.
    (before-each
      (projection-register-type 'foo :predicate ".foo" :run "foo")
      (projection-register-type 'bar :predicate ".bar" :run "bar")
      (projection-register-type 'baz :predicate ".baz" :run "baz"))

    (it "Can determine project type using file-name predicate"
      ;; GIVEN
      ;;   A project containing the file marker for project type 'bar.
      (f-touch ".bar")

      ;; WHEN
      ;;   I try to determine the current project type.
      (setq config (projection-project-type default-directory))

      ;; THEN
      ;;   The current project matches the definition for 'bar.
      (expect config :to-equal (assoc 'bar projection-types)))

    (it "Can match using any entry in a list of predicates"
      ;; GIVEN
      ;;   A project containing one of the file markers for project type
      ;;   'bar.
      (projection-register-type 'bar :predicate '(".bar" ".bar2" ".bar3"))
      (f-touch ".bar3")

      ;; WHEN
      ;;   I try to determine the current project type.
      (setq config (projection-project-type default-directory))

      ;; THEN
      ;;   The current project matches the definition for 'bar.
      (expect config :to-equal (assoc 'bar projection-types)))

    (it "Can match using a function entry in a list of predicates"
      ;; GIVEN
      ;;   A list of predicates for project-type 'bar with a function
      ;;   predicate that evaluates to true.
      (projection-register-type 'bar :predicate `(".bar" (lambda () t) ".bar3"))

      ;; WHEN
      ;;   I try to determine the current project type.
      (setq config (projection-project-type default-directory))

      ;; THEN
      ;;   The current project matches the definition for 'bar.
      (expect config :to-equal (assoc 'bar projection-types)))

    (it "Can determine project type using function predicate"
      ;; GIVEN
      ;;   A project list with a predicate function for 'bar that returns
      ;;   true.
      (projection-register-type 'bar :predicate (lambda () t))

      ;; WHEN
      ;;   I try to determine the current project type.
      (setq config (projection-project-type default-directory))

      ;; THEN
      ;;   The current project matches the definition for 'bar.
      (expect config :to-equal (assoc 'bar projection-types)))

    (it "Picks earliest entry in `projection-types' when multiple project types are valid"
      ;; GIVEN
      ;;   2 Marker files matching both the 'foo and 'baz
      ;;   project types.
      (f-touch ".foo")
      (setq config (projection-project-type default-directory))
      (expect config :to-equal (assoc 'foo projection-types))
      (projection-reset-project-cache t)

      (f-touch ".baz")

      ;; WHEN
      ;;   I try to determine the current project type.
      (setq config (projection-project-type default-directory))

      ;; THEN
      ;;   The current project matches the definition for 'bar
      ;;   because it appears earlier in the project type list
      ;;   compared to 'foo.
      (expect config :to-equal (assoc 'baz projection-types)))

    (it "Falls back to default project type when project type cannot be matched"
      ;; GIVEN
      ;;   An empty repository directory which won't match any project definition.
      ;; WHEN
      ;;   I determine the project-type for the current directory.
      (setq config (projection-project-type default-directory))

      ;; THEN
      ;;   I determine the project-type for the current directory.
      (expect config :to-equal (cons t projection-default-type))))

  (it "Falls back to default project type when no project types are defined"
    ;; GIVEN
    ;;   No defined project-types.
    (expect projection-types :to-be nil)

    ;; WHEN
    ;;   I determine the project-type for the current directory.
    (setq config (projection-project-type default-directory))

    ;; THEN
    ;;   I determine the project-type for the current directory.
    (expect config :to-equal (cons t projection-default-type))))
