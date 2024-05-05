;; -*- lexical-binding: t -*-

(require 'projection-artifacts)
(require 'projection-types)
(require 'projection-utils)
(require 'projection-type-cmake)
(require 'projection-multi-cmake)
(require 'projection-multi-ctest)
(require 'projection-dape)

(require 'projection-test-utils)

(describe "Project type Gradle"
  (+projection-test-setup)

  (before-each
    (setq projection-project-types (list projection-project-type-gradle))

    (+projection-setup-project '(("build.gradle.kts" . ""))))

  (it "Can be identified"
    (+projection-project-matches-p 'gradle))

  (describe "Commands"
    (it "Runs with gradle when gradlew is unused"
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-build-project
       "gradle --daemon build -x test"))

    (it "Runs with gradlew when set"
      ;; GIVEN
      (f-touch "gradlew")

      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-build-project
       "./gradlew --daemon build -x test"))

    (it "Runs without daemon when configured"
      ;; GIVEN
      (let ((projection-gradle-use-daemon))
        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-build-project
         "gradle build -x test"))))

  (describe "Multi compile"
    (before-all (require 'projection-multi-gradle))

    (it "Can determine gradle tasks"
      (let ((targets (mapcar #'car (projection-multi-gradle-targets))))
        (expect targets :to-have-same-items-as
                '("gradle:init"
                  "gradle:wrapper"
                  "gradle:buildEnvironment"
                  "gradle:components"
                  "gradle:dependencies"
                  "gradle:dependencyInsight"
                  "gradle:dependentComponents"
                  "gradle:help"
                  "gradle:model"
                  "gradle:projects"
                  "gradle:properties"
                  "gradle:tasks"))))))
