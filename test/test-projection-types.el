;; -*- lexical-binding: t; eval: (evil-vimish-fold-mode +1) -*-

(require 'projection-test-utils)

(require 'projection-types)
(require 'projection-utils)
(require 'projection-utils-cmake)

(require 'projection-multi-npm-scripts)
(require 'projection-multi-yarn-scripts)

(describe "Projection types"
  :var (original-directory
        test-directory
        cmake-types)
  ;; Save the original directory before we run any tests.
  (before-all
    (setq original-directory default-directory))
  ;; For each test change to a temporary working directory.
  (before-each
    (cd (setq test-directory (make-temp-file "buttercup-test-" t)))
    (projection-reset-project-cache t))
  ;; And change back to the original directory and delete the test directory
  ;; after the test finishes.
  (after-each
    (when (file-equal-p default-directory test-directory)
      (cd default-directory)
      (delete-directory test-directory t)))
  ;; Make compile synchronously block.
  (before-each
    (spy-on #'compile :and-call-fake
            (lambda (command &rest _)
              (projection--shell-command-to-string command))))

  (describe "CMake"
    (before-each
      (setq projection-project-types (list projection-project-type-cmake))

      ;; Setup a simple CMake project.
      (projection-find-test--setup-project
       '(("main.cpp" . "#include <stdio.h>

int main() {
  printf(\"Hello world\\n\");
}")
         ("CMakeLists.txt" . "cmake_minimum_required(VERSION 3.2)
project(projection_test)

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS \"${CMAKE_CXX_FLAGS} -std=c++11 -O3\")

add_library(main_lib main.cpp)
add_executable(main main.cpp)
target_link_libraries(main main_lib)
"))))

    (it "Can be identified"
      (+projection-project-matches-p 'cmake))

    (it "Can configure a CMake project"
      ;; GIVEN
      ;;   A simple CMake project.
      ;; WHEN
      ;;   I call projection-configure-project.
      ;; THEN
      ;;   A compilation was started to configure the project.
      (interactively-call-compile-command
       #'projection-configure-project
       "cmake -S . -B build"))

    (it "Adapts configuring to the configured CMake build directory"
      ;; GIVEN
      ;;   * A simple CMake project.
      ;;   * An Emacs instance that overrode the build directory to somewhere
      ;;   else.
      ;; WHEN
      ;;   I call projection-configure-project.
      ;; THEN
      ;;   A compilation was started to configure the project.
      (let ((projection-cmake-build-directory "blarg"))
        (interactively-call-compile-command
         #'projection-configure-project
         "cmake -S . -B blarg")))

    (it "Includes the configured CMake build-type"
      ;; GIVEN
      ;;   A simple CMake project where I've set the build type to Release.
      (interactively-set-cmake-build-type "Release")
      ;; WHEN
      ;;   I call projection-configure-project.
      ;; THEN
      ;;   The invoked command includes the desired build type.
      (interactively-call-compile-command
       #'projection-configure-project
       "cmake -S . -B build -DCMAKE_BUILD_TYPE\\=Release"))

    (it "Can set any customized configure options"
      ;; GIVEN
      ;;   A simple CMake project where I've overridden the configure options.
      (let ((projection-cmake-configure-options '("Foo" "Bar")))
        ;; WHEN
        ;;   I call projection-configure-project.
        ;; THEN
        ;;   The invoked command includes the extra configure options.
        (interactively-call-compile-command
         #'projection-configure-project
         "cmake -S . -B build Foo Bar")))

    (it "Runs tests through a separate ctest binary"
      ;; GIVEN
      (let ((projection-cmake-ctest-options)
            (projection-cmake-ctest-environment-variables))
        ;; WHEN/THEN
        (interactively-call-compile-command
         #'projection-test-project
         "ctest --test-dir build test")))

    (it "Assigns any configured environment variables when running tests"
      ;; GIVEN
      (let ((projection-cmake-ctest-options)
            (projection-cmake-ctest-environment-variables
             '(("foo" . "bar"))))
        ;; WHEN/THEN
        (interactively-call-compile-command
         #'projection-test-project
         "env foo\\=bar ctest --test-dir build test")))

    (it "Forwards any configured options when running tests"
      ;; GIVEN
      (let ((projection-cmake-ctest-options '("-foo" "-bar"))
            (projection-cmake-ctest-environment-variables))
        ;; WHEN/THEN
        (interactively-call-compile-command
         #'projection-test-project
         "ctest --test-dir build -foo -bar test")))

    (describe "With the CMake help-target backend"
      :var ((existing-target-backend projection-cmake-target-backend))
      (before-all (require 'projection-multi-cmake))
      (before-all (setq projection-cmake-target-backend 'help-target))
      (after-all (setq projection-cmake-target-backend projection-cmake-target-backend))

      (it "Does not construct a CMake query file"
        ;; GIVEN
        (expect projection-cmake-target-backend :to-equal 'help-target)

        ;; WHEN
        (interactively-call-compile-command
         #'projection-configure-project
         "cmake -S . -B build")

        ;; THEN
        (expect (file-expand-wildcards "build/.cmake/api/v1/query/*/query.json")
                :to-be nil))

      (describe "Multi compile"
        :var ((expected-targets '("cmake:all" "cmake:clean" "cmake:main_lib")))

        (it "Extracts CMake targets from the Make backends"
          ;; GIVEN
          (let ((projection-cmake-configure-options '("-GNinja")))
            (call-interactively #'projection-configure-project)

            ;; WHEN
            (let ((cmake-targets (mapcar #'car (projection-multi-cmake-targets))))
              ;; THEN
              (dolist (expected-target expected-targets)
                (expect expected-target :to-be-in cmake-targets)))))

        (it "Extracts CMake targets from the Ninja backends"
          ;; GIVEN
          (let ((projection-cmake-configure-options '("-GUnix Makefiles")))
            (call-interactively #'projection-configure-project)

            ;; WHEN
            (let ((cmake-targets (mapcar #'car (projection-multi-cmake-targets))))
              ;; THEN
              (dolist (expected-target expected-targets)
                (expect expected-target :to-be-in cmake-targets)))))

        (it "Filters out targets matching the configured regexp"
          ;; GIVEN
          (call-interactively #'projection-configure-project)

          ;; WHEN
          (let* ((projection-multi-cmake-exclude-targets (rx bol "main_lib" eol))
                 (cmake-targets (mapcar #'car (projection-multi-cmake-targets))))
            ;; THEN
            (expect "main_lib" :not :to-be-in cmake-targets)))))

    (describe "With the CMake file API backend"
      :var ((existing-target-backend projection-cmake-target-backend)
            (expected-targets '("cmake:all" "cmake:clean" "cmake:main_lib" "cmake:main")))
      (before-all (setq projection-cmake-target-backend 'code-model))
      (after-all (setq projection-cmake-target-backend projection-cmake-target-backend))

      (it "Constructs a CMake query file while configuring"
        ;; WHEN
        (interactively-call-compile-command
         #'projection-configure-project
         "cmake -S . -B build")

        ;; THEN
        (expect (file-expand-wildcards "build/.cmake/api/v1/query/*/query.json")
                :not :to-be nil))

      (it "Extracts CMake targets from the code-model"
        ;; GIVEN
        (call-interactively #'projection-configure-project)

        ;; WHEN
        (let ((cmake-targets (mapcar #'car (projection-multi-cmake-targets))))
          ;; THEN
          (dolist (expected-target expected-targets)
            (expect expected-target :to-be-in cmake-targets))))

      (it "Filters out targets matching the configured regexp"
        ;; GIVEN
        (call-interactively #'projection-configure-project)

        ;; WHEN
        (let* ((projection-multi-cmake-exclude-targets (rx bol "main_lib" eol))
               (cmake-targets (mapcar #'car (projection-multi-cmake-targets))))
          ;; THEN
          (expect "main_lib" :not :to-be-in cmake-targets))))

    (describe "With a CMake presets configuration"
      :var ((configure-presets '("configurePreset1" "configurePreset2" "configurePreset3"))
            (build-presets '("buildPreset1")))
      (before-each
        (projection-find-test--setup-project-tree
         '(("CMakePresets.json" . "{
  \"version\": 6,
  \"cmakeMinimumRequired\": {
    \"major\": 3,
    \"minor\": 23,
    \"patch\": 0
  },
  \"configurePresets\": [
    {
      \"name\": \"configurePreset1\",
      \"displayName\": \"Preset number 1 for configuring\"
    },
    {
      \"name\": \"configurePreset2\",
      \"displayName\": \"Preset number 2 for configuring\"
    },
    {
      \"name\": \"configurePreset3\",
      \"displayName\": \"Preset number 3 for configuring\"
    }
  ],
  \"buildPresets\": [
    {
      \"name\": \"buildPreset1\",
      \"configurePreset\": \"configurePreset1\"
    }
  ],
  \"workflowPresets\": [
    {
      \"name\": \"default\",
      \"steps\": [
        {
          \"type\": \"configure\",
          \"name\": \"configurePreset1\"
        },
        {
          \"type\": \"build\",
          \"name\": \"buildPreset1\"
        }
      ]
    }
  ],
  \"vendor\": {
    \"example.com/ExampleIDE/1.0\": {
      \"autoFormat\": false
    }
  }
}"))))

      (it "Prompts for the current projects preset interactively"
        ;; GIVEN
        ;;   * A CMake project with a presets configuration.
        ;;   * The projection CMake config specifies to prompt once and then
        ;;   cache the chosen preset.
        (let ((projection-cmake-preset 'prompt-once))
          (spy-on #'completing-read :and-return-value "configurePreset1")

          ;; WHEN
          ;;   I call projection-configure-project.
          ;; THEN
          ;;   * The user was prompted once for a preset from the set configure
          ;;   presets.
          ;;   * The invoked command includes the preset option.
          (interactively-call-compile-command
           #'projection-configure-project
           "cmake -S . -B build --preset\\=configurePreset1")

          (expect 'completing-read :to-have-been-called-times 1)
          (expect (get-completion-table-candidates
                   (spy-calls-args-for 'completing-read 0))
                  :to-equal configure-presets)))

      (it "Caches chosen presets per build-type after first invocation"
        ;; GIVEN
        ;;   * A CMake project with a presets configuration.
        ;;   * The projection CMake config specifies to prompt once and then
        ;;   cache the chosen preset.
        (let ((projection-cmake-preset 'prompt-once))
          (spy-on #'completing-read :and-return-value "configurePreset1")

          ;; WHEN
          ;;   I call projection-configure-project.
          ;; THEN
          ;;   * The user was prompted once for a preset from the set configure
          ;;   presets.
          ;;   * The invoked command includes the preset option.
          (interactively-call-compile-command
           #'projection-configure-project
           "cmake -S . -B build --preset\\=configurePreset1")

          (expect 'completing-read :to-have-been-called-times 1)
          (expect (get-completion-table-candidates
                   (spy-calls-args-for 'completing-read 0))
                  :to-equal configure-presets)

          ;; WHEN
          ;;   I call projection-configure-project again.
          ;; THEN
          ;;   * The user was not prompted for presets again.
          ;;   * The invoked command includes the preset option from the prior
          ;;   invocation.
          (with-completing-read-not-called
           (interactively-call-compile-command
            #'projection-configure-project
            "cmake -S . -B build --preset\\=configurePreset1"))

          ;; WHEN
          ;;   I call projection-build-project.
          ;; THEN
          ;;   * The user was prompted for the build presets this time round.
          ;;   * The invoked command includes the preset option.
          (spy-on #'completing-read :and-return-value "buildPreset1")

          (interactively-call-compile-command
           #'projection-build-project
           "cmake --build build --preset\\=buildPreset1")

          (expect 'completing-read :to-have-been-called-times 1)
          (expect (get-completion-table-candidates
                   (spy-calls-args-for 'completing-read 0))
                  :to-equal build-presets)))

      (it "Doesn't prompt for a preset when one was set interactively"
        ;; GIVEN
        ;;   * A CMake project with a presets configuration.
        ;;   * I have interactively choosen an available configure preset.
        (interactively-set-cmake-preset 'configure "configurePreset1")

        ;; WHEN
        ;;   I call projection-configure-project.
        ;; THEN
        ;;   * I was not prompted for a CMake preset to use.
        ;;   * The invoked shell command references the configured preset.
        (with-completing-read-not-called
         (interactively-call-compile-command
          #'projection-configure-project
          "cmake -S . -B build --preset\\=configurePreset1")))

      (it "Allows the user to interactively clear the configured preset"
        ;; GIVEN
        ;;   * A CMake project with a presets configuration.
        ;;   * I have interactively choosen an available configure preset.
        (interactively-set-cmake-preset 'configure "configurePreset1")

        ;; WHEN
        ;;   I call projection-configure-project.
        ;; THEN
        ;;   The invoked shell command references the configured preset.
        (with-completing-read-not-called
         (interactively-call-compile-command
          #'projection-configure-project
          "cmake -S . -B build --preset\\=configurePreset1"))

        ;; WHEN
        ;;   I call projection-cmake-set-preset and supply an empty string.
        (interactively-set-cmake-preset 'configure "")

        ;; WHEN
        ;;   I call projection-configure-project again.
        ;; THEN
        ;;   * The user was prompted for a configure preset to use.
        ;;   * The invoked shell-command references the chosen preset.
        (spy-on #'completing-read :and-return-value "configurePreset2")

        (interactively-call-compile-command
         #'projection-configure-project
         "cmake -S . -B build --preset\\=configurePreset2")

        (expect 'completing-read :to-have-been-called-times 1)
        (expect (get-completion-table-candidates
                 (spy-calls-args-for 'completing-read 0))
                :to-equal configure-presets))

      (it "Allows users to interactively set a default preset for all build types"
        ;; GIVEN
        ;;   A CMake project with a presets configuration.
        ;; WHEN
        ;;   I have interactively choosen an available preset for all build types.
        (interactively-set-cmake-preset nil "defaultPreset")

        ;; WHEN
        ;;   I call projection-configure-project.
        ;; THEN
        ;;   * The user was not prompted for a configure preset to use.
        ;;   * The invoked shell-command references the chosen default preset.
        (with-completing-read-not-called
         (interactively-call-compile-command
          #'projection-configure-project
          "cmake -S . -B build --preset\\=defaultPreset"))

        ;; WHEN
        ;;   I call projection-build-project.
        ;; THEN
        ;;   * The user was not prompted for a build preset to use.
        ;;   * The invoked shell-command references the chosen default preset.
        (with-completing-read-not-called
         (interactively-call-compile-command
          #'projection-build-project
          "cmake --build build --preset\\=defaultPreset")))

      (it "Prefers a build-type specific preset over the default preset"
        ;; GIVEN
        ;;   A CMake project with a presets configuration.
        ;; WHEN
        ;;   I have interactively choosen an available preset for configure
        ;;   and then for all build types.
        (interactively-set-cmake-preset 'configure "configurePreset1")
        (interactively-set-cmake-preset nil "defaultPreset")

        ;; WHEN
        ;;   I call projection-configure-project.
        ;; THEN
        ;;   * The user was not prompted for a configure preset to use.
        ;;   * The invoked shell-command references the chosen configure preset.
        (with-completing-read-not-called
         (interactively-call-compile-command
          #'projection-configure-project
          "cmake -S . -B build --preset\\=configurePreset1"))

        ;; WHEN
        ;;   I call projection-build-project.
        ;; THEN
        ;;   * The user was not prompted for a build preset to use.
        ;;   * The invoked shell-command references the chosen default preset.
        (with-completing-read-not-called
         (interactively-call-compile-command
          #'projection-build-project
          "cmake --build build --preset\\=defaultPreset")))

      (it "Never uses a preset when configured"
        ;; Try run configure
        (let ((projection-cmake-preset 'disable))
          (with-completing-read-not-called
           (interactively-call-compile-command
            #'projection-configure-project
            "cmake -S . -B build"))))

      (it "Always prompts for a preset when configured"
        ;; GIVEN
        ;;   * A CMake project with a presets configuration.
        ;;   * An Emacs where the user chose to always be prompted.
        (let ((projection-cmake-preset 'prompt-always))
          ;; WHEN
          ;;   I call projection-configure-project.
          ;; THEN
          ;;   The user was prompted and the chosen prest was included in the
          ;;   invoked compilation command.
          (spy-on #'completing-read :and-return-value "configurePreset1")

          (interactively-call-compile-command
           #'projection-configure-project
           "cmake -S . -B build --preset\\=configurePreset1")

          (expect 'completing-read :to-have-been-called-times 1)

          ;; WHEN
          ;;   I call projection-configure-project again.
          ;; THEN
          ;;   The user was prompted and the chosen prest was included in the
          ;;   invoked compilation command again.
          (spy-on #'completing-read :and-return-value "configurePreset2")

          (interactively-call-compile-command
           #'projection-configure-project
           "cmake -S . -B build --preset\\=configurePreset2")

          (expect 'completing-read :to-have-been-called-times 1)))

      (it "Never prompts for a preset when configured"
        ;; GIVEN
        ;;   * A CMake project with a presets configuration.
        ;;   * An Emacs where the user chose to never be prompted.
        (let ((projection-cmake-preset 'silent))
          ;; WHEN
          ;;   I call projection-configure-project.
          ;; THEN
          ;;   The user was not prompted and no preset was included in the invoked
          ;;   shell command.
          (with-completing-read-not-called
           (interactively-call-compile-command
            #'projection-configure-project
            "cmake -S . -B build"))

          ;; WHEN
          ;;   I have interactively choosen an available configure preset.
          (interactively-set-cmake-preset 'configure "configurePreset1")

          ;; WHEN
          ;;   I call projection-configure-project again.
          ;; THEN
          ;;   The user was not prompted and the invoked shell command references
          ;;   the chosen preset.
          (with-completing-read-not-called
           (interactively-call-compile-command
            #'projection-configure-project
            "cmake -S . -B build --preset\\=configurePreset1"))))

      (it "Prompts and saves chosen preset for build-type after first invocation only when multiple presets are available"
        ;; GIVEN
        ;;   * A CMake project with a presets configuration.
        ;;   * An Emacs where the user chose to only be prompted when multiple
        ;;   presets are available for configuring but not building.
        (let ((projection-cmake-preset 'prompt-once-when-multiple))
          ;; WHEN
          ;;   I call projection-build-project.
          ;; THEN
          ;;   The user was not prompted and the invoked shell command used the
          ;;   only available build preset.
          (with-completing-read-not-called
           (interactively-call-compile-command
            #'projection-build-project
            "cmake --build build --preset\\=buildPreset1"))

          ;; WHEN
          ;;   I call projection-configure-project.
          ;; THEN
          ;;   * The user was prompted once for a preset from the set configure
          ;;   presets.
          ;;   * The invoked command includes the preset option.
          (spy-on #'completing-read :and-return-value "configurePreset1")

          (interactively-call-compile-command
           #'projection-configure-project
           "cmake -S . -B build --preset\\=configurePreset1")

          (expect 'completing-read :to-have-been-called-times 1)

          ;; WHEN
          ;;   I call projection-configure-project again.
          ;; THEN
          ;;   * The user was not prompted for presets again.
          ;;   * The invoked command includes the preset option from the prior
          ;;   invocation.
          (interactively-call-compile-command
           #'projection-configure-project
           "cmake -S . -B build --preset\\=configurePreset1")

          (expect 'completing-read :to-have-been-called-times 1)))

      (describe "Multi compile"
        (before-all (require 'projection-multi-cmake))

        (it "Includes targets for any workflow presets"
          ;; GIVEN
          (spy-on #'completing-read :and-return-value "configurePreset1")
          (call-interactively #'projection-configure-project)

          ;; WHEN
          (let ((cmake-targets (mapcar #'car (projection-multi-cmake-targets))))
            ;; THEN
            (expect "cmake:workflow:default" :to-be-in cmake-targets))))))

  (describe "NPM"
    (before-each
      (setq projection-project-types (list projection-project-type-npm))

      (projection-find-test--setup-project
       '(("package.json" . "{
  \"name\": \"projection-test\",
  \"version\": \"1.0.0\",
  \"description\": \"\",
  \"main\": \"index.js\",
  \"scripts\": {
    \"foo\": \"foo bar\",
    \"baz\": \"baz bag\"
  },
  \"author\": \"\",
  \"license\": \"ISC\",
  \"dependencies\": {
    \"is-boolean\": \"^0.0.2\"
  }
}")
         ("package-lock.json" . "{
  \"name\": \"projection-test\",
  \"version\": \"1.0.0\",
  \"lockfileVersion\": 2,
  \"requires\": true,
  \"packages\": {},
  \"dependencies\": {}
}"))))
  (it "Can be identified"
    (+projection-project-matches-p 'npm))

  (describe "Multi compile"
    (it "Can extract available scripts"
      ;; GIVEN
      ;;   A npm project.
      ;; WHEN
      ;;   I extract available npm script targets.
      (let ((targets (projection-multi-npm-script-targets)))
        ;; THEN
        ;;   The targets were extracted from the projects scripts correctly.
        (expect targets :to-equal
                '(("npm:foo" . "npm run foo")
                  ("npm:baz" . "npm run baz")))))))

  (describe "Yarn"
    (before-each
      (setq projection-project-types (list projection-project-type-yarn))

      (projection-find-test--setup-project
       '(("package.json" . "{
  \"name\": \"projection-test\",
  \"version\": \"1.0.0\",
  \"description\": \"\",
  \"main\": \"index.js\",
  \"scripts\": {
    \"foo\": \"foo bar\",
    \"baz\": \"baz bag\"
  },
  \"author\": \"\",
  \"license\": \"ISC\",
  \"dependencies\": {
    \"is-boolean\": \"^0.0.2\"
  }
}")
         ("yarn.lock" . "# THIS IS AN AUTOGENERATED FILE. DO NOT EDIT THIS FILE DIRECTLY.
# yarn lockfile v1
"))))
    (it "Can be identified"
      (+projection-project-matches-p 'yarn))

    (describe "Multi compile"
      (it "Can extract available scripts"
        ;; Yarn requires having run yarn install before you can check available scripts.
        ;; Here we just mock it out altogether.
        (spy-on #'shell-command :and-call-fake
                (lambda (&rest _)
                  (insert "yarn run v1.22.19
info Commands available from binary scripts: tape
info Project commands
   - baz
      baz bag
   - foo
      foo bar
question Which command would you like to run?: ")))

        ;; GIVEN
        ;;   A yarn project.
        ;; WHEN
        ;;   I extract available yarn script targets.
        (let ((targets (projection-multi-yarn-script-targets)))
          ;; THEN
          ;;   * The targets were extracted from the projects scripts correctly.
          ;;   * The targets includes any scripts from package binaries.
          (expect targets :to-equal
                  '(("yarn:tape" . "yarn run tape")
                    ("yarn:baz" . "yarn run baz")
                    ("yarn:foo" . "yarn run foo"))))

        (expect #'shell-command :to-have-been-called-times 1))))
    )
