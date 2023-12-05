;; -*- lexical-binding: t -*-


(require 'projection-types)
(require 'projection-utils)
(require 'projection-utils-cmake)

(require 'projection-test-utils)

(describe "Project type CMake"
  (+projection-test-setup)

  (before-each
    (setq projection-project-types (list projection-project-type-cmake))

    ;; Setup a simple CMake project.
    (+projection-setup-project
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
    (+expect-interactive-command-calls-compile-with
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
      (+expect-interactive-command-calls-compile-with
       #'projection-configure-project
       "cmake -S . -B blarg")))

  (it "Includes the configured CMake build-type"
    ;; GIVEN
    ;;   A simple CMake project where I've set the build type to Release.
    (+interactively-set-cmake-build-type "Release")
    ;; WHEN
    ;;   I call projection-configure-project.
    ;; THEN
    ;;   The invoked command includes the desired build type.
    (+expect-interactive-command-calls-compile-with
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
      (+expect-interactive-command-calls-compile-with
       #'projection-configure-project
       "cmake -S . -B build Foo Bar")))

  (it "Runs tests through a separate ctest binary"
    ;; GIVEN
    (let ((projection-cmake-ctest-options)
          (projection-cmake-ctest-environment-variables))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-test-project
       "ctest --test-dir build test")))

  (it "Assigns any configured environment variables when running tests"
    ;; GIVEN
    (let ((projection-cmake-ctest-options)
          (projection-cmake-ctest-environment-variables
           '(("foo" . "bar"))))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-test-project
       "env foo\\=bar ctest --test-dir build test")))

  (it "Forwards any configured options when running tests"
    ;; GIVEN
    (let ((projection-cmake-ctest-options '("-foo" "-bar"))
          (projection-cmake-ctest-environment-variables))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-test-project
       "ctest --test-dir build -foo -bar test")))

  (describe "Clear build directory"
    (it "Fails when no build directory configured"
      ;; GIVEN
      (let* ((projection-cmake-build-directory nil)
             ;; WHEN
             (err (should-error (projection-cmake-clear-build-directory))))
        ;; THEN
        (expect (cadr err) :to-match
                "Cannot remove build directory at unconfigured location")))

    (it "Fails build directory does not exist"
      ;; GIVEN/WHEN
      (let* ((err (should-error (projection-cmake-clear-build-directory))))
        ;; THEN
        (expect (cadr err) :to-match
                "Build directory .* already does not exist")))

    (describe "With existing build-directory"
      (before-each
        (mkdir (projection-cmake--build-directory 'expand) 'parents))

      (it "Aborts when user decides not to clear build directory"
        ;; GIVEN
        (spy-on #'compile :and-return-value nil)
        (spy-on #'yes-or-no-p :and-return-value nil)
        (spy-on #'message :and-return-value nil)

        ;; WHEN
        (projection-cmake-clear-build-directory)

        ;; THEN
        (expect 'yes-or-no-p :to-have-been-called-times 1)
        (expect 'compile :to-have-been-called-times 0))

      (it "Removes the build directory"
        ;; GIVEN
        (spy-on #'compile :and-return-value nil)
        (spy-on #'yes-or-no-p :and-return-value t)
        (spy-on #'message :and-return-value nil)

        ;; WHEN
        (projection-cmake-clear-build-directory)

        ;; THEN
        (expect 'yes-or-no-p :to-have-been-called-times 1)
        (expect 'compile :to-have-been-called-times 1)
        (expect (spy-calls-args-for 'compile 0) :to-equal '("rm -rf build")))))

  (describe "With the CMake help-target backend"
    :var ((existing-target-backend projection-cmake-target-backend))
    (before-all (require 'projection-multi-cmake))
    (before-all (setq projection-cmake-target-backend 'help-target))
    (after-all (setq projection-cmake-target-backend projection-cmake-target-backend))

    (it "Does not construct a CMake query file"
      ;; GIVEN
      (expect projection-cmake-target-backend :to-equal 'help-target)

      ;; WHEN
      (+expect-interactive-command-calls-compile-with
       #'projection-configure-project
       "cmake -S . -B build")

      ;; THEN
      (expect (file-expand-wildcards "build/.cmake/api/v1/query/*/query.json")
              :to-be nil))

    (describe "Multi compile"
      :var ((expected-targets '("cmake:all" "cmake:clean" "cmake:main_lib")))

      (it "Extracts CMake targets from the Ninja backends"
        ;; GIVEN
        (let ((projection-cmake-configure-options '("-GNinja")))
          (call-interactively #'projection-configure-project)

          ;; WHEN
          (let ((cmake-targets (mapcar #'car (projection-multi-cmake-targets))))
            ;; THEN
            (dolist (expected-target expected-targets)
              (expect expected-target :to-be-in cmake-targets)))))

      (it "Extracts CMake targets from the Make backends"
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
      (+expect-interactive-command-calls-compile-with
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
      (+projection-setup-project-tree
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
        (+expect-interactive-command-calls-compile-with
         #'projection-configure-project
         "cmake -S . -B build --preset\\=configurePreset1")

        (expect 'completing-read :to-have-been-called-times 1)
        (expect (+completion-table-candidates
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
        (+expect-interactive-command-calls-compile-with
         #'projection-configure-project
         "cmake -S . -B build --preset\\=configurePreset1")

        (expect 'completing-read :to-have-been-called-times 1)
        (expect (+completion-table-candidates
                 (spy-calls-args-for 'completing-read 0))
                :to-equal configure-presets)

        ;; WHEN
        ;;   I call projection-configure-project again.
        ;; THEN
        ;;   * The user was not prompted for presets again.
        ;;   * The invoked command includes the preset option from the prior
        ;;   invocation.
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-configure-project
          "cmake -S . -B build --preset\\=configurePreset1"))

        ;; WHEN
        ;;   I call projection-build-project.
        ;; THEN
        ;;   * The user was prompted for the build presets this time round.
        ;;   * The invoked command includes the preset option.
        (spy-on #'completing-read :and-return-value "buildPreset1")

        (+expect-interactive-command-calls-compile-with
         #'projection-build-project
         "cmake --build build --preset\\=buildPreset1")

        (expect 'completing-read :to-have-been-called-times 1)
        (expect (+completion-table-candidates
                 (spy-calls-args-for 'completing-read 0))
                :to-equal build-presets)))

    (it "Doesn't prompt for a preset when one was set interactively"
      ;; GIVEN
      ;;   * A CMake project with a presets configuration.
      ;;   * I have interactively choosen an available configure preset.
      (+interactively-set-cmake-preset 'configure "configurePreset1")

      ;; WHEN
      ;;   I call projection-configure-project.
      ;; THEN
      ;;   * I was not prompted for a CMake preset to use.
      ;;   * The invoked shell command references the configured preset.
      (+with-completing-read-not-called
       (+expect-interactive-command-calls-compile-with
        #'projection-configure-project
        "cmake -S . -B build --preset\\=configurePreset1")))

    (it "Allows the user to interactively clear the configured preset"
      ;; GIVEN
      ;;   * A CMake project with a presets configuration.
      ;;   * I have interactively choosen an available configure preset.
      (+interactively-set-cmake-preset 'configure "configurePreset1")

      ;; WHEN
      ;;   I call projection-configure-project.
      ;; THEN
      ;;   The invoked shell command references the configured preset.
      (+with-completing-read-not-called
       (+expect-interactive-command-calls-compile-with
        #'projection-configure-project
        "cmake -S . -B build --preset\\=configurePreset1"))

      ;; WHEN
      ;;   I call projection-cmake-set-preset and supply an empty string.
      (+interactively-set-cmake-preset 'configure "")

      ;; WHEN
      ;;   I call projection-configure-project again.
      ;; THEN
      ;;   * The user was prompted for a configure preset to use.
      ;;   * The invoked shell-command references the chosen preset.
      (spy-on #'completing-read :and-return-value "configurePreset2")

      (+expect-interactive-command-calls-compile-with
       #'projection-configure-project
       "cmake -S . -B build --preset\\=configurePreset2")

      (expect 'completing-read :to-have-been-called-times 1)
      (expect (+completion-table-candidates
               (spy-calls-args-for 'completing-read 0))
              :to-equal configure-presets))

    (it "Allows users to interactively set a default preset for all build types"
      ;; GIVEN
      ;;   A CMake project with a presets configuration.
      ;; WHEN
      ;;   I have interactively choosen an available preset for all build types.
      (+interactively-set-cmake-preset nil "defaultPreset")

      ;; WHEN
      ;;   I call projection-configure-project.
      ;; THEN
      ;;   * The user was not prompted for a configure preset to use.
      ;;   * The invoked shell-command references the chosen default preset.
      (+with-completing-read-not-called
       (+expect-interactive-command-calls-compile-with
        #'projection-configure-project
        "cmake -S . -B build --preset\\=defaultPreset"))

      ;; WHEN
      ;;   I call projection-build-project.
      ;; THEN
      ;;   * The user was not prompted for a build preset to use.
      ;;   * The invoked shell-command references the chosen default preset.
      (+with-completing-read-not-called
       (+expect-interactive-command-calls-compile-with
        #'projection-build-project
        "cmake --build build --preset\\=defaultPreset")))

    (it "Prefers a build-type specific preset over the default preset"
      ;; GIVEN
      ;;   A CMake project with a presets configuration.
      ;; WHEN
      ;;   I have interactively choosen an available preset for configure
      ;;   and then for all build types.
      (+interactively-set-cmake-preset 'configure "configurePreset1")
      (+interactively-set-cmake-preset nil "defaultPreset")

      ;; WHEN
      ;;   I call projection-configure-project.
      ;; THEN
      ;;   * The user was not prompted for a configure preset to use.
      ;;   * The invoked shell-command references the chosen configure preset.
      (+with-completing-read-not-called
       (+expect-interactive-command-calls-compile-with
        #'projection-configure-project
        "cmake -S . -B build --preset\\=configurePreset1"))

      ;; WHEN
      ;;   I call projection-build-project.
      ;; THEN
      ;;   * The user was not prompted for a build preset to use.
      ;;   * The invoked shell-command references the chosen default preset.
      (+with-completing-read-not-called
       (+expect-interactive-command-calls-compile-with
        #'projection-build-project
        "cmake --build build --preset\\=defaultPreset")))

    (it "Never uses a preset when configured"
      ;; Try run configure
      (let ((projection-cmake-preset 'disable))
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
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

        (+expect-interactive-command-calls-compile-with
         #'projection-configure-project
         "cmake -S . -B build --preset\\=configurePreset1")

        (expect 'completing-read :to-have-been-called-times 1)

        ;; WHEN
        ;;   I call projection-configure-project again.
        ;; THEN
        ;;   The user was prompted and the chosen prest was included in the
        ;;   invoked compilation command again.
        (spy-on #'completing-read :and-return-value "configurePreset2")

        (+expect-interactive-command-calls-compile-with
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
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-configure-project
          "cmake -S . -B build"))

        ;; WHEN
        ;;   I have interactively choosen an available configure preset.
        (+interactively-set-cmake-preset 'configure "configurePreset1")

        ;; WHEN
        ;;   I call projection-configure-project again.
        ;; THEN
        ;;   The user was not prompted and the invoked shell command references
        ;;   the chosen preset.
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
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
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-build-project
          "cmake --build build --preset\\=buildPreset1"))

        ;; WHEN
        ;;   I call projection-configure-project.
        ;; THEN
        ;;   * The user was prompted once for a preset from the set configure
        ;;   presets.
        ;;   * The invoked command includes the preset option.
        (spy-on #'completing-read :and-return-value "configurePreset1")

        (+expect-interactive-command-calls-compile-with
         #'projection-configure-project
         "cmake -S . -B build --preset\\=configurePreset1")

        (expect 'completing-read :to-have-been-called-times 1)

        ;; WHEN
        ;;   I call projection-configure-project again.
        ;; THEN
        ;;   * The user was not prompted for presets again.
        ;;   * The invoked command includes the preset option from the prior
        ;;   invocation.
        (+expect-interactive-command-calls-compile-with
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
          (expect "cmake:workflow:default" :to-be-in cmake-targets)))))
  )
