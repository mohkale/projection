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
    ;; GIVEN/WHEN/THEN
    (+expect-interactive-command-calls-compile-with
     #'projection-configure-project
     "cmake -S . -B build"))

  (it "Adapts configuring to the configured CMake build directory"
    ;; GIVEN
    (let ((projection-cmake-build-directory "blarg"))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-configure-project
       "cmake -S . -B blarg")))

  (it "Includes the configured CMake build-type"
    ;; GIVEN
    (+interactively-set-cmake-build-type "Release")
    ;; WHEN/THEN
    (+expect-interactive-command-calls-compile-with
     #'projection-configure-project
     "cmake -S . -B build -DCMAKE_BUILD_TYPE\\=Release"))

  (it "Can set any customized configure options"
    ;; GIVEN
    (let ((projection-cmake-configure-options '("Foo" "Bar")))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-configure-project
       "cmake -S . -B build Foo Bar")))

  (it "Runs tests through a separate ctest binary"
    ;; GIVEN
    (let ((projection-cmake-ctest-environment-variables))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-test-project
       "ctest --test-dir build test")))

  (it "Assigns any configured environment variables when running tests"
    ;; GIVEN
    (let ((projection-cmake-ctest-environment-variables
           '(("foo" . "bar"))))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-test-project
       "env foo\\=bar ctest --test-dir build test")))

  (it "Runs ctest with a customized number of jobs in parallel"
    ;; GIVEN
    (let ((projection-cmake-ctest-jobs 10))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-test-project
       "ctest --test-dir build --parallel\\=10 test")))

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
          (configure-presets-display-names '("Preset number 1 for configuring"
                                             "Preset number 2 for configuring"
                                             "Preset number 3 for configuring"))
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
  \"testPresets\": [
    {
      \"name\": \"testPreset1WithConfigurePreset1\",
      \"configurePreset\": \"configurePreset1\"
    },
    {
      \"name\": \"testPreset1WithConfigurePreset2\",
      \"configurePreset\": \"configurePreset2\"
    },
    {
      \"name\": \"testPreset2WithConfigurePreset2\",
      \"configurePreset\": \"configurePreset2\"
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
      (let ((projection-cmake-preset 'prompt-once))
        (spy-on #'completing-read :and-return-value "Preset number 1 for configuring")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-configure-project
         "cmake -S . -B build --preset\\=configurePreset1")

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)
        (expect (+completion-table-candidates
                 (spy-calls-args-for 'completing-read 0))
                :to-equal configure-presets-display-names)))

    (it "Caches chosen presets per build-type after first invocation"
      ;; GIVEN
      (let ((projection-cmake-preset 'prompt-once))
        (spy-on #'completing-read :and-return-value "Preset number 1 for configuring")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-configure-project
         "cmake -S . -B build --preset\\=configurePreset1")

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)
        (expect (+completion-table-candidates
                 (spy-calls-args-for 'completing-read 0))
                :to-equal configure-presets-display-names)

        ;; WHEN/THEN
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-configure-project
          "cmake -S . -B build --preset\\=configurePreset1"))

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 0)

        ;; GIVEN
        (spy-on #'completing-read :and-return-value "buildPreset1")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-build-project
         "cmake --build build --preset\\=buildPreset1")

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)
        (expect (+completion-table-candidates
                 (spy-calls-args-for 'completing-read 0))
                :to-equal build-presets)))

    (it "Doesn't prompt for a preset when one was set interactively"
      ;; GIVEN
      (+interactively-set-cmake-preset 'configure "Preset number 1 for configuring")

      ;; WHEN/THEN
      (+with-completing-read-not-called
       (+expect-interactive-command-calls-compile-with
        #'projection-configure-project
        "cmake -S . -B build --preset\\=configurePreset1")))

    (it "Never uses a preset when configured"
      ;; GIVEN
      (let ((projection-cmake-preset 'disable))
        ;; WHEN/THEN
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-configure-project
          "cmake -S . -B build"))))

    (it "Always prompts for a preset when configured"
      ;; GIVEN
      (let ((projection-cmake-preset 'prompt-always))
        (spy-on #'completing-read :and-return-value "Preset number 1 for configuring")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-configure-project
         "cmake -S . -B build --preset\\=configurePreset1")

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)

        ;; GIVEN
        (spy-on #'completing-read :and-return-value "Preset number 2 for configuring")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-configure-project
         "cmake -S . -B build --preset\\=configurePreset2")

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)))

    (it "Never prompts for a preset when configured"
      ;; GIVEN
      (let ((projection-cmake-preset 'silent))
        ;; WHEN/THEN
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-configure-project
          "cmake -S . -B build"))

        ;; GIVEN
        (+interactively-set-cmake-preset 'configure "Preset number 1 for configuring")

        ;; WHEN/THEN
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-configure-project
          "cmake -S . -B build --preset\\=configurePreset1"))))

    (it "Prompts and saves chosen preset for build-type after first invocation only when multiple presets are available"
      ;; GIVEN
      (let ((projection-cmake-preset 'prompt-once-when-multiple))
        ;; WHEN/THEN
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-build-project
          "cmake --build build --preset\\=buildPreset1"))

        ;; GIVEN
        (spy-on #'completing-read :and-return-value "Preset number 1 for configuring")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-configure-project
         "cmake -S . -B build --preset\\=configurePreset1")

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-configure-project
         "cmake -S . -B build --preset\\=configurePreset1")

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)))

    (it "Prompts with only related targets matching the active configuration target for building and testing"
      ;; GIVEN
      (+interactively-set-cmake-preset 'configure "Preset number 2 for configuring")
      (spy-on #'completing-read :and-return-value "testPreset1WithConfigurePreset2")

      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-test-project
       "ctest --test-dir build --preset\\=testPreset1WithConfigurePreset2 test")

      ;; THEN
      (expect #'completing-read :to-have-been-called-times 1)
      (expect (+completion-table-candidates
               (spy-calls-args-for 'completing-read 0))
              :to-equal '("testPreset1WithConfigurePreset2" "testPreset2WithConfigurePreset2")))

    (it "Clears cached test preset when re-setting a configure preset"
      (let ((projection-cmake-preset 'prompt-always))
        (+interactively-set-cmake-preset 'configure "Preset number 2 for configuring")
        (+interactively-set-cmake-preset 'test "testPreset1WithConfigurePreset2")
        (+interactively-set-cmake-preset 'configure "Preset number 1 for configuring")

        ;; WHEN/THEN
        (spy-on #'completing-read :and-return-value "testPreset1WithConfigurePreset1")
        (+expect-interactive-command-calls-compile-with
         #'projection-test-project
         "ctest --test-dir build --preset\\=testPreset1WithConfigurePreset1 test")))

    (describe "Multi compile"
      (before-all (require 'projection-multi-cmake))

      (it "Includes targets for any workflow presets"
        ;; GIVEN
        (spy-on #'completing-read :and-return-value "Preset number 1 for configuring")
        (call-interactively #'projection-configure-project)

        ;; WHEN
        (let ((cmake-targets (mapcar #'car (projection-multi-cmake-targets))))
          ;; THEN
          (expect "cmake:workflow:default" :to-be-in cmake-targets)))))
  )
