;; -*- lexical-binding: t -*-

(require 'projection-artifacts)
(require 'projection-types)
(require 'projection-utils)
(require 'projection-type-cmake)
(require 'projection-multi-cmake)
(require 'projection-multi-ctest)
(require 'projection-dape)

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

enable_testing()

set(CMAKE_CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS \"${CMAKE_CXX_FLAGS} -std=c++11 -O3\")

add_library(main_lib main.cpp)
add_executable(main main.cpp)
target_link_libraries(main main_lib)

add_test(NAME main-test COMMAND true main-test-arg)
set_property(TEST main-test PROPERTY ENVIRONMENT \"FOO=1\")
add_test(NAME hidden COMMAND true)
"))))

  (it "Can be identified"
    (+projection-project-matches-p 'cmake))

  (it "Can configure a CMake project"
    ;; GIVEN/WHEN/THEN
    (+expect-interactive-command-calls-compile-with
     #'projection-commands-configure-project
     "cmake -S . -B build"))

  (it "Adapts configuring to the configured CMake build directory"
    ;; GIVEN
    (let ((projection-cmake-build-directory "blarg"))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-commands-configure-project
       "cmake -S . -B blarg")))

  (it "Builds with a customized number of jobs in parallel"
    ;; GIVEN
    (let ((projection-build-jobs 10))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-commands-build-project
       "cmake --build build --parallel\\=10")))

  (it "Includes the configured CMake build-type"
    ;; GIVEN
    (+interactively-set-cmake-build-type "Release")
    ;; WHEN/THEN
    (+expect-interactive-command-calls-compile-with
     #'projection-commands-configure-project
     "cmake -S . -B build -DCMAKE_BUILD_TYPE\\=Release"))

  (it "Includes the configured CMake log-level"
    ;; GIVEN
    (+interactively-set-cmake-configure-log-level "DEBUG")
    ;; WHEN/THEN
    (+expect-interactive-command-calls-compile-with
     #'projection-commands-configure-project
     "cmake -S . -B build --log-level\\=DEBUG"))

  (it "Builds verbosely when configured"
    ;; GIVEN
    (+interactively-set-cmake-build-verbosely t)
    ;; WHEN/THEN
    (+expect-interactive-command-calls-compile-with
     #'projection-commands-build-project
     "cmake --build build --verbose"))

  (it "Can set any customized configure options"
    ;; GIVEN
    (let ((projection-cmake-configure-options '("Foo" "Bar")))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-commands-configure-project
       "cmake -S . -B build Foo Bar")))

  (it "Runs tests through a separate ctest binary"
    ;; GIVEN
    (let ((projection-cmake-ctest-environment-variables))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-commands-test-project
       "ctest --test-dir build test")))

  (it "Assigns any configured environment variables when running tests"
    ;; GIVEN
    (let ((projection-cmake-ctest-environment-variables
           '(("foo" . "bar"))))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-commands-test-project
       "env foo\\=bar ctest --test-dir build test")))

  (it "Runs ctest with a customized number of jobs in parallel"
    ;; GIVEN
    (let ((projection-test-jobs 10))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-commands-test-project
       "ctest --test-dir build --parallel\\=10 test")))

  (it "Forwards any configured options when running tests"
    ;; GIVEN
    (let ((projection-cmake-ctest-options '("-foo" "-bar"))
          (projection-cmake-ctest-environment-variables))
      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-commands-test-project
       "ctest --test-dir build -foo -bar test")))

  (describe "Clear build directory"
    (it "Fails when no build directory configured"
      ;; GIVEN
      (let* ((projection-cmake-build-directory nil)
             ;; WHEN
             (err (should-error (projection-cmake-clear-build-directory))))
        ;; THEN
        (expect (cadr err) :to-match
                "Unable to determine directory to remove")))

    (it "Fails build directory does not exist"
      ;; GIVEN/WHEN
      (let* ((err (should-error (projection-cmake-clear-build-directory))))
        ;; THEN
        (expect (cadr err) :to-match
                "Directory .* already does not exist")))

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

  (describe "CMake file API"
    :var ((expected-targets '("cmake:all" "cmake:clean" "cmake:main_lib" "cmake:main" "cmake:install")))

    (it "Skips setup when build directory is unset"
      (let ((projection-cmake-build-directory nil))
        (call-interactively #'projection-commands-configure-project)))

    (it "Constructs a CMake query file while configuring"
      ;; WHEN
      (+expect-interactive-command-calls-compile-with
       #'projection-commands-configure-project
       "cmake -S . -B build")

      ;; THEN
      (expect (file-expand-wildcards "build/.cmake/api/v1/query/*/query.json")
              :not :to-be nil))

    (it "Does not throw an error on build directory missing reply for projection"
      ;; GIVEN
      (let ((projection-cmake--file-api-client "alternate-client"))
        (call-interactively #'projection-commands-configure-project))

      ;; WHEN/THEN
      (expect (projection-multi-cmake-targets) :not :to-throw))

    (it "Gracefully handles incomplete JSON file response"
      ;; GIVEN
      (let ((projection-cmake--file-api-client "alternate-client"))
        (call-interactively #'projection-commands-configure-project))

      (f-write "{" 'utf-8
               (f-join (projection-cmake--build-directory 'expand)
                       (projection-cmake--file-api-reply-directory-suffix)
                       "broken-reply.json"))

      ;; WHEN/THEN
      (expect (projection-multi-cmake-targets) :not :to-throw))

    (it "Extracts CMake targets from the code-model"
      ;; GIVEN
      (call-interactively #'projection-commands-configure-project)

      ;; WHEN
      (let ((cmake-targets (mapcar #'car (projection-multi-cmake-targets))))
        ;; THEN
        (expect cmake-targets :to-have-same-items-as expected-targets)))

    (it "Filters out targets matching the configured regexp"
      ;; GIVEN
      (call-interactively #'projection-commands-configure-project)

      ;; WHEN
      (let* ((projection-multi-cmake-exclude-targets (rx bol "main_lib" eol))
             (cmake-targets (mapcar #'car (projection-multi-cmake-targets))))
        ;; THEN
        (expect "main_lib" :not :to-be-in cmake-targets)))

    (describe "Artifacts"
      (it "Can query CMake and CTest artifacts"
        ;; GIVEN
        (call-interactively #'projection-commands-configure-project)

        (+with-completing-read-default-return
          ;; WHEN
          (call-interactively 'projection-artifacts-list)

          ;; THEN
          (expect (+completion-table-candidates
                   (spy-calls-args-for 'completing-read 0))
                  :to-equal '("CMake executable: main"
                              "CMake library: libmain_lib.a"
                              "CTest: main-test"
                              "CTest: hidden")))))

    (describe "Dape"
      (before-each
        (spy-on #'dape)
        ;; Assume all debuggers are available.
        (spy-on #'dape--config-ensure :and-return-value t))
      (before-each
        (call-interactively #'projection-commands-configure-project))

      (it "Prompts with only debuggable CMake and CTest artifacts"
        ;; GIVEN
        (spy-on #'completing-read :and-call-fake
                (+fake-completing-read "CTest: main-test" "gdb"))

        ;; WHEN
        (call-interactively #'projection-dape)

        ;; THEN
        (expect (+completion-table-candidates
                 (spy-calls-args-for 'completing-read 0))
                :to-equal '("CMake executable: main"
                            "CTest: main-test"
                            "CTest: hidden")))

      (it "Can debug a CTest test"
        ;; GIVEN
        (spy-on #'completing-read :and-call-fake
                (+fake-completing-read "CTest: main-test" "gdb"))

        ;; WHEN
        (call-interactively #'projection-dape)

        ;; THEN
        (expect 'dape :to-have-been-called-times 1)
        (let ((dape-config (car (spy-calls-args-for 'dape 0))))
          (expect (plist-get dape-config 'command) :to-equal "gdb")
          (expect (plist-get dape-config :program) :to-match "true")
          (expect (plist-get dape-config :args) :to-equal ["main-test-arg"])
          (expect (plist-get dape-config :environment) :to-equal '(:FOO "1"))
          (expect (plist-get dape-config :cwd) :to-equal
                  (f-join default-directory projection-cmake-build-directory))))

      (it "Can debug a CMake executable"
        ;; GIVEN
        (spy-on #'completing-read :and-call-fake
                (+fake-completing-read "CMake executable: main" "gdb"))

        ;; WHEN
        (call-interactively #'projection-dape)

        ;; THEN
        (expect 'dape :to-have-been-called-times 1)
        (let ((dape-config (car (spy-calls-args-for 'dape 0))))
          (expect (plist-get dape-config 'command) :to-equal "gdb")
          (expect (plist-get dape-config :program) :to-match "main")
          (expect (plist-get dape-config :cwd) :to-be nil)))
      )
    )

  (describe "With a CMake presets configuration"
    :var ((configure-presets '("configurePreset1" "configurePreset2" "configurePreset3"))
          (configure-presets-display-names '("Preset number 1 for configuring"
                                             "Preset number 2 for configuring"))
          (build-presets-for-configure-preset-1 '("buildForConfigurePreset1-Debug"
                                                  "buildForConfigurePreset1-Release"))
          (build-presets-for-configure-preset-2 '("buildForConfigurePreset2-Debug")))
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
      \"displayName\": \"Preset number 1 for configuring\",
      \"description\": \"Default build using Ninja Multi-Config generator\",
      \"generator\": \"Ninja Multi-Config\",
      \"cacheVariables\": {
        \"CMAKE_CONFIGURATION_TYPES\": \"Debug;Release;MinSizeRel;RelWithDebInfo\"
      }
    },
    {
      \"name\": \"configurePreset2\",
      \"displayName\": \"Preset number 2 for configuring\"
    },
    {
      \"name\": \"windowsOnlyPreset\",
      \"displayName\": \"Windows only preset\",
      \"condition\": {
        \"type\": \"equals\",
        \"lhs\": \"${hostSystemName}\",
        \"rhs\": \"Windows\"
      }
    }
  ],
  \"buildPresets\": [
    {
      \"name\": \"buildForConfigurePreset1-Debug\",
      \"configurePreset\": \"configurePreset1\",
      \"configuration\": \"Debug\"
    },
    {
      \"name\": \"buildForConfigurePreset1-Release\",
      \"configurePreset\": \"configurePreset1\",
      \"configuration\": \"Release\"
    },
    {
      \"name\": \"buildForConfigurePreset2-Debug\",
      \"configurePreset\": \"configurePreset2\",
      \"configuration\": \"Debug\"
    }
  ],
  \"testPresets\": [
    {
      \"name\": \"excludeHidden\",
      \"hidden\": true,
      \"filter\": {
        \"exclude\": {
          \"name\": \"hidden\"
        }
      }
    },
    {
      \"name\": \"testForConfigurePreset1-Debug\",
      \"displayName\": \"Default\",
      \"configuration\": \"Debug\",
      \"configurePreset\": \"configurePreset1\",
      \"inherits\": [\"excludeHidden\"]
    },
    {
      \"name\": \"testForConfigurePreset1WithHiddenTests-Debug\",
      \"displayName\": \"WithHidden\",
      \"configurePreset\": \"configurePreset1\",
      \"configuration\": \"Debug\"
    },
    {
      \"name\": \"testForConfigurePreset1-Release\",
      \"displayName\": \"Default\",
      \"configuration\": \"Release\",
      \"configurePreset\": \"configurePreset1\",
      \"inherits\": [\"excludeHidden\"]
    },
    {
      \"name\": \"testForConfigurePreset1WithHiddenTests-Release\",
      \"displayName\": \"WithHidden\",
      \"configurePreset\": \"configurePreset1\",
      \"configuration\": \"Release\"
    },
    {
      \"name\": \"testForConfigurePreset2\",
      \"configurePreset\": \"configurePreset2\",
      \"inherits\": [\"excludeHidden\"]
    }
  ],
  \"workflowPresets\": [
    {
      \"name\": \"default\",
      \"steps\": [
        {
          \"type\": \"configure\",
          \"name\": \"configurePreset2\"
        },
        {
          \"type\": \"build\",
          \"name\": \"buildForConfigurePreset2-Debug\"
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

    (it "Can includes presets failing condition checks"
      (let ((projection-cmake-respect-preset-conditions nil))
        (spy-on #'completing-read :and-return-value "Preset number 1 for configuring")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-commands-configure-project
         "cmake -S . -B build --preset\\=configurePreset1")

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)
        (expect (+completion-table-candidates
                 (spy-calls-args-for 'completing-read 0))
                :to-contain "Windows only preset")))

    (it "Prompts for the current projects preset interactively"
      ;; GIVEN
      (let ((projection-cmake-preset 'prompt-once))
        (spy-on #'completing-read :and-return-value "Preset number 1 for configuring")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-commands-configure-project
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
         #'projection-commands-configure-project
         "cmake -S . -B build --preset\\=configurePreset1")

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)
        (expect (+completion-table-candidates
                 (spy-calls-args-for 'completing-read 0))
                :to-equal configure-presets-display-names)

        ;; WHEN/THEN
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-commands-configure-project
          "cmake -S . -B build --preset\\=configurePreset1"))

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 0)

        ;; GIVEN
        (spy-on #'completing-read :and-return-value "buildForConfigurePreset1-Debug")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-commands-build-project
         "cmake --build build --preset\\=buildForConfigurePreset1-Debug")

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)
        (expect (+completion-table-candidates
                 (spy-calls-args-for 'completing-read 0))
                :to-equal build-presets-for-configure-preset-1)))

    (it "Doesn't prompt for a preset when one was set interactively"
      ;; GIVEN
      (+interactively-set-cmake-preset 'configure "Preset number 1 for configuring")

      ;; WHEN/THEN
      (+with-completing-read-not-called
       (+expect-interactive-command-calls-compile-with
        #'projection-commands-configure-project
        "cmake -S . -B build --preset\\=configurePreset1")))

    (it "Never uses a preset when configured"
      ;; GIVEN
      (let ((projection-cmake-preset 'disable))
        ;; WHEN/THEN
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-commands-configure-project
          "cmake -S . -B build"))))

    (it "Always prompts for a preset when configured"
      ;; GIVEN
      (let ((projection-cmake-preset 'prompt-always))
        (spy-on #'completing-read :and-return-value "Preset number 1 for configuring")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-commands-configure-project
         "cmake -S . -B build --preset\\=configurePreset1")

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)

        ;; GIVEN
        (spy-on #'completing-read :and-return-value "Preset number 2 for configuring")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-commands-configure-project
         "cmake -S . -B build --preset\\=configurePreset2")

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)))

    (it "Never prompts for a preset when configured"
      ;; GIVEN
      (let ((projection-cmake-preset 'silent))
        ;; WHEN/THEN
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-commands-configure-project
          "cmake -S . -B build"))

        ;; GIVEN
        (+interactively-set-cmake-preset 'configure "Preset number 1 for configuring")

        ;; WHEN/THEN
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-commands-configure-project
          "cmake -S . -B build --preset\\=configurePreset1"))))

    (it "Prompts and saves chosen preset for build-type after first invocation only when multiple presets are available"
      ;; GIVEN
      (let ((projection-cmake-preset 'prompt-once-when-multiple))
        ;; GIVEN
        (spy-on #'completing-read :and-return-value "Preset number 2 for configuring")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-commands-configure-project
         "cmake -S . -B build --preset\\=configurePreset2")

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)

        ;; WHEN/THEN
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-commands-build-project
          "cmake --build build --preset\\=buildForConfigurePreset2-Debug"))

        ;; WHEN/THEN
        (+with-completing-read-not-called
         (+expect-interactive-command-calls-compile-with
          #'projection-commands-configure-project
          "cmake -S . -B build --preset\\=configurePreset2"))))

    (it "Prompts with only related targets matching the active configuration target for building and testing"
      ;; GIVEN
      (+interactively-set-cmake-preset 'configure "Preset number 1 for configuring")
      (spy-on #'completing-read :and-return-value "Default")

      ;; WHEN/THEN
      (+expect-interactive-command-calls-compile-with
       #'projection-commands-test-project
       "ctest --test-dir build --preset\\=testForConfigurePreset1-Debug test")

      ;; THEN
      (expect #'completing-read :to-have-been-called-times 1)
      (expect (+completion-table-candidates
               (spy-calls-args-for 'completing-read 0))
              :to-equal '("Default"
                          "WithHidden"
                          "Default"
                          "WithHidden")))

    (it "Clears cached test preset when re-setting a configure preset"
      (+interactively-set-cmake-preset 'configure "Preset number 2 for configuring")
      (+interactively-set-cmake-preset 'test "testForConfigurePreset2")
      (+interactively-set-cmake-preset 'test "testForConfigurePreset2")
      (+interactively-set-cmake-preset 'configure "Preset number 1 for configuring")

      ;; WHEN/THEN
      (spy-on #'completing-read :and-return-value "Default")
      (+expect-interactive-command-calls-compile-with
       #'projection-commands-test-project
       "ctest --test-dir build --preset\\=testForConfigurePreset1-Debug test"))

    (it "Ignores build or test preset when active configure preset conflicts with it"
      (let ((projection-cmake-preset '((configure . prompt-always)
                                       (test . "testPreset1WithConfigurePreset1"))))
        (+interactively-set-cmake-preset 'configure "Preset number 2 for configuring")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-commands-test-project
         "ctest --test-dir build test")))

    (it "Can configure alternate preset setting on preset invalidation"
      (let ((projection-cmake-preset '((configure . "configurePreset1")
                                       (on-invalid . prompt-once-when-multiple))))
        (+interactively-set-cmake-preset 'configure "Preset number 1 for configuring")
        (+interactively-set-cmake-preset 'test "Default")
        (+interactively-set-cmake-preset 'configure "Preset number 2 for configuring")

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-commands-test-project
         "ctest --test-dir build --preset\\=testForConfigurePreset2 test")
        ))

    (describe "Multi compile"
      (before-all (require 'projection-multi-cmake))

      (it "Automatically invalides CTest target cache on test preset change"
        ;; GIVEN
        (+interactively-set-cmake-preset 'configure "Preset number 1 for configuring")
        (+interactively-set-cmake-preset 'test "Default")
        (call-interactively #'projection-commands-configure-project)

        ;; WHEN
        (let ((ctest-targets (mapcar #'car (projection-multi-ctest-targets))))
          ;; THEN
          (expect "ctest:hidden" :not :to-be-in ctest-targets))

        ;; WHEN
        (+interactively-set-cmake-preset 'test "WithHidden")
        (let ((ctest-targets (mapcar #'car (projection-multi-ctest-targets))))
          ;; THEN
          (expect "ctest:hidden" :to-be-in ctest-targets)))

      (it "Includes targets for any workflow presets"
        ;; GIVEN
        (spy-on #'completing-read :and-return-value "Preset number 1 for configuring")
        (call-interactively #'projection-commands-configure-project)

        ;; WHEN
        (let ((cmake-targets (mapcar #'car (projection-multi-cmake-targets))))
          ;; THEN
          (expect "cmake:workflow:default" :to-be-in cmake-targets)))))
  )
