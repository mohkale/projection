;; -*- lexical-binding: t -*-

(require 'projection-utils-cmake)

(defun fake-completing-read (&rest return-values)
  (let ((count 0))
    (lambda (&rest _)
      (prog1 (nth (min (1- (length return-values)) count)
                  return-values)
        (setq count (1+ count))))))

(defun interactively-set-cmake-preset (build-type preset)
  (let ((call-list
         (if build-type
             (list (symbol-name build-type) preset)
           (list preset))))
    (spy-on #'completing-read :and-call-fake
            (apply #'fake-completing-read call-list))

    (let ((current-prefix-arg (unless build-type
                                '(4))))
      (call-interactively #'projection-cmake-set-preset))

    (expect 'completing-read :to-have-been-called-times (length call-list))))

(defun interactively-set-cmake-build-type (build-type)
  (spy-on #'completing-read :and-return-value build-type)

  (call-interactively #'projection-cmake-set-build-type)

  (expect 'completing-read :to-have-been-called-times 1))

(defun interactively-call-compile-command (command shell-command)
  (spy-on #'compile)

  (call-interactively command)

  (expect 'compile :to-have-been-called-times 1)
  (expect 'compile :to-have-been-called-with shell-command))

(defun get-completion-table-candidates (call-args)
  (let ((completion-table (cadr call-args)))
    (funcall completion-table "" nil t)))

(defmacro with-completing-read-not-called (&rest body)
  (declare (indent defun))
  `(progn
    (spy-on #'completing-read)
    ,@body
    (expect 'completing-read :to-have-been-called-times 0)))

(describe "Projection CMake project"
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

  (before-each
    (setq projection-project-types
          (list
           (projection-type
            :name 'cmake
            :predicate "CMakeLists.txt"
            :configure #'projection-cmake-run-configure
            :build     #'projection-cmake-run-build
            :test      #'projection-cmake-run-test
            :install   #'projection-cmake-run-install
            :package   #'projection-cmake-run-package)))

    ;; Setup a simple CMake project.
    (projection-find-test--setup-project
     '(("main.cpp" . "#include <stdio.h>

int main() {
  printf(\"Hello world\\n\");
}")
       ("CMakeLists.txt" . "cmake_minimum_required(VERSION 3.2)
project(projection_test)
add_executable(main main.cpp)
"))))

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
  ]
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
      ;;   * The user was not prompted for a configure preset to use.
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
      (interactively-set-cmake-preset nil "defaultPreset")

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

        (expect 'completing-read :to-have-been-called-times 1)))))
