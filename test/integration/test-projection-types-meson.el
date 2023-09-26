;; -*- lexical-binding: t -*-

(require 'projection-types)
(require 'projection-utils)
(require 'projection-utils-meson)

(require 'projection-test-utils)

(describe "Project type Meson"
  (+projection-test-setup)

  (before-each
    (setq projection-project-types (list projection-project-type-meson))

    (+projection-setup-project
     '(("meson.build" . "project('simple', 'c')
src = ['source1.c', 'source2.c', 'source3.c']
exe = executable('myexe', src)
test('simple test', exe)")
       ("source1.c" . "")
       ("source2.c" . "")
       ("source3.c" . "int main() { return 0; }"))))

  (it "Can be identified"
    (+projection-project-matches-p 'meson))

  (it "Can configure/build/test/install a Meson project"
    (+expect-interactive-command-calls-compile-with
     #'projection-configure-project
     "meson setup builddir")

    (+expect-interactive-command-calls-compile-with
     #'projection-build-project
     "meson compile -C builddir")

    (+expect-interactive-command-calls-compile-with
     #'projection-test-project
     "meson test -C builddir")

    (+expect-interactive-command-calls-compile-with
     #'projection-install-project
     "meson install -C builddir --destdir install"))

  (it "Can reconfigure when build directory already exists"
    ;; GIVEN
    (mkdir (projection-meson--build-directory) 'parents)

    ;; WHEN/THEN
    (+expect-interactive-command-calls-compile-with
     #'projection-configure-project
     "meson setup builddir --reconfigure"))

  (it "Adapts configuring to the configured Meson build directory"
    (let ((projection-meson-build-directory "blarg"))
      (+expect-interactive-command-calls-compile-with
       #'projection-configure-project
       "meson setup blarg")))

  (it "Includes the configured Meson build-type"
    ;; GIVEN
    (+interactively-set-meson-build-type "plain")
    ;; WHEN/THEN
    (+expect-interactive-command-calls-compile-with
     #'projection-configure-project
     "meson setup builddir --buildtype\\=plain"))
  )
