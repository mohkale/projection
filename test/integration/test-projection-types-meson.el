;; -*- lexical-binding: t -*-

(require 'projection-types)
(require 'projection-utils)
(require 'projection-utils-meson)
(require 'projection-multi-meson)

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

  (describe "Multi compile"
    (it "Can extract available build targets"
      ;; GIVEN
      (call-interactively #'projection-configure-project)
      ;; WHEN
      (let ((targets (projection-multi-meson-targets)))
        ;; THEN
        (expect targets :to-equal
                '(("meson:clean" . "meson compile -C builddir --clean")
                  ("meson:myexe" . "meson compile -C builddir myexe")
                  ("meson:test:simple test" . "meson test -C builddir -- simple\\ test")))))
    )

  (describe "Set build-options"
    :var ((expected-build-options '("auto_features" "backend" "buildtype" "debug"
                                    "default_library" "install_umask" "layout"
                                    "optimization" "prefer_static" "strip"
                                    "unity" "unity_size" "warning_level"
                                    "werror" "wrap_mode" "pkgconfig.relocatable"
                                    "python.install_env" "python.platlibdir"
                                    "python.purelibdir" "backend_max_links"
                                    "b_asneeded" "b_colorout" "b_coverage"
                                    "b_lto" "b_lto_threads" "b_lundef"
                                    "b_ndebug" "b_pch" "b_pgo" "b_pie"
                                    "b_sanitize" "b_staticpic" "build.c_std"
                                    "c_std" "bindir" "datadir" "includedir"
                                    "infodir" "libdir" "libexecdir" "localedir"
                                    "localstatedir" "mandir" "prefix" "sbindir"
                                    "sharedstatedir" "sysconfdir" "errorlogs"
                                    "stdsplit")))
    (before-each
      (spy-on #'projection--shell-command)
      (call-interactively #'projection-configure-project))

    (it "Can query available build options for the current project"
      ;; GIVEN
      (spy-on #'completing-read :and-call-fake (+fake-completing-read "debug"))
      (spy-on #'yes-or-no-p :and-return-value t)

      ;; WHEN
      (call-interactively #'projection-meson-set-build-option)

      ;; THEN
      (expect 'completing-read :to-have-been-called-times 1)
      (let ((actual-build-options (+completion-table-candidates
                                   (spy-calls-args-for 'completing-read 0))))
        (dolist (build-option expected-build-options)
          (expect actual-build-options :to-contain build-option))))

    (it "Allows you to set a boolean build-option to true"
      ;; GIVEN
      (spy-on #'completing-read :and-call-fake (+fake-completing-read "debug"))
      (spy-on #'yes-or-no-p :and-return-value t)

      ;; WHEN
      (call-interactively #'projection-meson-set-build-option)

      ;; THEN
      (expect 'projection--shell-command :to-have-been-called-times 1)
      (expect (cadr (spy-calls-args-for 'projection--shell-command 0))
              :to-equal "meson configure builddir -D debug\\=true"))

    (it "Allows you to set a boolean build-option to false"
      ;; GIVEN
      (spy-on #'completing-read :and-call-fake (+fake-completing-read "debug"))
      (spy-on #'yes-or-no-p :and-return-value nil)

      ;; WHEN
      (call-interactively #'projection-meson-set-build-option)

      ;; THEN
      (expect 'projection--shell-command :to-have-been-called-times 1)
      (expect (cadr (spy-calls-args-for 'projection--shell-command 0))
              :to-equal "meson configure builddir -D debug\\=false"))

    (it "Allows you to set a numerical build-option"
      ;; GIVEN
      (spy-on #'completing-read :and-call-fake (+fake-completing-read "backend_max_links"))
      (spy-on #'read-number :and-return-value 5)

      ;; WHEN
      (call-interactively #'projection-meson-set-build-option)

      ;; THEN
      (expect 'projection--shell-command :to-have-been-called-times 1)
      (expect (cadr (spy-calls-args-for 'projection--shell-command 0))
              :to-equal "meson configure builddir -D backend_max_links\\=5"))

    (it "Allows you to set a string build-option"
      ;; GIVEN
      (spy-on #'completing-read :and-call-fake (+fake-completing-read "prefix"))
      (spy-on #'read-string :and-return-value "foobar")

      ;; WHEN
      (call-interactively #'projection-meson-set-build-option)

      ;; THEN
      (expect 'projection--shell-command :to-have-been-called-times 1)
      (expect (cadr (spy-calls-args-for 'projection--shell-command 0))
              :to-equal "meson configure builddir -D prefix\\=foobar"))

    (it "Allows you to set a multi-choice build-option"
      ;; GIVEN
      (spy-on #'completing-read :and-call-fake
              (+fake-completing-read "backend" "ninja"))

      ;; WHEN
      (call-interactively #'projection-meson-set-build-option)

      ;; THEN
      (expect 'completing-read :to-have-been-called-times 2)
      (expect (+completion-table-candidates (spy-calls-args-for 'completing-read 1))
              :to-equal '("ninja" "vs" "vs2010" "vs2012" "vs2013"
                          "vs2015" "vs2017" "vs2019" "vs2022" "xcode"))

      (expect 'projection--shell-command :to-have-been-called-times 1)
      (expect (cadr (spy-calls-args-for 'projection--shell-command 0))
              :to-equal "meson configure builddir -D backend\\=ninja"))
    )
  )
