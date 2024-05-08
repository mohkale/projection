;; -*- lexical-binding: t -*-

(require 'projection-types)

(require 'projection-type-golang)
(require 'projection-test-utils)

(describe "Project type Golang"
  (+projection-test-setup)

  (before-each
    (setq projection-project-types (list projection-project-type-golang)))

  (describe "With no module file"
    (before-each
      (+projection-setup-project '("foo.go")))

    (it "Can be identified"
      (+projection-project-matches-p 'golang))

    (it "Treats as single target-package"
      ;; GIVEN
      (let ((projection-golang-package 'prompt-once-when-multiple))
        (spy-on #'completing-read)

        ;; WHEN/THEN
        (+expect-interactive-command-calls-compile-with
         #'projection-commands-build-project
         "go build ./...")

        ;; THEN
        (expect 'completing-read :not :to-have-been-called)))
    )

  (describe "With module file"
    (before-each
      (+projection-setup-project
       '(("go.mod" . "module golang.org/x/tools

go 1.18

require (
	github.com/yuin/goldmark v1.4.13
	golang.org/x/mod v0.12.0
	golang.org/x/net v0.15.0
	golang.org/x/sys v0.12.0
)

require golang.org/x/sync v0.3.0")
         ("go.sum" . "github.com/yuin/goldmark v1.4.13 h1:fVcFKWvrslecOb/tg+Cc05dkeYx540o0FuFt3nUVDoE=
github.com/yuin/goldmark v1.4.13/go.mod h1:6yULJ656Px+3vBD8DxQVa3kxgyrAnzto9xy5taEt/CY=
golang.org/x/mod v0.12.0 h1:rmsUpXtvNzj340zd98LZ4KntptpfRHwpFOHG188oHXc=
golang.org/x/mod v0.12.0/go.mod h1:iBbtSCu2XBx23ZKBPSOrRkjjQPZFPuis4dIYUhu/chs=
golang.org/x/net v0.15.0 h1:ugBLEUaxABaB5AJqW9enI0ACdci2RUd4eP51NTBvuJ8=
golang.org/x/net v0.15.0/go.mod h1:idbUs1IY1+zTqbi8yxTbhexhEEk5ur9LInksu6HrEpk=
golang.org/x/sync v0.3.0 h1:ftCYgMx6zT/asHUrPw8BLLscYtGznsLAnjq5RH9P66E=
golang.org/x/sync v0.3.0/go.mod h1:FU7BRWz2tNW+3quACPkgCx/L+uEAv1htQ0V83Z9Rj+Y=
golang.org/x/sys v0.12.0 h1:CM0HF96J0hcLAwsHPJZjfdNzs0gftsLfgKt57wWHJ0o=
golang.org/x/sys v0.12.0/go.mod h1:oPkhp1MJrh7nUepCBck5+mAzfO9JrbApNNgaTdGDITg="))))

    (it "Can be identified"
      (+projection-project-matches-p 'golang))

    (describe "With single package project"
      (before-each
        (+projection-setup-project-tree
         '(("foo" ("foo.go" . "package foo")))))

      (it "Can avoid prompting for target-package"
        ;; GIVEN
        (let ((projection-golang-package 'prompt-once-when-multiple))
          (spy-on #'completing-read)

          ;; WHEN/THEN
          (+expect-interactive-command-calls-compile-with
           #'projection-commands-build-project
           "go build golang.org/x/tools/foo")

          ;; THEN
          (expect 'completing-read :not :to-have-been-called))))

    (describe "With multi package project"
      :var ((golang-packages '("*All*" "bar/baz" "foo")))

      (before-each
        (+projection-setup-project-tree
         '(("foo" ("foo.go" . "package foo"))
           ("bar" ("baz" ("bag.go" . "package baz"))))))

      (it "Supports setting target-package to all packages recursively"
        ;; GIVEN
        (let ((projection-golang-package 'all))
          ;; WHEN/THEN
          (+expect-interactive-command-calls-compile-with
           #'projection-commands-build-project
           "go build ./...")))

      (it "Supports omitting a target-package"
        ;; GIVEN
        (let ((projection-golang-package nil))
          ;; WHEN/THEN
          (+expect-interactive-command-calls-compile-with
           #'projection-commands-build-project
           "go build")))

      (it "Supports setting a target-package explicitly"
        ;; GIVEN
        (let ((projection-golang-package "foobar"))
          ;; WHEN/THEN
          (+expect-interactive-command-calls-compile-with
           #'projection-commands-build-project
           "go build foobar")))

      (it "Allows prompting for a target-package"
        ;; GIVEN
        (let ((projection-golang-package 'prompt-once))
          (spy-on #'completing-read :and-return-value "foo")

          ;; WHEN/THEN
          (+expect-interactive-command-calls-compile-with
           #'projection-commands-build-project
           "go build golang.org/x/tools/foo")

          ;; THEN
          (expect 'completing-read :to-have-been-called-times 1)
          (expect (+completion-table-candidates
                   (spy-calls-args-for 'completing-read 0))
                  :to-equal golang-packages)))

      (it "Allows prompting for a target-package in a multi-package projects"
        ;; GIVEN
        (let ((projection-golang-package 'prompt-once))
          (spy-on #'completing-read :and-return-value "foo")

          ;; WHEN/THEN
          (+expect-interactive-command-calls-compile-with
           #'projection-commands-build-project
           "go build golang.org/x/tools/foo")

          ;; THEN
          (expect 'completing-read :to-have-been-called-times 1)
          (expect (+completion-table-candidates
                   (spy-calls-args-for 'completing-read 0))
                  :to-equal golang-packages)))

      (it "Allows interactively amending the target-package"
        ;; GIVEN
        (spy-on #'completing-read :and-return-value "foo")

        ;; WHEN
        (call-interactively #'projection-golang-set-package)

        ;; THEN
        (expect 'completing-read :to-have-been-called-times 1)
        (expect (+completion-table-candidates
                 (spy-calls-args-for 'completing-read 0))
                :to-equal golang-packages)))))
