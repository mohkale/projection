;; -*- lexical-binding: t -*-

(require 'projection-types)
(require 'projection-multi-haskell-stack)

(require 'projection-test-utils)

(describe "Project type Haskell Stack"
  (+projection-test-setup)

  (before-each
    (setq projection-project-types (list projection-project-type-haskell-stack))

    (+projection-setup-project
     '(("stack.yaml" . "resolver: lts-24.9
packages:
- .")
       ("package.yaml" . "name: test-library
version: 0.0.0.1
author: Joe Smith
synopsis: Test package
library:
  source-dirs: lib
executables:
  exe-one:
    source-dirs: exe1
    main: Main.hs
  exe-two:
    source-dirs: exe2
    main: Exe2.hs
    dependencies:
      - base
      - test-library
tests:
  spec:
    source-dirs:
      - lib
      - test
    main: Spec.hs

benchmarks:
  bench-lib:
    source-dirs: bench/lib
    main: Main.hs
  bench-exe:
    source-dirs: bench/exe
    main: Main.hs"))))

  (it "Can be identified"
    (+projection-project-matches-p 'haskell-stack))

  (describe "Multi compile"
    (it "Can extract available targets"
      ;; WHEN
      (let ((targets (projection-multi-haskell-stack-targets)))
        ;; THEN
        (expect targets :to-equal
                '(("haskell-stack:test-library"
                   :command "stack build test-library"
                   :annotation "Package: Build")
                  ("haskell-stack:test-library:test"
                   :command "stack test test-library"
                   :annotation "Package: Run test suite")
                  ("haskell-stack:test-library:bench"
                   :command "stack bench test-library"
                   :annotation "Package: Run benchmarks")
                  ("haskell-stack:test-library:haddock"
                   :command "stack haddock test-library"
                   :annotation "Package: Build haddocks")
                  ("haskell-stack:test-library:ghci"
                   :command "stack ghci test-library"
                   :annotation "Package: Open ghci")
                  ("haskell-stack:test-library:clean"
                   :command "stack clean test-library"
                   :annotation "Package: Clean artifacts")
                  ("haskell-stack:test-library:lib"
                   :command "stack build test-library\\:lib --no-run-tests --no-run-benchmarks"
                   :annotation "Library: Build")
                  ("haskell-stack:test-library:exe:exe-one"
                   :command "stack build test-library\\:exe\\:exe-one --no-run-tests --no-run-benchmarks"
                   :annotation "Executable: Build")
                  ("haskell-stack:test-library:exe:exe-one:run"
                   :command "stack run test-library\\:exe\\:exe-one"
                   :annotation "Executable: Run")
                  ("haskell-stack:test-library:exe:exe-two"
                   :command "stack build test-library\\:exe\\:exe-two --no-run-tests --no-run-benchmarks"
                   :annotation "Executable: Build")
                  ("haskell-stack:test-library:exe:exe-two:run"
                   :command "stack run test-library\\:exe\\:exe-two"
                   :annotation "Executable: Run")
                  ("haskell-stack:test-library:test:spec"
                   :command "stack build test-library\\:test\\:spec --run-tests"
                   :annotation "Test suite: Build and run")
                  ("haskell-stack:test-library:bench:bench-exe"
                   :command "stack build test-library\\:bench\\:bench-exe --run-benchmarks"
                   :annotation "Benchmark: Build and run")
                  ("haskell-stack:test-library:bench:bench-lib"
                   :command "stack build test-library\\:bench\\:bench-lib --run-benchmarks"
                   :annotation "Benchmark: Build and run")))))))
