;;; projection-types.el --- Built-in project type definitions for `projection'. -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mohsin Kaleem

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module registers a collection of standard project types for use with
;; `projection'.

;;; Code:

(require 'projection-core)
(require 'projection-utils)

(defgroup projection-types nil
  "Projection project type definitions."
  :group 'projection)

;; NOTE: Project type detection happens in reverse order to registration. As
;; function based project type detection is considerably slower than simple
;; file based matching, such project types are defined near the top of this
;; file and the remaining near the end. Ideally common project types should
;; be checked earlier than exotic ones.



(defvar projection-project-type-haskell-cabal
  (projection-type
   :name 'haskell-cabal
   :predicate (defun projection-haskell-cabal-project-p ()
                (and (file-expand-wildcards "?*.cabal")
                     (not (file-exists-p "stack.yml"))))
   :build "cabal build"
   :test  "cabal test"
   :run   "cabal run"
   :test-suffix "Spec"))

(add-to-list 'projection-project-types projection-project-type-haskell-cabal 'append)



(defvar projection-project-type-dotnet
  (projection-type
   :name 'dotnet
   :predicate (defun projection-dotnet-project-p ()
                (or (file-expand-wildcards "?*.csproj")
                    (file-expand-wildcards "?*.fsproj")
                    (file-expand-wildcards "?*.sln")))
   :build "dotnet build"
   :test  "dotnet run"
   :run   "dotnet test"))

(add-to-list 'projection-project-types projection-project-type-dotnet 'append)



(defvar projection-project-type-nim-nimble
  (projection-type
   :name 'nim-nimble
   :predicate (defun projection-nimble-project-p ()
                (file-expand-wildcards "?*.nimble"))
   :build   "nimble --noColor build --colors:off"
   :test    "nimble --noColor test -d:nimUnittestColor:off --colors:off"
   :install "nimble --noColor install --colors:off"
   :run     "nimble --noColor run --colors:off"
   :src-dir "src"
   :test-dir "tests"))

(add-to-list 'projection-project-types projection-project-type-nim-nimble 'append)



;; Go should take higher precedence than Make because Go projects often have a
;; Makefile.

(autoload 'projection-golang-run-build "projection-type-golang")
(autoload 'projection-golang-run-run   "projection-type-golang")
(autoload 'projection-golang-run-test  "projection-type-golang")
(autoload 'projection-golang-list-artifacts "projection-type-golang")

(defvar projection-project-type-golang
  (projection-type
   :name 'golang
   :predicate (defun projection-golang-project-p ()
                (or (file-exists-p "go.work")
                    (file-exists-p "go.mod")
                    (file-expand-wildcards "*.go")))
   :configure "go get"
   :build #'projection-golang-run-build
   :test #'projection-golang-run-test
   :run #'projection-golang-run-run
   :test-suffix "_test"
   :compile-multi-targets
   '(("go:mod:tidy" . "go mod tidy")
     ("go:mod:verify" . "go mod verify")
     ("go:mod:why" . "go mod why")
     ("go:fmt:all" . "go fmt ./..."))
   :artifacts-list #'projection-golang-list-artifacts))

(add-to-list 'projection-project-types projection-project-type-golang 'append)



(defvar projection-project-type-go-task
  (projection-type
   :name 'go-task
   :predicate "Taskfile.yml"
   :build "task build"
   :test "task test"
   :install "task install"))

(add-to-list 'projection-project-types projection-project-type-go-task 'append)



(defvar projection-project-type-scons
  (projection-type
   :name 'scons
   :predicate "SConstruct"
   :build "scons"
   :test "scons test"
   :test-suffix "test"))

(add-to-list 'projection-project-types projection-project-type-scons 'append)



(autoload 'projection-meson-get-configure-command "projection-type-meson")
(autoload 'projection-meson-get-build-command     "projection-type-meson")
(autoload 'projection-meson-get-test-command      "projection-type-meson")
(autoload 'projection-meson-get-install-command   "projection-type-meson")

(defvar projection-project-type-meson
  (projection-type
   :name 'meson
   :predicate "meson.build"
   :configure #'projection-meson-get-configure-command
   :build #'projection-meson-get-build-command
   :test #'projection-meson-get-test-command
   :install #'projection-meson-get-install-command))

(add-to-list 'projection-project-types projection-project-type-meson 'append)



(defvar projection-project-type-nix
  (projection-type
   :name 'nix
   :predicate "default.nix"
   :build "nix-build"
   :test  "nix-build"))

(add-to-list 'projection-project-types projection-project-type-nix 'append)



(defvar projection-project-type-nix-flake
  (projection-type
   :name 'nix-flake
   :predicate "flake.nix"
   :build "nix build"
   :test  "nix flake check"
   :run   "nix run"))

(add-to-list 'projection-project-types projection-project-type-nix-flake 'append)



(defvar projection-project-type-bazel
  (projection-type
   :name 'bazel
   :predicate "WORKSPACE"
   :build "bazel build"
   :test  "bazel test"
   :run   "bazel run"))

(add-to-list 'projection-project-types projection-project-type-bazel 'append)



(defvar projection-project-type-debian
  (projection-type
   :name 'debian
   :predicate "debian/control"
   :build "debuild -uc -us"))

(add-to-list 'projection-project-types projection-project-type-debian 'append)



(defvar projection-build-jobs)
(defun projection-make-run-build (&optional target)
  "Build command generator for Make projects.
Set TARGET as the TARGET to build when set."
  (projection--join-shell-command
   `("make"
     ,@(when-let* ((jobs (projection--guess-parallelism projection-build-jobs)))
         (list "-j" (number-to-string jobs)))
     ,@(when target (list target)))))

(defvar projection-project-type-make
  (projection-type
   :name 'make
   :predicate '("Makefile" "GNUMakefile")
   :build   #'projection-make-run-build
   :test    (apply-partially #'projection-make-run-build "test")
   :install "make install"))

(add-to-list 'projection-project-types projection-project-type-make 'append)



(defcustom projection-autotools-configure-options nil
  "Options to pass to autotools configure script."
  :type '(list (repeat (string :tag "Argument")))
  :group 'projection-type-autotools)

(defun projection-autotools-run-configure ()
  "Configure command generator for autotools projects."
  (projection--join-shell-commands
   `(,(if (file-exists-p "autogen.sh")
          "./autogen.sh"
        "autoconf")
     ("./configure" ,@projection-autotools-configure-options))))

(defvar projection-project-type-autotools
  (projection-type
   :name 'autotools
   :predicate '("configure.ac" "configure.in")
   :configure #'projection-autotools-run-configure
   :build     "make"
   :install   "make install"))

(add-to-list 'projection-project-types projection-project-type-autotools 'append)



(autoload 'projection-cmake-run-configure "projection-type-cmake")
(autoload 'projection-cmake-run-build     "projection-type-cmake")
(autoload 'projection-cmake-run-test      "projection-type-cmake")
(autoload 'projection-cmake-run-install   "projection-type-cmake")
(autoload 'projection-cmake-clear-build-directory "projection-type-cmake")
(autoload 'projection-cmake-list-artifacts "projection-type-cmake")
(autoload 'projection-ctest-list-artifacts "projection-type-cmake")

(defvar projection-project-type-cmake
  (projection-type
   :name 'cmake
   :predicate "CMakeLists.txt"
   :configure #'projection-cmake-run-configure
   :build     #'projection-cmake-run-build
   :test      #'projection-cmake-run-test
   :install   #'projection-cmake-run-install
   :artifacts-list (list #'projection-cmake-list-artifacts
                         #'projection-ctest-list-artifacts)
   :test-suffix '(".t" ".g")
   :compile-multi-targets
   `(("cmake:clear" . ,#'projection-cmake-clear-build-directory))))

(add-to-list 'projection-project-types projection-project-type-cmake 'append)



(defvar projection-project-type-php-symfony
  (projection-type
   :name 'php-symfony
   :predicate (projection--all-files-exists "composer.json" "app" "src" "vendor")
   :build "app/console server:run"
   :test "phpunit -c app"
   :test-suffix "Test"))

(add-to-list 'projection-project-types projection-project-type-php-symfony 'append)



(defvar projection-project-type-rebar
  (projection-type
   :name 'rebar
   :predicate "rebar.config"
   :build "rebar3 compile"
   :test "rebar3 do eunit,ct"
   :test-suffix "_SUITE"))

(add-to-list 'projection-project-types projection-project-type-rebar 'append)



(defvar projection-project-type-elixir
  (projection-type
   :name 'elixir
   :predicate "mix.exs"
   :build "mix compile"
   :test "mix test"
   :test-suffix "_test"))

(add-to-list 'projection-project-types projection-project-type-elixir 'append)



(defvar projection-project-type-grunt
  (projection-type
   :name 'grunt
   :predicate "Gruntfile.js"
   :build "grunt"
   :test "grunt test"))

(add-to-list 'projection-project-types projection-project-type-grunt 'append)



(defvar projection-project-type-gulp
  (projection-type
   :name 'gulp
   :predicate "gulpfile.js"
   :build "gulp"
   :test "gulp test"))

(add-to-list 'projection-project-types projection-project-type-gulp 'append)



(defvar projection-project-type-yarn
  (projection-type
   :name 'yarn
   :predicate (projection--all-files-exists "package.json" "yarn.lock")
   :configure "yarn install"
   :build "yarn build"
   :test "yarn test"
   :test-suffix ".test"))

(add-to-list 'projection-project-types projection-project-type-yarn 'append)



(defvar projection-project-type-pnpm
  (projection-type
   :name 'pnpm
   :predicate (projection--all-files-exists "package.json" "pnpm-lock.yaml")
   :configure "pnpm install"
   :build "pnpm build"
   :test "pnpm test"
   :test-suffix ".test"))

(add-to-list 'projection-project-types projection-project-type-pnpm 'append)



(defvar projection-project-type-npm
  (projection-type
   :name 'npm
   :predicate (projection--all-files-exists "package.json" "package-lock.json")
   :configure "npm install"
   :test "npm test"
   :test-suffix ".test"))

(add-to-list 'projection-project-types projection-project-type-npm 'append)



(defvar projection-project-type-angular
  (projection-type
   :name 'angular
   :predicate (projection--all-files-exists "angular.json" ".angular-cli.json")
   :build "ng build"
   :run "ng serve"
   :test "ng test"
   :test-suffix ".spec"))

(add-to-list 'projection-project-types projection-project-type-angular 'append)



(defvar projection-project-type-django
  (projection-type
   :name 'django
   :predicate "manage.py"
   :build "python manage.py runserver"
   :test "python manage.py test"
   :test-prefix "test_"
   :test-suffix "_test"))

(add-to-list 'projection-project-types projection-project-type-django 'append)



(defvar projection-project-type-python-pip
  (projection-type
   :name 'python-pip
   :predicate "requirements.txt"
   :build "python setup.py build"
   :test "python -m unittest discover"
   :test-prefix "test_"
   :test-suffix "_test"
   :compile-multi-targets
   '(("python:venv:init" . "python3 -m venv init .venv")
     ("python:venv:clear" . "rm -rf .venv")
     ("python:venv:install" . ".venv/bin/pip install -r requirements.txt"))))

(add-to-list 'projection-project-types projection-project-type-python-pip 'append)



(defvar projection-project-type-python-pkg
  (projection-type
   :name 'python-pkg
   :predicate "setup.py"
   :build "python setup.py build"
   :test "python -m unittest discover"
   :test-prefix "test_"
   :test-suffix "_test"))

(add-to-list 'projection-project-types projection-project-type-python-pkg 'append)



(defvar projection-project-type-python-toml
  (projection-type
   :name 'python-toml
   :predicate "pyproject.toml"
   :build "python -m build"
   :test "python -m unittest discover"
   :test-prefix "test_"
   :test-suffix "_test"))

(add-to-list 'projection-project-types projection-project-type-python-toml 'append)



;;;###autoload (autoload 'projection-tox-clear-work-directory "projection-types" nil 'interactive)
(defalias 'projection-tox-clear-work-directory
  (projection--create-clear-directory-command ".tox"))

(defvar projection-project-type-python-tox
  (projection-type
   :name 'python-tox
   :predicate "tox.ini"
   :build "tox -r --notest"
   :test "tox"
   :test-prefix "test_"
   :test-suffix "_test"
   :compile-multi-targets
   `(("tox:clear" . ,#'projection-tox-clear-work-directory))))

(add-to-list 'projection-project-types projection-project-type-python-tox 'append)



(defvar projection-project-type-python-pipenv
  (projection-type
   :name 'python-pipenv
   :predicate "Pipfile"
   :build "pipenv run build"
   :test "pipenv run test"
   :test-prefix "test_"
   :test-suffix "_test"))

(add-to-list 'projection-project-types projection-project-type-python-pipenv 'append)



(defvar projection-project-type-python-poetry
  (projection-type
   :name 'python-poetry
   :predicate "poetry.lock"
   :build "poetry build"
   :test "poetry run python -m unittest discover"
   :test-prefix "test_"
   :test-suffix "_test"))

(add-to-list 'projection-project-types projection-project-type-python-poetry 'append)



(defvar projection-project-type-maven
  (projection-type
   :name 'maven
   :predicate "pom.xml"
   :build "mvn -B clean install"
   :test "mvn -B test"
   :src-dir "src/main/"
   :test-dir "src/test/"))

(add-to-list 'projection-project-types projection-project-type-maven 'append)



(autoload 'projection-gradle-run-build "projection-type-gradle")
(autoload 'projection-gradle-run-test  "projection-type-gradle")

(defvar projection-project-type-gradle
  (projection-type
   :name 'gradle
   :predicate '("build.gradle"
                "build.gradle.kts"
                "gradlew")
   :build #'projection-gradle-run-build
   :test #'projection-gradle-run-test
   :test-suffix "Spec"))

(add-to-list 'projection-project-types projection-project-type-gradle 'append)



(defvar projection-project-type-grails
  (projection-type
   :name 'grails
   :predicate (projection--all-files-exists "application.yml" "grails-app")
   :build "grails package"
   :test "grails test-app"
   :test-suffix "Spec"))

(add-to-list 'projection-project-types projection-project-type-grails 'append)



(defvar projection-project-type-sbt
  (projection-type
   :name 'sbt
   :predicate "build.sbt"
   :build "sbt compile"
   :test "sbt test"
   :src-dir "main"
   :test-dir "test"
   :test-suffix "Spec"))

(add-to-list 'projection-project-types projection-project-type-sbt 'append)



(defvar projection-project-type-mill
  (projection-type
   :name 'mill
   :predicate "build.sc"
   :build "mill __.compile"
   :test "mill __.test"
   :test-suffix "Test"
   :src-dir "src/"
   :test-dir "test/src/"))

(add-to-list 'projection-project-types projection-project-type-mill 'append)



(defvar projection-project-type-bloop
  (projection-type
   :name 'bloop
   :predicate ".bloop/bloop.settings.json"
   :build "bloop compile root"
   :test "bloop test --propagate --reporter scalac root"
   :src-dir "src/main/"
   :test-dir "src/test/"
   :test-suffix "Spec"))

(add-to-list 'projection-project-types projection-project-type-bloop 'append)



(defvar projection-project-type-lein-test
  (projection-type
   :name 'lein-test
   :predicate "project.clj"
   :build "lein compile"
   :test "lein test"
   :test-suffix "_test"))

(add-to-list 'projection-project-types projection-project-type-lein-test 'append)



(defvar projection-project-type-lein-midje
  (projection-type
   :name 'lein-midje
   :predicate (projection--all-files-exists "project.clj" ".midje.clj")
   :build "lein compile"
   :test "lein midje"
   :test-prefix "t_"))

(add-to-list 'projection-project-types projection-project-type-lein-midje 'append)



(defvar projection-project-type-boot-clj
  (projection-type
   :name 'boot-clj
   :predicate "build.boot"
   :build "boot aot"
   :test "boot test"
   :test-suffix "_test"))

(add-to-list 'projection-project-types projection-project-type-boot-clj 'append)



(defvar projection-project-type-clojure-cli
  (projection-type
   :name 'clojure-cli
   :predicate "deps.edn"
   :test-suffix "_test"))

(add-to-list 'projection-project-types projection-project-type-clojure-cli 'append)



(defvar projection-project-type-ruby-rspec
  (projection-type
   :name 'ruby-rspec
   :predicate (projection--all-files-exists "Gemfile" "lib" "spec")
   :build "bundle exec rake"
   :test "bundle exec rspec"
   :src-dir "lib/"
   :test-dir "spec/"
   :test-suffix "_spec"))

(add-to-list 'projection-project-types projection-project-type-ruby-rspec 'append)



(defvar projection-project-type-ruby-test
  (projection-type
   :name 'ruby-test
   :predicate (projection--all-files-exists "Gemfile" "lib" "test")
   :build "bundle exec rake"
   :test "bundle exec rake test"
   :test-suffix "_spec"))

(add-to-list 'projection-project-types projection-project-type-ruby-test 'append)



;; Rails needs to be registered after npm, otherwise `package.json` makes it `npm`.
;; https://github.com/bbatsov/projectile/pull/1191
(defvar projection-project-type-rails-test
  (projection-type
   :name 'rails-test
   :predicate (projection--all-files-exists "Gemfile" "app" "lib" "db" "config" "test")
   :build "bundle exec rails server"
   :test "bundle exec rake test"
   :test-suffix "_test"))

(add-to-list 'projection-project-types projection-project-type-rails-test 'append)



(defvar projection-project-type-rails-rspec
  (projection-type
   :name 'rails-rspec
   :predicate (projection--all-files-exists "Gemfile" "app" "lib" "db" "config" "spec")
   :build "bundle exec rails server"
   :test "bundle exec rspec"
   :src-dir "lib/"
   :test-dir "spec/"
   :test-suffix "_test"))

(add-to-list 'projection-project-types projection-project-type-rails-rspec 'append)



(defvar projection-project-type-crystal-rspec
  (projection-type
   :name 'crystal-rspec
   :predicate "shard.yml"
   :test "crystal spec"
   :src-dir "src/"
   :test-dir "spec/"
   :test-suffix "_spec"))

(add-to-list 'projection-project-types projection-project-type-crystal-rspec 'append)



(defvar projection-project-type-emacs-cask
  (projection-type
   :name 'emacs-cask
   :predicate "Cask"
   :configure "cask install"
   :test-prefix "test-"
   :test-suffix "-test"))

(add-to-list 'projection-project-types projection-project-type-emacs-cask 'append)



(defvar projection-project-type-emacs-eask
  (projection-type
   :name 'emacs-eask
   :predicate "Eask"
   :build "eask install"
   :test-prefix "test-"
   :test-suffix "-test"))

(add-to-list 'projection-project-types projection-project-type-emacs-eask 'append)



(defvar projection-project-type-emacs-eldev
  (projection-type
   :name 'emacs-eldev
   :predicate '("Eldev" "Eldev-local")
   :build "eldev compile"
   :test "eldev test"
   :run "eldev emacs"
   :package "eldev package"))

(add-to-list 'projection-project-types projection-project-type-emacs-eldev 'append)



(defvar projection-project-type-r
  (projection-type
   :name 'r
   :predicate "DESCRIPTION"
   :build "R CMD INSTALL --with-keep.source ."
   :test (concat "R CMD check -o " temporary-file-directory " .")))

(add-to-list 'projection-project-types projection-project-type-r 'append)



(defvar projection-project-type-haskell-stack
  (projection-type
   :name 'haskell-stack
   :predicate "stack.yaml"
   :build "stack build"
   :test "stack build --test"
   :test-suffix "Spec"))

(add-to-list 'projection-project-types projection-project-type-haskell-stack 'append)



(defvar projection-project-type-rust-cargo
  (projection-type
   :name 'rust-cargo
   :predicate "Cargo.toml"
   :build (projection--command-or-shell 'rustic-compile "cargo build")
   :test (projection--command-or-shell 'rustic-cargo-test "cargo test")
   :install (projection--command-or-shell 'rustic-cargo-install "cargo install")
   :run (projection--command-or-shell 'rustic-cargo-run "cargo run")))

(add-to-list 'projection-project-types projection-project-type-rust-cargo 'append)



(defvar projection-project-type-racket
  (projection-type
   :name 'racket
   :predicate "info.rkt"
   :test "raco test ."
   :install "raco pkg install"
   :package "raco pkg create --source $(pwd)"))

(add-to-list 'projection-project-types projection-project-type-racket 'append)



(defvar projection-project-type-dart
  (projection-type
   :name 'dart
   :predicate "pubspec.yaml"
   :build "pub get"
   :test "pub run test"
   :run "dart"
   :test-suffix "_test.dart"))

(add-to-list 'projection-project-types projection-project-type-dart 'append)



(defvar projection-project-type-elm
  (projection-type
   :name 'elm
   :predicate "elm.json"
   :build "elm make"))

(add-to-list 'projection-project-types projection-project-type-elm 'append)



(defvar projection-project-type-julia
  (projection-type
   :name 'julia
   :predicate "Project.toml"
   :build "julia --project=@. -e 'import Pkg; Pkg.precompile(); Pkg.build()'"
   :test "julia --project=@. -e 'import Pkg; Pkg.test()' --check-bounds=yes"
   :src-dir "src"
   :test-dir "test"))

(add-to-list 'projection-project-types projection-project-type-julia 'append)



(defvar projection-project-type-ocaml-dune
  (projection-type
   :name 'ocaml-dune
   :predicate "dune-project"
   :build "dune build"
   :test "dune runtest"))

(add-to-list 'projection-project-types projection-project-type-ocaml-dune 'append)



(defvar projection-project-type-vscode-tasks
  (projection-type
   :name 'vscode-tasks
   :predicate ".vscode/tasks.json")
  "Registered only for multi-target registration.")

(add-to-list 'projection-project-types projection-project-type-vscode-tasks 'append)



(provide 'projection-types)
;;; projection-types.el ends here
