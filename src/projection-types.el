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

;; NOTE: Project type detection happens in reverse order to registration. As
;; function based project type detection is considerably slower than simple
;; file based matching, such project types are defined near the top of this
;; file and the remaining near the end. Ideally common project types should
;; be checked earlier than exotic ones.



(projection-register-type 'haskell-cabal
  :predicate (defun projection-haskell-cabal-project-p ()
               (and (file-expand-wildcards "?*.cabal")
                    (not (file-exists-p "stack.yml"))))
  :build "cabal build"
  :test  "cabal test"
  :run   "cabal run"
  :test-suffix "Spec")



(projection-register-type 'dotnet
  :predicate (defun projection-dotnet-project-p ()
               (or (file-expand-wildcards "?*.csproj")
                   (file-expand-wildcards "?*.fsproj")
                   (file-expand-wildcards "?*.sln")))
  :build "dotnet build"
  :test  "dotnet run"
  :run   "dotnet test")



(projection-register-type 'golang
  :predicate (defun projection-golang-project-p ()
               (or (file-exists-p "go.mod")
                   (file-expand-wildcards "*.go")))
  :build "go build"
  :test "go test ./..."
  :test-suffix "_test")



(projection-register-type 'scons
  :predicate "SConstruct"
  :build "scons"
  :test "scons test"
  :test-suffix "test")



;; TODO: This is the only project type that specifies a :compilation-dir option.
;; is it really necessary, shouldn't the command take an option for this instead.

;; (projection-register-type 'meson
;;   :predicate "meson.build"
;;   :compilation-dir "build"
;;   :configure "meson %s"
;;   :build "ninja"
;;   :test "ninja test")



(projection-register-type 'nix
  :predicate "default.nix"
  :build "nix-build"
  :test  "nix-build")



(projection-register-type 'nix-flake
  :predicate "flake.nix"
  :build "nix build"
  :test  "nix flake check"
  :run   "nix run")



(projection-register-type 'bazel
  :predicate "WORKSPACE"
  :build "bazel build"
  :test  "bazel test"
  :run   "bazel run")



(projection-register-type 'debian
  :predicate "debian/control"
  :build "debuild -uc -us")



(require 'projection-multi-make)

(projection-register-type 'make
  :predicate '("Makefile" "GNUMakefile")
  :build   "make"
  :test    "make test"
  :install "make install"
  :targets #'projection-multi-make-targets)



(require 'projection-types-cmake)



(projection-register-type 'php-symfony
  :predicate (projection--all-files-exists "composer.json" "app" "src" "vendor")
  :build "app/console server:run"
  :test "phpunit -c app"
  :test-suffix "Test")



(projection-register-type 'rebar
  :predicate "rebar.config"
  :build "rebar3 compile"
  :test "rebar3 do eunit,ct"
  :test-suffix "_SUITE")



(projection-register-type 'elixir
  :predicate "mix.exs"
  :build "mix compile"
  :test "mix test"
  :test-suffix "_test")



(projection-register-type 'grunt
  :predicate "Gruntfile.js"
  :build "grunt"
  :test "grunt test")



(projection-register-type 'gulp
  :predicate "gulpfile.js"
  :build "gulp"
  :test "gulp test")



(projection-register-type 'npm
  :predicate "package.json"
  :build "npm install"
  :test "npm test"
  :test-suffix ".test")



(projection-register-type 'angular
  :predicate (projection--all-files-exists "angular.json" ".angular-cli.json")
  :build "ng build"
  :run "ng serve"
  :test "ng test"
  :test-suffix ".spec")



(projection-register-type 'django
  :predicate "manage.py"
  :build "python manage.py runserver"
  :test "python manage.py test"
  :test-prefix "test_"
  :test-suffix "_test")



(projection-register-type 'python-pip
  :predicate "requirements.txt"
  :build "python setup.py build"
  :test "python -m unittest discover"
  :test-prefix "test_"
  :test-suffix "_test")



(projection-register-type 'python-pkg
  :predicate "setup.py"
  :build "python setup.py build"
  :test "python -m unittest discover"
  :test-prefix "test_"
  :test-suffix "_test")



(require 'projection-multi-tox)

(projection-register-type 'python-tox
  :predicate "tox.ini"
  :build "tox -r --notest"
  :test "tox"
  :test-prefix "test_"
  :test-suffix "_test"
  :targets #'projection-multi-tox-targets)



(projection-register-type 'python-pipenv
  :predicate "Pipfile"
  :build "pipenv run build"
  :test "pipenv run test"
  :test-prefix "test_"
  :test-suffix "_test")



(require 'projection-multi-poetry-poe)

(projection-register-type 'python-poetry
  :predicate "poetry.lock"
  :build "poetry build"
  :test "poetry run python -m unittest discover"
  :test-prefix "test_"
  :test-suffix "_test"
  :targets #'projection-multi-poetry-poe-targets)



(projection-register-type 'maven
  :predicate "pom.xml"
  :build "mvn -B clean install"
  :test "mvn -B test"
  :src-dir "src/main/"
  :test-dir "src/test/")



(projection-register-type 'gradle
  :predicate "build.gradle"
  :build "gradle build"
  :test "gradle test"
  :test-suffix "Spec")



(projection-register-type 'gradlew
  :predicate "gradlew"
  :build "./gradlew build"
  :test "./gradlew test"
  :test-suffix "Spec")



(projection-register-type 'grails
  :predicate (projection--all-files-exists "application.yml" "grails-app")
  :build "grails package"
  :test "grails test-app"
  :test-suffix "Spec")



(projection-register-type 'sbt
  :predicate "build.sbt"
  :build "sbt compile"
  :test "sbt test"
  :src-dir "main"
  :test-dir "test"
  :test-suffix "Spec")



(projection-register-type 'mill
  :predicate "build.sc"
  :build "mill all __.compile"
  :test "mill all __.test"
  :test-suffix "Test")



(projection-register-type 'lein-test
  :predicate "project.clj"
  :build "lein compile"
  :test "lein test"
  :test-suffix "_test")



(projection-register-type 'lein-midje
  :predicate (projection--all-files-exists "project.clj" ".midje.clj")
  :build "lein compile"
  :test "lein midje"
  :test-prefix "t_")



(projection-register-type 'boot-clj
  :predicate "build.boot"
  :build "boot aot"
  :test "boot test"
  :test-suffix "_test")



(projection-register-type 'clojure-cli
  :predicate "deps.edn"
  :test-suffix "_test")



(projection-register-type 'bloop
  :predicate ".bloop"
  :build "bloop compile root"
  :test "bloop test --propagate --reporter scalac root"
  :src-dir "src/main/"
  :test-dir "src/test/"
  :test-suffix "Spec")



(projection-register-type 'ruby-rspec
  :predicate (projection--all-files-exists "Gemfile" "lib" "spec")
  :build "bundle exec rake"
  :test "bundle exec rspec"
  :src-dir "lib/"
  :test-dir "spec/"
  :test-suffix "_spec")



(projection-register-type 'ruby-test
  :predicate (projection--all-files-exists "Gemfile" "lib" "test")
  :build "bundle exec rake"
  :test "bundle exec rake test"
  :test-suffix "_spec")



;; Rails needs to be registered after npm, otherwise `package.json` makes it `npm`.
;; https://github.com/bbatsov/projectile/pull/1191
(projection-register-type 'rails-test
  :predicate (projection--all-files-exists "Gemfile" "app" "lib" "db" "config" "test")
  :build "bundle exec rails server"
  :test "bundle exec rake test"
  :test-suffix "_test")



(projection-register-type 'rails-rspec
  :predicate (projection--all-files-exists "Gemfile" "app" "lib" "db" "config" "spec")
  :build "bundle exec rails server"
  :test "bundle exec rspec"
  :src-dir "lib/"
  :test-dir "spec/"
  :test-suffix "_test")



(projection-register-type 'crystal-spec
  :predicate "shard.yml"
  :test "crystal spec"
  :src-dir "src/"
  :test-dir "spec/"
  :test-suffix "_spec")



(projection-register-type 'emacs-cask
  :predicate "Cask"
  :build "cask install"
  :test-prefix "test-"
  :test-suffix "-test")



(projection-register-type 'emacs-eldev
  :predicate '("Eldev" "Eldev-local")
  :build "eldev compile"
  :test "eldev test"
  :run "eldev emacs"
  :package "eldev package")



(projection-register-type 'r
  :predicate "DESCRIPTION"
  :build "R CMD INSTALL --with-keep.source ."
  :test (concat "R CMD check -o " temporary-file-directory " ."))



(projection-register-type 'haskell-stack
  :predicate "stack.yaml"
  :build "stack build"
  :test "stack build --test"
  :test-suffix "Spec")



(projection-register-type 'rust-cargo
  :predicate "Cargo.toml"
  :build (projection--command-or-shell 'rustic-compile "cargo build")
  :test  (projection--command-or-shell 'rustic-cargo-test "cargo test")
  :run   (projection--command-or-shell 'rustic-cargo-run "cargo run"))



(projection-register-type 'racket
  :predicate "info.rkt"
  :test "raco test ."
  :install "raco pkg install"
  :package "raco pkg create --source $(pwd)")



(projection-register-type 'dart
  :predicate "pubspec.yaml"
  :build "pub get"
  :test "pub run test"
  :run "dart"
  :test-suffix "_test.dart")



(projection-register-type 'ocaml-dune
  :predicate "dune-project"
  :build "dune build"
  :test "dune runtest")



(provide 'projection-types)
;;; projection-types.el ends here
