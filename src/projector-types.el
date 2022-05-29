;;; projector-types.el --- Built-in project type definitions for `projector'. -*- lexical-binding: t; -*-

;;; Commentary:

;; This module registers a collection of standard project types for use with
;; `projector'.

;;; Code:

(require 'subr-x)
(require 'projector-core)

;; NOTE: Project type detection happens in reverse order to registration. As
;; function based project type detection is considerably slower than simple
;; file based matching, such project types are defined near the top of this
;; file and the remaining near the end. Ideally common project types should
;; be checked earlier than exotic ones.

(defun projector--command-or-shell (func shell-command)
  "Generate a command function which will run either FUNC or SHELL-COMMAND.
The result is a lambda which, if FUNC is bound and interactive returns FUNC,
otherwise it will return SHELL-COMMAND."
  (lambda ()
    (if (commandp func)
        func
      shell-command)))

(defun projector--join-shell-command (argv)
  "Join quoted arguments from ARGV into a shell command."
  (string-join (mapcar #'shell-quote-argument argv) " "))

(defun projector--all-files-exists (&rest files)
  "Generate a predicate function which is true if all files in FILES exist."
  (apply-partially #'cl-every #'file-exists-p files))



(projector-register-type 'haskell-cabal
  :predicate (defun projector-haskell-cabal-project-p ()
               (and (file-expand-wildcards "?*.cabal")
                    (not (file-exists-p "stack.yml"))))
  :build "cabal build"
  :test  "cabal test"
  :run   "cabal run"
  :test-suffix "Spec")



(projector-register-type 'dotnet
  :predicate (defun projector-dotnet-project-p ()
               (or (file-expand-wildcards "?*.csproj")
                   (file-expand-wildcards "?*.fsproj")
                   (file-expand-wildcards "?*.sln")))
  :build "dotnet build"
  :test  "dotnet run"
  :run   "dotnet test")



(projector-register-type 'golang
  :predicate (defun projector-golang-project-p ()
               (or (file-exists-p "go.mod")
                   (file-expand-wildcards "*.go")))
  :build "go build"
  :test "go test ./..."
  :test-suffix "_test")



(projector-register-type 'scons
  :predicate "SConstruct"
  :build "scons"
  :test "scons test"
  :test-suffix "test")



;; TODO: This is the only project type that specifies a :compilation-dir option.
;; is it really necessary, shouldn't the command take an option for this instead.

;; (projector-register-type 'meson
;;   :predicate "meson.build"
;;   :compilation-dir "build"
;;   :configure "meson %s"
;;   :build "ninja"
;;   :test "ninja test")



(projector-register-type 'nix
  :predicate "default.nix"
  :build "nix-build"
  :test  "nix-build")



(projector-register-type 'nix-flake
  :predicate "flake.nix"
  :build "nix build"
  :test  "nix flake check"
  :run   "nix run")



(projector-register-type 'bazel
  :predicate "WORKSPACE"
  :build "bazel build"
  :test  "bazel test"
  :run   "bazel run")



(projector-register-type 'debian
  :predicate "debian/control"
  :build "debuild -uc -us")



(projector-register-type 'make
  :predicate '("Makefile" "GNUMakefile")
  :build   "make"
  :test    "make test"
  :install "make install")



(projector-register-type 'gnumake
  :predicate "GNUMakefile"
  :build "make"
  :test "make test"
  :install "make install")



(defcustom projector-cmake-build-directory "build"
  "Build directory for cmake project builds."
  :type 'string
  :group 'projector)

(defcustom projector-cmake-configure-options nil
  "Default CMake options when configured with projector.
Place any -D options or extra flags you always want to use (for example
-DCMAKE_EXPORT_COMPILE_COMMANDS) in this option variable."
  :type '(list (repeat string))
  :group 'projector)

;; TODO: Support [[https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html][cmake-presets]].

(defun projector--cmake-command (&optional target)
  "Generate a CMake command optionally to run TARGET."
  (projector--join-shell-command
   `("cmake"
     "--build" ,projector-cmake-build-directory
     ,@(when target (list "--target" target)))))

(projector-register-type 'cmake
  :predicate "CMakeLists.txt"
  ;; The configure step takes the source directory and the output build
  ;; directory.
  :configure (lambda ()
               (projector--join-shell-command
                `("cmake"
                  "-S" "."
                  "-B" ,projector-cmake-build-directory
                  ,@projector-cmake-configure-options)))
  ;; The remaining commands take the build directory and an optional target
  ;; with it.
  :build (lambda () (projector--cmake-command))
  :test (lambda () (projector--cmake-command "test"))
  :install (lambda () (projector--cmake-command "install"))
  :package (lambda () (projector--cmake-command "package")))



(projector-register-type 'php-symfony
  :predicate (projector--all-files-exists "composer.json" "app" "src" "vendor")
  :build "app/console server:run"
  :test "phpunit -c app"
  :test-suffix "Test")



(projector-register-type 'rebar
  :predicate "rebar.config"
  :build "rebar3 compile"
  :test "rebar3 do eunit,ct"
  :test-suffix "_SUITE")



(projector-register-type 'elixir
  :predicate "mix.exs"
  :build "mix compile"
  :test "mix test"
  :test-suffix "_test")



(projector-register-type 'grunt
  :predicate "Gruntfile.js"
  :build "grunt"
  :test "grunt test")



(projector-register-type 'gulp
  :predicate "gulpfile.js"
  :build "gulp"
  :test "gulp test")



(projector-register-type 'npm
  :predicate "package.json"
  :build "npm install"
  :test "npm test"
  :test-suffix ".test")



(projector-register-type 'angular
  :predicate (projector--all-files-exists "angular.json" ".angular-cli.json")
  :build "ng build"
  :run "ng serve"
  :test "ng test"
  :test-suffix ".spec")



(projector-register-type 'django
  :predicate "manage.py"
  :build "python manage.py runserver"
  :test "python manage.py test"
  :test-prefix "test_"
  :test-suffix "_test")



(projector-register-type 'python-pip
  :predicate "requirements.txt"
  :build "python setup.py build"
  :test "python -m unittest discover"
  :test-prefix "test_"
  :test-suffix "_test")



(projector-register-type 'python-pkg
  :predicate "setup.py"
  :build "python setup.py build"
  :test "python -m unittest discover"
  :test-prefix "test_"
  :test-suffix "_test")



(projector-register-type 'python-tox
  :predicate "tox.ini"
  :build "tox -r --notest"
  :test "tox"
  :test-prefix "test_"
  :test-suffix "_test")



(projector-register-type 'python-pipenv
  :predicate "Pipfile"
  :build "pipenv run build"
  :test "pipenv run test"
  :test-prefix "test_"
  :test-suffix "_test")



(projector-register-type 'python-poetry
  :predicate "poetry.lock"
  :build "poetry build"
  :test "poetry run python -m unittest discover"
  :test-prefix "test_"
  :test-suffix "_test")



(projector-register-type 'maven
  :predicate "pom.xml"
  :build "mvn -B clean install"
  :test "mvn -B test"
  :src-dir "src/main/"
  :test-dir "src/test/")



(projector-register-type 'gradle
  :predicate "build.gradle"
  :build "gradle build"
  :test "gradle test"
  :test-suffix "Spec")



(projector-register-type 'gradlew
  :predicate "gradlew"
  :build "./gradlew build"
  :test "./gradlew test"
  :test-suffix "Spec")



(projector-register-type 'grails
  :predicate (projector--all-files-exists "application.yml" "grails-app")
  :build "grails package"
  :test "grails test-app"
  :test-suffix "Spec")



(projector-register-type 'sbt
  :predicate "build.sbt"
  :build "sbt compile"
  :test "sbt test"
  :src-dir "main"
  :test-dir "test"
  :test-suffix "Spec")



(projector-register-type 'mill
  :predicate "build.sc"
  :build "mill all __.compile"
  :test "mill all __.test"
  :test-suffix "Test")



(projector-register-type 'lein-test
  :predicate "project.clj"
  :build "lein compile"
  :test "lein test"
  :test-suffix "_test")



(projector-register-type 'lein-midje
  :predicate (projector--all-files-exists "project.clj" ".midje.clj")
  :build "lein compile"
  :test "lein midje"
  :test-prefix "t_")



(projector-register-type 'boot-clj
  :predicate "build.boot"
  :build "boot aot"
  :test "boot test"
  :test-suffix "_test")



(projector-register-type 'clojure-cli
  :predicate "deps.edn"
  :test-suffix "_test")



(projector-register-type 'bloop
  :predicate ".bloop"
  :build "bloop compile root"
  :test "bloop test --propagate --reporter scalac root"
  :src-dir "src/main/"
  :test-dir "src/test/"
  :test-suffix "Spec")



(projector-register-type 'ruby-rspec
  :predicate (projector--all-files-exists "Gemfile" "lib" "spec")
  :build "bundle exec rake"
  :test "bundle exec rspec"
  :src-dir "lib/"
  :test-dir "spec/"
  :test-suffix "_spec")



(projector-register-type 'ruby-test
  :predicate (projector--all-files-exists "Gemfile" "lib" "test")
  :build "bundle exec rake"
  :test "bundle exec rake test"
  :test-suffix "_spec")



;; Rails needs to be registered after npm, otherwise `package.json` makes it `npm`.
;; https://github.com/bbatsov/projectile/pull/1191
(projector-register-type 'rails-test
  :predicate (projector--all-files-exists "Gemfile" "app" "lib" "db" "config" "test")
  :build "bundle exec rails server"
  :test "bundle exec rake test"
  :test-suffix "_test")



(projector-register-type 'rails-rspec
  :predicate (projector--all-files-exists "Gemfile" "app" "lib" "db" "config" "spec")
  :build "bundle exec rails server"
  :test "bundle exec rspec"
  :src-dir "lib/"
  :test-dir "spec/"
  :test-suffix "_test")



(projector-register-type 'crystal-spec
  :predicate "shard.yml"
  :test "crystal spec"
  :src-dir "src/"
  :test-dir "spec/"
  :test-suffix "_spec")



(projector-register-type 'emacs-cask
  :predicate "Cask"
  :build "cask install"
  :test-prefix "test-"
  :test-suffix "-test")



(projector-register-type 'emacs-eldev
  :predicate '("Eldev" "Eldev-local")
  :build "eldev compile"
  :test "eldev test"
  :run "eldev emacs"
  :package "eldev package")



(projector-register-type 'r
  :predicate "DESCRIPTION"
  :build "R CMD INSTALL --with-keep.source ."
  :test (concat "R CMD check -o " temporary-file-directory " ."))



(projector-register-type 'haskell-stack
  :predicate "stack.yaml"
  :build "stack build"
  :test "stack build --test"
  :test-suffix "Spec")



(projector-register-type 'rust-cargo
  :predicate "Cargo.toml"
  :build (projector--command-or-shell 'rustic-compile "cargo build")
  :test  (projector--command-or-shell 'rustic-cargo-test "cargo test")
  :run   (projector--command-or-shell 'rustic-cargo-run "cargo run"))



(projector-register-type 'racket
  :predicate "info.rkt"
  :test "raco test ."
  :install "raco pkg install"
  :package "raco pkg create --source $(pwd)")



(projector-register-type 'dart
  :predicate "pubspec.yaml"
  :build "pub get"
  :test "pub run test"
  :run "dart"
  :test-suffix "_test.dart")



(projector-register-type 'ocaml-dune
  :predicate "dune-project"
  :build "dune build"
  :test "dune runtest")



(provide 'projector-types)
;;; projector-types.el ends here
