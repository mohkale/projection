;;; projection-multi-haskell-stack.el --- Projection integration for `compile-multi' and the Haskell Stack project type. -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Patrick M. Niedzielski <patrick@pniedzielski.net>
;; Copyright (C) 2023 Mohsin Kaleem

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

;; This library exposes a target generation function for
;; `compile-multi' which sources the list of available targets from a
;; Haskell Stack project's tasks.

;;; Code:

(require 'projection-types)
(require 'projection-type-haskell-stack)
(require 'projection-multi)

(defgroup projection-multi-haskell-stack nil
  "Helpers for `compile-multi' and Haskell Stack projects."
  :group 'projection-multi)



(defun projection-multi-haskell-stack--exe-p (target)
  "Return a true value if TARGET is an executable target, nil otherwise."
  (string-match ":exe:" target))

(defun projection-multi-haskell-stack--test-p (target)
  "Return a true value if TARGET is an test suite target, nil otherwise."
  (string-match ":test:" target))

(defun projection-multi-haskell-stack--bench-p (target)
  "Return a true value if TARGET is an benchmark target, nil otherwise."
  (string-match ":bench:" target))



(defcustom projection-multi-haskell-stack-cache-targets t
  "When true cache the Haskell Stack packages and targets of each project."
  :type '(boolean :tag "Always/Never cache targets")
  :group 'projection-multi-haskell-stack)

(defun projection-multi-haskell-stack--packages ()
  "Read Haskell Stack packages respecting project cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-haskell-stack-packages
   projection-multi-haskell-stack-cache-targets
   #'projection-multi-haskell-stack--packages2))

(projection--declare-cache-var
  'projection-multi-haskell-stack-packages
  :title "Multi Haskell Stack packages"
  :category "Haskell Stack"
  :description "Haskell Stack packages associated with this project"
  :hide t)

(defun projection-multi-haskell-stack--packages2 ()
  "Read Haskell Stack packages."
  (let ((result))
    (projection--log :debug "Resolving available Haskell Stack packages")
    (projection--with-shell-command-buffer
     (projection-haskell-stack--command "ide" "packages" "--stdout")
     (while (not (eobp))
       (let ((line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position))))
         (push (cons line "Package") result))
       (forward-line 1)))
    (nreverse result)))

(defun projection-multi-haskell-stack--targets ()
  "Read Haskell Stack targets respecting project cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-haskell-stack-targets
   projection-multi-haskell-stack-cache-targets
   #'projection-multi-haskell-stack--targets2))

(projection--declare-cache-var
  'projection-multi-haskell-stack-targets
  :title "Multi Haskell Stack targets"
  :category "Haskell Stack"
  :description "Haskell Stack targets associated with this project"
  :hide t)

(defun projection-multi-haskell-stack--targets2 ()
  "Read Haskell Stack targets."
  (let ((result))
    (projection--log :debug "Resolving available Haskell Stack targets")
    (projection--with-shell-command-buffer
     (projection-haskell-stack--command "ide" "targets" "--stdout")
     (while (not (eobp))
       (let* ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position)))
              (documentation
               (cond ((projection-multi-haskell-stack--exe-p line)
                      "Executable")
                     ((projection-multi-haskell-stack--test-p line)
                      "Test suite")
                     ((projection-multi-haskell-stack--bench-p line)
                      "Benchmark")
                     (t "Library"))))
         (push (cons line documentation) result))
       (forward-line 1)))
    (nreverse result)))



(defun projection-multi-haskell-stack--package-build-target
    (package documentation project-type)
  "`compile-multi' target generator function to build a Haskell Stack PACKAGE.
The generated target will be prefixed with PROJECT-TYPE.  When set, the
generated target will be annotated with DOCUMENTATION."
  `(,(concat project-type ":" package)
    :command
    ,(projection-haskell-stack--command "build" package)
    :annotation
    ,(if documentation
         (concat documentation ": Build")
       (concat "Build"))))

(defun projection-multi-haskell-stack--package-test-target
    (package documentation project-type)
  "`compile-multi' target generator function to test a Haskell Stack PACKAGE.
The generated target will be prefixed with PROJECT-TYPE.  When set, the
generated target will be annotated with DOCUMENTATION."
  `(,(concat project-type ":" package ":test")
    :command
    ,(projection-haskell-stack--command "test" package)
    :annotation
    ,(if documentation
         (concat documentation ": Run test suite")
       (concat "Run test suite"))))

(defun projection-multi-haskell-stack--package-bench-target
    (package documentation project-type)
  "`compile-multi' target generator function to benchmark a Haskell Stack
PACKAGE.
The generated target will be prefixed with PROJECT-TYPE.  When set, the
generated target will be annotated with DOCUMENTATION."
  `(,(concat project-type ":" package ":bench")
    :command
    ,(projection-haskell-stack--command "bench" package)
    :annotation
    ,(if documentation
         (concat documentation ": Run benchmarks")
       (concat "Run benchmarks"))))

(defun projection-multi-haskell-stack--package-haddock-target
    (package documentation project-type)
  "`compile-multi' target generator function to build the docs for a
Haskell Stack PACKAGE.
The generated target will be prefixed with PROJECT-TYPE.  When set, the
generated target will be annotated with DOCUMENTATION."
  `(,(concat project-type ":" package ":haddock")
    :command
    ,(projection-haskell-stack--command "haddock" package)
    :annotation
    ,(if documentation
         (concat documentation ": Build haddocks")
       (concat "Build haddocks"))))

(defun projection-multi-haskell-stack--package-ghci-target
    (package documentation project-type)
  "`compile-multi' target generator function to open ghci in a Haskell Stack
PACKAGE.
The generated target will be prefixed with PROJECT-TYPE.  When set, the
generated target will be annotated with DOCUMENTATION."
  `(,(concat project-type ":" package ":ghci")
    :command
    ,(projection-haskell-stack--command "ghci" package)
    :annotation
    ,(if documentation
         (concat documentation ": Open ghci")
       (concat "Open ghci"))))

(defun projection-multi-haskell-stack--package-clean-target
    (package documentation project-type)
  "`compile-multi' target generator function to clean a Haskell Stack PACKAGE.
The generated target will be prefixed with PROJECT-TYPE.  When set, the
generated target will be annotated with DOCUMENTATION."
  `(,(concat project-type ":" package ":clean")
    :command
    ,(projection-haskell-stack--command "clean" package)
    :annotation
    ,(if documentation
         (concat documentation ": Clean artifacts")
       (concat "Clean artifacts"))))

(defun projection-multi-haskell-stack--target-build-target
    (target documentation project-type)
  "`compile-multi' target generator function to build a Haskell Stack TARGET.
The generated target will be prefixed with PROJECT-TYPE.  When set, the
generated target will be annotated with DOCUMENTATION."
  `(,(concat project-type ":" target)
    :command
    ,(projection-haskell-stack--command "build" target
                                        "--no-run-tests" "--no-run-benchmarks")
    :annotation
    ,(if documentation
         (concat documentation ": Build")
       (concat "Build"))))

(defun projection-multi-haskell-stack--target-test-target
    (target documentation project-type)
  "`compile-multi' target generator function to run a test suite Haskell Stack
TARGET.
The generated target will be prefixed with PROJECT-TYPE.  When set, the
generated target will be annotated with DOCUMENTATION."
  `(,(concat project-type ":" target)
    :command
    ,(projection-haskell-stack--command "build" target "--run-tests")
    :annotation
    ,(if documentation
         (concat documentation ": Build and run")
       (concat "Run test suite"))))

(defun projection-multi-haskell-stack--target-bench-target
    (target documentation project-type)
  "`compile-multi' target generator function to run a benchmark Haskell Stack
TARGET.
The generated target will be prefixed with PROJECT-TYPE.  When set, the
generated target will be annotated with DOCUMENTATION."
  `(,(concat project-type ":" target)
    :command
    ,(projection-haskell-stack--command "build" target "--run-benchmarks")
    :annotation
    ,(if documentation
         (concat documentation ": Build and run")
       (concat "Run benchmark"))))

(defun projection-multi-haskell-stack--target-run-target
    (target documentation project-type)
  "`compile-multi' target generator function to run an executable Haskell Stack
TARGET.
The generated target will be prefixed with PROJECT-TYPE.  When set, the
generated target will be annotated with DOCUMENTATION."
  `(,(concat project-type ":" target ":run")
    :command
    ,(projection-haskell-stack--command "run" target)
    :annotation
    ,(if documentation
         (concat documentation ": Run")
       (concat "Run"))))



;;;###autoload
(defun projection-multi-haskell-stack-targets (&optional project-type)
  "`compile-multi' target generator function for Haskell Stack projects.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "haskell-stack"))

  (append
   ;; Packages
   (cl-loop for (package . documentation) in
            (projection-multi-haskell-stack--packages)
            append `(,(projection-multi-haskell-stack--package-build-target
                       package documentation project-type)
                     ,(projection-multi-haskell-stack--package-test-target
                       package documentation project-type)
                     ,(projection-multi-haskell-stack--package-bench-target
                       package documentation project-type)
                     ,(projection-multi-haskell-stack--package-haddock-target
                       package documentation project-type)
                     ,(projection-multi-haskell-stack--package-ghci-target
                       package documentation project-type)
                     ,(projection-multi-haskell-stack--package-clean-target
                       package documentation project-type)))
   ;; Targets
   (cl-loop for (target . documentation) in
            (projection-multi-haskell-stack--targets)
            append
            `(,@(cond
                ;; Executable targets
                ((projection-multi-haskell-stack--exe-p target)
                 (list
                  (projection-multi-haskell-stack--target-build-target
                   target documentation project-type)
                  (projection-multi-haskell-stack--target-run-target
                   target documentation project-type)))
                ;; Test suite targets
                ((projection-multi-haskell-stack--test-p target)
                 (list
                  (projection-multi-haskell-stack--target-test-target
                   target documentation project-type)))
                ;; Benchmark targets
                ((projection-multi-haskell-stack--bench-p target)
                 (list
                  (projection-multi-haskell-stack--target-bench-target
                   target documentation project-type)))
                ;; Library targets
                (t (list
                    (projection-multi-haskell-stack--target-build-target
                     target documentation project-type))))))))

;;;###autoload
(defun projection-multi-compile-haskell-stack ()
  "`compile-multi' wrapper for only Haskell Stack targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-haskell-stack-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (projection-type-append-compile-multi-targets
    projection-project-type-haskell-stack
    #'projection-multi-haskell-stack-targets))

(provide 'projection-multi-haskell-stack)
;;; projection-multi-haskell-stack.el ends here
