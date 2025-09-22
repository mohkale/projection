;;; projection-type-haskell-stack.el --- Helpers for supporting Haskell Stack projects. -*- lexical-binding: t; -*-

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

;; Projection project-type helpers for Haskell Stack projects.

;;; Code:

(require 'project)
(require 'f)
(require 'projection)
(require 'projection-core-misc)
(require 'projection-core-type)
(require 'projection-core-match)
(require 'projection-utils)

(defgroup projection-type-haskell-stack nil
  "Projection Haskell Stack project type."
  :group 'projection-types)

(defun projection-haskell-stack--command (&rest args)
  "Generate a Haskell Stack command with ARGS."
  (projection--join-shell-command
   `("stack"
     ,@(when-let* ((job-count (projection--guess-parallelism
                                     projection-build-jobs)))
         (list "--jobs" (number-to-string job-count)))
     ,@args)))

;; Haskell Stack compilation commands.

(defun projection-haskell-stack-run-build ()
  "Build command generator for Haskell Stack projects."
  ;; See https://stackoverflow.com/a/4714118
  (projection-haskell-stack--command "build"))

(defun projection-haskell-stack-run-test ()
  "Test command generator for Haskell Stack projects."
  (projection-haskell-stack--command "test"))

(defun projection-haskell-stack-run-bench ()
  "Benchmark command generator for Haskell Stack projects."
  (projection-haskell-stack--command "bench"))

(defun projection-haskell-stack-run-doc ()
  "Haddock command generator for Haskell Stack projects."
  (projection-haskell-stack--command "haddock"))

(defun projection-haskell-stack-run-clean ()
  "Clean command generator for Haskell Stack projects."
  (projection-haskell-stack--command "clean"))

(defun projection-haskell-stack-run-ghci ()
  "ghci command generator for Haskell Stack projects."
  (projection-haskell-stack--command "ghci"))

(provide 'projection-type-haskell-stack)
;;; projection-type-haskell-stack.el ends here
