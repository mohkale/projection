;;; projection-multi-rustic.el --- Projection integration for `compile-multi' and the rustic projects. -*- lexical-binding: t; -*-

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

;; This library exposes a target generation function for `compile-multi' which
;; generates available targets from the `rustic' package when it is installed.
;; This simply provides convenient access to helper functions from `rustic'
;; through `compile-multi'.

;;; Code:

(require 'cl-lib)

(require 'projection-multi)
(require 'projection-types)

;;;###autoload
(defun projection-multi-rustic-targets (&optional project-type)
  "`compile-multi' target generator function for `rustic' projects.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "rustic"))

  (when (featurep 'rustic)
    (cl-loop
     for (command . label) in
     `((rustic-compile . "compile")
       (rustic-cargo-add . "cargo:add")
       (rustic-cargo-rm . "cargo:remove")
       (rustic-cargo-upgrade . "cargo:upgrade")
       (rustic-cargo-fmt . "cargo:format")
       (rustic-cargo-fmt . "format:project")
       (rustic-cargo-run . "cargo:run")
       (rustic-cargo-comint-run . "cargo:run:comint")
       (rustic-cargo-plain-run . "cargo:run:plain")
       (rustic-cargo-outdated . "cargo:outdated")
       (rustic-cargo-expand . "cargo:macro-expand")
       (rustic-cargo-bench . "cargo:bench")
       (rustic-cargo-build-doc . "cargo:doc:build")
       (rustic-cargo-doc . "cargo:doc:open")
       (rustic-cargo-lints . "cargo:lints")
       (rustic-cargo-install . "cargo:install")
       (rustic-cargo-update . "cargo:update")
       (rustic-cargo-clippy . "cargo:clippy")
       (rustic-cargo-clippy . "cargo:clippy:fix")
       ,@(when (derived-mode-p 'rustic-mode)
           `((rustic-format-buffer . "format:buffer")
             (rustic-format-file . "format:file")
             (rustic-cargo-add-missing-dependencies . "cargo:add-deps")
             ,@(when (region-active-p)
                 `((rustic-format-region . "format:region"))))))
     collect (cons (concat project-type ":" label) command))))

;;;###autoload
(defun projection-multi-compile-rustic ()
  "`compile-multi' wrapper for only `rustic' targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-rustic-targets))))

;;;###autoload
(defvar projection-project-type-rust-cargo)
;;;###autoload
(with-eval-after-load 'projection-types
  (projection-type-append-compile-multi-targets projection-project-type-rust-cargo
    #'projection-multi-rustic-targets))

(provide 'projection-multi-rustic)
;;; projection-multi-rustic.el ends here
