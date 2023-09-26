;;; projection-multi-meson.el --- Projection integration for `compile-multi' and the Meson project type. -*- lexical-binding: t; -*-

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
;; sources the list of available targets from a Meson projects build config.

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'json)

(require 'projection-core-log)
(require 'projection-utils-meson)
(require 'projection-multi)
(require 'projection-types)

(defun projection-multi-meson--targets ()
  "Read Meson targets."
  (thread-last
    (projection-meson--code-model)
    (alist-get 'targets)
    (mapcar (apply-partially #'alist-get 'name))))



;;;###autoload
(defun projection-multi-meson-targets (&optional project-type)
  "`compile-multi' target generator function for Meson projects.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "meson"))

  (append
   `((,(concat project-type ":clean") . ,(projection-meson-get-build-command "--clean")))
   (cl-loop
    for target in (projection-multi-meson--targets)
    collect (cons (concat project-type ":" target)
                  (projection-meson-get-build-command target)))))

;;;###autoload
(defun projection-multi-compile-meson ()
  "`compile-multi' wrapper for only Meson targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-meson-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (oset projection-project-type-meson compile-multi-targets
        (seq-uniq
         (append
          (oref projection-project-type-meson compile-multi-targets)
          (list #'projection-multi-meson-targets)))))

(provide 'projection-multi-meson)
;;; projection-multi-meson.el ends here
