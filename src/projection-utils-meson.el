;;; projection-utils-meson.el --- Helpers for supporting Meson projects. -*- lexical-binding: t; -*-

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

;; Projection project-type helpers for Meson projects.

;;; Code:

(defgroup projection-type-meson nil
  "Projection Meson project type."
  :group 'projection-types)

(defcustom projection-meson-build-directory "builddir"
  "Build directory for meson project builds."
  :type 'string
  :group 'projection-type-meson)

(defcustom projection-meson-install-directory "install"
  "Install directory for meson project installations."
  :type 'string
  :group 'projection-type-meson)



;; Meson compilation commands.

(defun projection-meson-get-configure-command ()
  "Generate a shell command to run a Meson configure."
  (concat "meson setup "
          (shell-quote-argument projection-meson-build-directory)))

(defun projection-meson-get-build-command ()
  "Generate a shell command to run a Meson build."
  (concat "meson compile -C "
          (shell-quote-argument projection-meson-build-directory)))

(defun projection-meson-get-test-command ()
  "Generate a shell command to run a Meson test."
  (concat "meson test -C "
          (shell-quote-argument projection-meson-build-directory)))

(defun projection-meson-get-install-command ()
  "Generate a shell command to run a Meson installation."
  (format "meson install -C %s --destdir %s"
          (shell-quote-argument projection-meson-build-directory)
          (shell-quote-argument projection-meson-install-directory)))

(provide 'projection-utils-meson)
;;; projection-utils-meson.el ends here
