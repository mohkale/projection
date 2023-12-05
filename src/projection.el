;;; projection.el --- Project type support for `project' -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mohsin Kaleem

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Keywords: project, convenience
;; Package-Requires: ((emacs "29.1") (project "0.9.8") (compat "29.1.4.1") (f "0.20") (s "1.13"))
;; Version: 0.1
;; Homepage: https://github.com/mohkale/projection

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

;; This package builds atop standard Emacs project.el by providing various
;; utility commands and support for individual project types.
;;
;; Different projects are interacted with in different ways but many have
;; common tasks such as configuring, building or testing. Projection aims
;; to be a generic wrapper around these project types such that you can
;; configure (or use the builtin) commands for various project types and
;; invoke them seamlessly through a common command like
;; `projection-configure-project'.
;;
;; Beyond just project specific commands projection also maintains utility
;; commands or integrations between projection and other packages such as
;; `projection-multi' or `projection-ibuffer'.
;;
;; Projection takes heavy inspiration from `projectile'.

;;; Code:

(require 'compat)
(require 'projection-core)
(require 'projection-types)

(defgroup projection nil
  "Project specific helper commands."
  :group 'project
  :link '(url-link :tag "GitHub" "https://github.com/mohkale/projection"))

;;;###autoload
(defvar projection-per-project-type-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" '("CMake" . nil))
    (define-key map "cp" 'projection-cmake-set-preset)
    (define-key map "cb" 'projection-cmake-set-build-type)
    (define-key map "cd" 'projection-cmake-clear-build-directory)

    (define-key map "g" '("Golang" . nil))
    (define-key map "gp" 'projection-golang-set-package)

    (define-key map "m" '("Meson" . nil))
    (define-key map "mb" 'projection-meson-set-build-type)
    (define-key map "mc" 'projection-meson-set-build-option)

    map)
  "Keymap containing commands specific to each project type.")

;;;###autoload
(defvar projection-map
  (let ((map (make-sparse-keymap)))
    (define-key map "SPC" '("Extensions" . (projection-per-project-type-map)))
    ;; `projection-core'
    (define-key map "1" 'projection-set-primary-project-type)
    (define-key map "2" 'projection-update-extra-project-types)
    (define-key map "I" 'projection-show-project-info)
    (define-key map "DEL" 'projection-cache-clear)
    ;; `projection-find'
    (define-key map "TAB" 'projection-find-other-file)
    (define-key map "o"   'projection-find-other-file)
    ;; `projection-hook'
    (define-key map "h" 'projection-hook)
    (define-key map "H" 'projection-hook-clear)
    ;; `projection-ibuffer'
    (define-key map "m" 'ibuffer-projection-current-project)
    (define-key map "M" 'ibuffer-projection-toggle-filter-groups)
    ;; `projection-commands'.
    (define-key map "c" 'projection-commands-build-project)
    (define-key map "g" 'projection-commands-configure-project)
    (define-key map "t" 'projection-commands-test-project)
    (define-key map "r" 'projection-commands-run-project)
    (define-key map "p" 'projection-commands-run-project)
    (define-key map "k" 'projection-commands-package-project)
    (define-key map "i" 'projection-commands-install-project)
    map)
  "Keymap for projection project-management bindings.")

(provide 'projection)
;;; projection.el ends here
