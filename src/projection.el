;;; projection.el --- Project specific compilation commands -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mohsin Kaleem

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Keywords: project
;; Package-Requires: ((emacs "29.0") (project "0.8.1") (compile-multi "0.1"))
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

;; This library builds on standard Emacs project.el providing project specific
;; helper commands for compilation, testing and running a project. It acts as
;; a substitute for `projectile' wherever possible.

;;; Code:

(require 'projection-core)
(require 'projection-types)

(defgroup projection nil
  "Project specific helper commands."
  :group 'project
  :link '(url-link :tag "GitHub" "https://github.com/mohkale/projection"))

(defvar-keymap projection-map
  :doc "Keymap for projection project-management bindings."
  ;; `projection-core'
  "I" 'projection-show-project-info
  "DEL" 'projection-reset-project-cache
  ;; `projection-find'
  "TAB" 'projection-find-other-file
  "o"   'projection-find-other-file
  ;; `projection-hook'
  "h" 'projection-hook
  "H" 'projection-hook-clear
  ;; `projection-ibuffer'
  "m" 'projection-ibuffer
  ;; `projection-commands'.
  "c" 'projection-build-project
  "g" 'projection-configure-project
  "t" 'projection-test-project
  "r" 'projection-run-project
  "p" 'projection-run-project
  "k" 'projection-package-project
  "i" 'projection-install-project
  "RET" 'projection-project-command
  ;; `projection-multi'
  "SPC" 'projection-multi-compile)

(provide 'projection)
;;; projection.el ends here
