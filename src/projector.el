;;; projector.el --- Project specific compilation commands -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mohsin Kaleem

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Keywords: project
;; Package-Requires: ((emacs "29.0") (project "0.8.1") (compile-multi "0.1"))
;; Version: 0.1
;; Homepage: https://github.com/mohkale/projector

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

(require 'projector-core)
(require 'projector-types)

(defgroup projector nil
  "Project specific helper commands."
  :group 'project
  :link '(url-link :tag "GitHub" "https://github.com/mohkale/projector"))

(defvar-keymap projector-map
  :doc "Keymap for projector project-management bindings."
  ;; `projector-core'
  "I" 'projector-show-project-info
  "DEL" 'projector-reset-project-cache
  ;; `projector-find'
  "TAB" 'projector-find-other-file
  "o"   'projector-find-other-file
  ;; `projector-hook'
  "h" 'projector-hook
  "H" 'projector-hook-clear
  ;; `projector-ibuffer'
  "m" 'projector-ibuffer
  ;; `projector-commands'.
  "c" 'projector-build-project
  "g" 'projector-configure-project
  "t" 'projector-test-project
  "r" 'projector-run-project
  "p" 'projector-run-project
  "k" 'projector-package-project
  "i" 'projector-install-project
  "RET" 'projector-project-command
  ;; `projector-multi'
  "SPC" 'projector-multi-compile)

(provide 'projector)
;;; projector.el ends here
