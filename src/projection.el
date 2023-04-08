;;; projection.el --- Project specific compilation commands -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mohsin Kaleem

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Keywords: project, convenience
;; Package-Requires: ((emacs "28.1") (project "0.9.8") (compile-multi "0.1"))
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

(defvar projection-map
  (let ((map (make-sparse-keymap)))
    ;; `projection-core'
    (define-key map "I" 'projection-show-project-info)
    (define-key map "DEL" 'projection-reset-project-cache)
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
    (define-key map "c" 'projection-build-project)
    (define-key map "g" 'projection-configure-project)
    (define-key map "t" 'projection-test-project)
    (define-key map "r" 'projection-run-project)
    (define-key map "p" 'projection-run-project)
    (define-key map "k" 'projection-package-project)
    (define-key map "i" 'projection-install-project)
    (define-key map "RET" 'projection-project-command)
    ;; `projection-multi'
    (define-key map "SPC" 'projection-multi-compile)
    map)
  "Keymap for projection project-management bindings.")

(provide 'projection)
;;; projection.el ends here
