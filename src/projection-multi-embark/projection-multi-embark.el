;;; projection-multi-embark.el --- `projection-multi' integration for `embark' -*- lexical-binding: t; -*-

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Keywords: project, convenience
;; Package-Requires: ((emacs "29") (projection "0.1") (compile-multi-embark "0.5"))
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

;; Setup projection commands in `projection-multi-embark-command-map'.

;;; Code:

(require 'projection-core)
(require 'projection-commands)
(require 'compile-multi-embark)

(defgroup projection-multi-embark nil
  "Projection integration for `embark' through `compile-multi'."
  :group 'compile-multi
  :group 'projection
  :group 'projection-compile-multi)



(defvar projection-multi-embark-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" (cons "projection-set-command" #'projection-set-command-for-type))
    ;; NOTE: Keep in sync with `projection-map'.
    (define-key map "c" #'projection-set-build-command)
    (define-key map "g" #'projection-set-configure-command)
    (define-key map "t" #'projection-set-test-command)
    (define-key map "r" #'projection-set-run-command)
    (define-key map "p" #'projection-set-run-command)
    (define-key map "k" #'projection-set-package-command)
    (define-key map "i" #'projection-set-install-command)
    map)
  "Command map for `projection-multi-embark'.")

;;;###autoload
(defun projection-multi-embark-setup-command-map ()
  "Setup `projection-multi-embark' command map in relevent embark maps.'"
  (define-key compile-multi-embark-command-map "p" (cons "projection" projection-multi-embark-command-map)))

(provide 'projection-multi-embark)
;;; projection-multi-embark.el ends here
