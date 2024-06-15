;;; projection-multi-embark.el --- Integration for `projection-multi' and `embark' -*- lexical-binding: t; -*-

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Keywords: project, convenience
;; Package-Requires: ((emacs "29.1") (projection "0.1") (compile-multi-embark "0.5"))
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



(defmacro projection-multi-embark--set-command-wrapper (type)
  (let ((func-name (intern (concat "projection-multi-embark-set-" (symbol-name type) "-command")))
        (default-func-name (intern (concat "projection-commands-set-" (symbol-name type) "-command"))))
    `(defun ,func-name (command project)
       (interactive
        (list (read-shell-command "Compile command: ")
              (projection--current-project)))
       (funcall (or (when (stringp command)
                      (get-text-property
                       0 'projection-multi-embark--set-command-callback command))
                    #',default-func-name)
                command project))))

;;;###autoload (autoload 'projection-multi-embark-set-build-command "projection-commands" nil t)
(projection-multi-embark--set-command-wrapper build)
;;;###autoload (autoload 'projection-multi-embark-set-configure-command "projection-commands" nil t)
(projection-multi-embark--set-command-wrapper configure)
;;;###autoload (autoload 'projection-multi-embark-set-test-command "projection-commands" nil t)
(projection-multi-embark--set-command-wrapper test)
;;;###autoload (autoload 'projection-multi-embark-set-run-command "projection-commands" nil t)
(projection-multi-embark--set-command-wrapper run)
;;;###autoload (autoload 'projection-multi-embark-set-package-command "projection-commands" nil t)
(projection-multi-embark--set-command-wrapper package)
;;;###autoload (autoload 'projection-multi-embark-set-install-command "projection-commands" nil t)
(projection-multi-embark--set-command-wrapper install)



(defvar projection-multi-embark-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" (cons "projection-set-command" #'projection-commands-set-command-for-type))
    ;; NOTE: Keep in sync with `projection-map'.
    (define-key map "c" #'projection-multi-embark-set-build-command)
    (define-key map "g" #'projection-multi-embark-set-configure-command)
    (define-key map "t" #'projection-multi-embark-set-test-command)
    (define-key map "r" #'projection-multi-embark-set-run-command)
    (define-key map "p" #'projection-multi-embark-set-run-command)
    (define-key map "k" #'projection-multi-embark-set-package-command)
    (define-key map "i" #'projection-multi-embark-set-install-command)
    map)
  "Command map for `projection-multi-embark'.")

;;;###autoload
(defun projection-multi-embark-setup-command-map ()
  "Setup `projection-multi-embark' command map in relevent embark maps."
  (define-key compile-multi-embark-command-map "p" (cons "projection" projection-multi-embark-command-map)))

(provide 'projection-multi-embark)
;;; projection-multi-embark.el ends here
