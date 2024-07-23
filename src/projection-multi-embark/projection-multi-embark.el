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



(defconst projection-multi-embark--custom-property
  'projection-set-command-callback)

(defmacro projection-multi-embark--set-command-wrapper (type)
  (let ((func-name (intern (concat "projection-multi-embark-set-" (symbol-name type) "-command-dwim")))
        (raw-func-name (intern (concat "projection-multi-embark-set-" (symbol-name type) "-command")))
        (default-func-name (intern (concat "projection-commands-set-" (symbol-name type) "-command"))))
    `(progn
       (defun ,func-name (command &optional set-raw)
         ,(format "Set %s command for the current project to COMMAND.
Command should be a shell command string. As a special case COMMAND if
having a property `%s' will have that property called with key-args of
:type, :command and :project. For convenience all other text-properties
of COMMAND will be passed as keyword arguments to the special property.

Pass SET-RAW to always set `%s' command to COMMAND directly."
                  type projection-multi-embark--custom-property type)
         (let ((project (projection--current-project)))
           (if-let ((should-not-set-raw (not set-raw))
                    (command-is-string (stringp command))
                    (set-command-property
                     (get-text-property 0 projection-multi-embark--custom-property command)))
               (apply set-command-property
                      (append
                       (list :type ',type
                             :command command
                             :project project)
                       (cl-loop for prop being the elements of (text-properties-at 0 command)
                                using (index i)
                                when (eq (% i 2) 0)
                                  collect (intern (concat ":" (symbol-name prop)))
                                else
                                  collect prop)))
             (funcall #',default-func-name command project))))

       (defun ,raw-func-name (command)
         ,(format "Proxy for %s with SET-RAW being always true."
                  func-name)
         (,func-name command 'set-raw)))))

;;;###autoload (progn (autoload 'projection-multi-embark-set-build-command "projection-commands" nil t) (autoload 'projection-multi-embark-set-build-command-dwim "projection-commands" nil t))
(projection-multi-embark--set-command-wrapper build)
;;;###autoload (progn (autoload 'projection-multi-embark-set-configure-command "projection-commands" nil t) (autoload 'projection-multi-embark-set-configure-command-dwim "projection-commands" nil t))
(projection-multi-embark--set-command-wrapper configure)
;;;###autoload (progn (autoload 'projection-multi-embark-set-test-command "projection-commands" nil t) (autoload 'projection-multi-embark-set-test-command-dwim "projection-commands" nil t))
(projection-multi-embark--set-command-wrapper test)
;;;###autoload (progn (autoload 'projection-multi-embark-set-run-command "projection-commands" nil t) (autoload 'projection-multi-embark-set-run-command-dwim "projection-commands" nil t))
(projection-multi-embark--set-command-wrapper run)
;;;###autoload (progn (autoload 'projection-multi-embark-set-package-command "projection-commands" nil t) (autoload 'projection-multi-embark-set-package-command-dwim "projection-commands" nil t))
(projection-multi-embark--set-command-wrapper package)
;;;###autoload (progn (autoload 'projection-multi-embark-set-install-command "projection-commands" nil t) (autoload 'projection-multi-embark-set-install-command-dwim "projection-commands" nil t))
(projection-multi-embark--set-command-wrapper install)



(defvar projection-multi-embark-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" (cons "projection-set-command" #'projection-commands-set-command-for-type))
    ;; NOTE: Keep in sync with `projection-map'.
    (define-key map "c" #'projection-multi-embark-set-build-command-dwim)
    (define-key map "C" #'projection-multi-embark-set-build-command)
    (define-key map "g" #'projection-multi-embark-set-configure-command-dwim)
    (define-key map "G" #'projection-multi-embark-set-configure-command)
    (define-key map "t" #'projection-multi-embark-set-test-command-dwim)
    (define-key map "T" #'projection-multi-embark-set-test-command)
    (define-key map "r" #'projection-multi-embark-set-run-command-dwim)
    (define-key map "R" #'projection-multi-embark-set-run-command)
    (define-key map "p" #'projection-multi-embark-set-run-command-dwim)
    (define-key map "P" #'projection-multi-embark-set-run-command)
    (define-key map "k" #'projection-multi-embark-set-package-command-dwim)
    (define-key map "K" #'projection-multi-embark-set-package-command)
    (define-key map "i" #'projection-multi-embark-set-install-command-dwim)
    (define-key map "I" #'projection-multi-embark-set-install-command)
    map)
  "Command map for `projection-multi-embark'.")

;;;###autoload
(defun projection-multi-embark-setup-command-map ()
  "Setup `projection-multi-embark' command map in relevent embark maps."
  (define-key compile-multi-embark-command-map "p" (cons "projection" projection-multi-embark-command-map)))

(provide 'projection-multi-embark)
;;; projection-multi-embark.el ends here
