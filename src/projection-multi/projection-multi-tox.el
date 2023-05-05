;;; projection-multi-tox.el --- Projection integration for `compile-multi' and the Python Tox project type. -*- lexical-binding: t; -*-

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
;; sources the list of available targets from a tox projects tox configuration.

;;; Code:

(require 'cl-lib)

(require 'projection-core)
(require 'projection-core-log)
(require 'projection-utils)
(require 'projection-multi)

(defgroup projection-multi-tox nil
  "Helpers for `compile-multi' and tox projects."
  :group 'projection-multi)

(defcustom projection-multi-tox-cache-targets t
  "When true cache the Tox targets of each project."
  :type 'boolean
  :group 'projection-multi-tox)

(defcustom projection-multi-tox-include-default-target t
  "When true include a compilation target to run the default tox environments."
  :type 'boolean
  :group 'projection-multi-tox)

(defun projection-multi-tox--targets-from-file (project-file)
  "Read tox targets based on PROJECT-FILE respecting project cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-tox-targets
   (and projection-multi-tox-cache-targets
        (projection--cache-modtime-predicate project-file))
   #'projection-multi-tox--targets-from-file2))

(defun projection-multi-tox--targets-from-file2 ()
  "Read tox targets."
  (projection--log :debug "Resolving available tox targets")
  (when-let ((output
              (or (projection--shell-command-to-string "tox --listenvs-all")
                  (projection--shell-command-to-string "tox --listenvs"))))
    (string-lines output)))

;;;###autoload
(defun projection-multi-tox-targets (&optional project-type)
  "`compile-multi' target generator function for Tox projects.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "tox"))

  (when (file-exists-p "tox.ini")
    (append
     (when projection-multi-tox-include-default-target
       (list `(,(concat project-type "default") . "tox")))
     (cl-loop
      for target in (projection-multi-tox--targets-from-file "tox.ini")
      collect (cons (concat project-type ":" target)
                    (concat "tox -e " (shell-quote-argument target)))))))

;;;###autoload
(defun projection-multi-compile-tox ()
  "`compile-multi' wrapper for only tox targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-tox-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (projection-register-type 'python-tox
    :targets 'projection-multi-tox-targets))

(provide 'projection-multi-tox)
;;; projection-multi-tox.el ends here
