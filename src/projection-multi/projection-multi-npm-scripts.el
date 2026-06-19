;;; projection-multi-npm-scripts.el --- Projection integration for `compile-multi' and the NPM package.json scripts. -*- lexical-binding: t; -*-

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
;; sources the list of available targets from a npm projects scripts config
;; in package.json.

;;; Code:

(require 'cl-lib)

(require 'projection-core)
(require 'projection-utils)
(require 'projection-multi)
(require 'projection-types)

(defgroup projection-multi-npm-scripts nil
  "Helpers for `compile-multi' and npm projects with package.json."
  :group 'projection-multi)

(defcustom projection-multi-npm-scripts-cache-targets 'auto
  "When true cache the NPM script targets of each project."
  :type '(choice
          (const :tag "Cache targets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache targets"))
  :group 'projection-multi-npm-scripts)

(defun projection-multi-npm-scripts--targets-from-file (package-json)
  "Read npm script targets based on PROJECT-FILE respecting project cache.
PACKAGE-JSON is the file that will be used to invalidate the cache of targets."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-npm-script-targets
   (cond
    ((eq projection-multi-npm-scripts-cache-targets 'auto)
     (projection--cache-modtime-predicate package-json))
    (t projection-multi-npm-scripts-cache-targets))
   #'projection-multi-npm-scripts--targets-from-file2))

(projection--declare-cache-var
  'projection-multi-npm-script-targets
  :title "Multi NPM script targets"
  :category "NPM"
  :description "NPM script targets associated with this project"
  :hide t)

(defun projection-multi-npm-scripts--targets-from-file2 ()
  "Read npm script targets."
  (projection--log :debug "Resolving available npm script targets")
  (projection--with-shell-command-buffer "npm run"
    (let (result)
      (save-match-data
        (while (search-forward-regexp
                (rx bol (repeat 2 space) (group (+ (not space))) eol)
                nil 'noerror)
          (push (match-string 1) result)))
      (nreverse result))))

;;;###autoload
(defun projection-multi-npm-script-targets (&optional project-type)
  "`compile-multi' target generator function for npm projects with scripts.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (or project-type (setq project-type "npm"))

  (when (and (file-exists-p "package.json")
             (executable-find "npm" 'remote))
    (cl-loop
     for target in (projection-multi-npm-scripts--targets-from-file "package.json")
     collect (cons (concat project-type ":" target)
                   (concat "npm run " (shell-quote-argument target))))))

;;;###autoload
(defun projection-multi-compile-npm-scripts ()
  "`compile-multi' wrapper for only npm script targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-npm-script-targets))))

;;;###autoload
(defvar projection-project-type-npm)
;;;###autoload
(with-eval-after-load 'projection-types
  (projection-type-append-compile-multi-targets projection-project-type-npm
    #'projection-multi-npm-script-targets))

(provide 'projection-multi-npm-scripts)
;;; projection-multi-npm-scripts.el ends here
