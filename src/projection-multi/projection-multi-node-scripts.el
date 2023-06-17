;;; projection-multi-node-scripts.el --- Projection integration for `compile-multi' and the NPM/Yarn package.json scripts. -*- lexical-binding: t; -*-

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
;; sources the list of available targets from a node projects scripts config
;; in package.json. It has variants for both npm and yarn.

;;; Code:

(require 'cl-lib)

(require 'projection-core)
(require 'projection-core-log)
(require 'projection-utils)
(require 'projection-multi)
(require 'projection-types)

(defgroup projection-multi-node-scripts nil
  "Helpers for `compile-multi' and node projects with package.json."
  :group 'projection-multi)

(defcustom projection-multi-node-scripts-cache-targets 'auto
  "When true cache the NPM/Yarn script targets of each project."
  :type '(choice
          (const auto :tag "Cache targets and invalidate cache automatically")
          (boolean :tag "Always/Never cache targets"))
  :group 'projection-multi-node-scripts)

(defun projection-multi-node-scripts--targets-from-file (node-package-manager package-json)
  "Read node script targets based on PROJECT-FILE respecting project cache.
NODE-PACKAGE-MANAGER specifies which package manager (npm, yarn, etc.) to fetch
the available run targets through. PACKAGE-JSON is the file that will be used to
invalidate the cache of targets."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   (intern
    (concat "projection-multi-node-" node-package-manager "-script-targets"))
   (cond
    ((eq projection-multi-node-scripts-cache-targets 'auto)
     (projection--cache-modtime-predicate package-json))
    (t projection-multi-node-scripts-cache-targets))
   (apply-partially #'projection-multi-node-scripts--targets-from-file2 node-package-manager)))

(defun projection-multi-node-scripts--targets-from-file2 (node-package-manager)
  "Read node script targets through NODE-PACKAGE-MANAGER."
  (projection--log :debug "Resolving available node %s script targets"
                   node-package-manager)
  (projection--with-shell-command-buffer (concat node-package-manager " run")
    (let (result)
      (save-match-data
        (while (search-forward-regexp
                (rx bol "  " (group (+ (not " "))) eol)
                nil 'noerror)
          (push (match-string 1) result))
        (nreverse result)))))

(defun projection-multi--node-script-targets (project-type node-package-manager)
  "`compile-multi' target generator function for node projects with scripts.
When set the generated targets will be prefixed with PROJECT-TYPE.
NODE-PACKAGE-MANAGER determines which package manager to run scripts with."
  (when (and (file-exists-p "package.json")
             (executable-find node-package-manager 'remote))
    (cl-loop
     for target in (projection-multi-node-scripts--targets-from-file
                    node-package-manager "package.json")
     collect (cons (concat project-type ":" target)
                   (concat node-package-manager " run " (shell-quote-argument target))))))

;;;###autoload
(defun projection-multi-npm-script-targets (&optional project-type)
  "`compile-multi' target generator function for npm projects with scripts.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (projection-multi--node-script-targets
   (or project-type "npm")
   "npm"))

;;;###autoload
(defun projection-multi-compile-npm-scripts ()
  "`compile-multi' wrapper for only npm script targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-npm-script-targets))))

;;;###autoload
(defun projection-multi-yarn-script-targets (&optional project-type)
  "`compile-multi' target generator function for yarn projects with scripts.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (projection-multi--node-script-targets
   (or project-type "yarn")
   "yarn"))

;;;###autoload
(defun projection-multi-compile-yarn-scripts ()
  "`compile-multi' wrapper for only yarn script targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-yarn-script-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (oset projection-project-type-npm compile-multi-targets
        (seq-uniq
         (append
          (oref projection-project-type-npm compile-multi-targets)
          (list #'projection-multi-npm-script-targets)))))

;;;###autoload
(with-eval-after-load 'projection-types
  (oset projection-project-type-yarn compile-multi-targets
        (seq-uniq
         (append
          (oref projection-project-type-yarn compile-multi-targets)
          (list #'projection-multi-yarn-script-targets)))))

(provide 'projection-multi-node-scripts)
;;; projection-multi-node-scripts.el ends here
