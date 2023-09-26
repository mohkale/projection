;;; projection-multi-meson.el --- Projection integration for `compile-multi' and the Meson project type. -*- lexical-binding: t; -*-

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
;; sources the list of available targets from a Meson projects build config.

;;; Code:

(require 'cl-lib)
(require 'f)
(require 'json)

(require 'projection-core-log)
(require 'projection-utils-meson)
(require 'projection-multi)
(require 'projection-types)

(defgroup projection-multi-meson nil
  "Helpers for `compile-multi' and Meson projects."
  :group 'projection-multi)

(defcustom projection-multi-meson-cache-targets 'auto
  "When true cache the Meson targets of each project."
  :type '(choice
          (const :tag "Cache targets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache targets"))
  :group 'projection-multi-meson)

(defun projection-multi-meson--targets ()
  "Read Meson targets respecting project cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-meson-targets
   (pcase projection-multi-meson-cache-targets
    ('auto (projection--meson-configure-modtime-p))
    (_ projection-multi-meson-cache-targets))
   #'projection-multi-meson--targets2))

(projection--declare-cache-var
  'projection-multi-meson-targets
  :title "Multi Meson targets"
  :category "Meson"
  :description "Multi Meson command targets"
  :hide t)

(defun projection-multi-meson--targets2 ()
  "Read Meson targets."
  (thread-last
    (projection-multi-meson--parse-targets)
    (alist-get 'targets)
    (mapcar (apply-partially #'alist-get 'name))))

(defun projection-multi-meson--parse-targets ()
  "Query targets metadata from Meson."
  (projection--with-shell-command-buffer
    (projection--join-shell-command
     `("meson" "introspect"
       ,(projection-meson--build-directory)
       "--targets" "--force-object-output"))
    (condition-case err
        (let ((json-array-type 'list)) (json-read))
      (json-readtable-error
       (projection--log :error "error while querying Meson targets %s." (cdr err))))))



;;;###autoload
(defun projection-multi-meson-targets (&optional project-type)
  "`compile-multi' target generator function for Meson projects.
When set the generated targets will be prefixed with PROJECT-TYPE."
  (setq project-type (or project-type "meson"))

  (append
   `((,(concat project-type ":clean") . ,(projection-meson-get-build-command "--clean")))
   (cl-loop
    for target in (projection-multi-meson--targets)
    collect (cons (concat project-type ":" target)
                  (projection-meson-get-build-command target)))))

;;;###autoload
(defun projection-multi-compile-meson ()
  "`compile-multi' wrapper for only Meson targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-meson-targets))))

;;;###autoload
(with-eval-after-load 'projection-types
  (oset projection-project-type-meson compile-multi-targets
        (seq-uniq
         (append
          (oref projection-project-type-meson compile-multi-targets)
          (list #'projection-multi-meson-targets)))))

(provide 'projection-multi-meson)
;;; projection-multi-meson.el ends here
