;;; projection-multi-poetry-poe.el --- Projection integration for `compile-multi' and the Poetry project type with the poe command backend. -*- lexical-binding: t; -*-

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
;; sources the list of available targets from a pytests discover tests feature.

;;; Code:

(require 'projection-types)
(require 'projection-core)
(require 'projection-utils)
(require 'projection-multi)

(defgroup projection-multi-pytest nil
  "Helpers for `compile-multi' and python projects using pytest."
  :group 'projection-multi)

(defcustom projection-multi-pytest-executable "pytest"
  "Path to a pytest executable used to query available tests."
  :type 'string)

(defconst projection-multi-pytest--args
  '("--disable-pytest-warnings" "--collect-only" "--quiet"))

(defcustom projection-multi-pytest-cache-targets t
  "When true cache the Pytest targets of each project."
  :type '(boolean :tag "Always/Never cache targets"))

(defun projection-multi-python--tests ()
  "Read Pytest test-cases respecting project cache."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-multi-poetry-poe-targets
   projection-multi-pytest-cache-targets
   #'projection-multi-python--tests2))

(projection--declare-cache-var
  'projection-multi-poetry-poe-targets
  :title "Multi Pytest tests"
  :category "Pytest"
  :hide t)

(defun projection-multi-python--tests2 ()
  "Read Pytest test-cases."
  (projection--log :debug "Resolving available Pytest tests")

  (projection--with-shell-command-buffer
    (projection--join-shell-command `(,projection-multi-pytest-executable
                                      ,@projection-multi-pytest--args))
    (save-match-data
      (cl-loop
       while (re-search-forward (rx bol
                                    (or
                                     (and
                                      (group-n 1 (+ any)) "::"
                                      (group-n 2 (+ any)) "::"
                                      (group-n 3 (+ any)))
                                     (and
                                      (group-n 1 (+ any)) "::"
                                      (group-n 3 (+ any))))
                                    eol)
                                nil 'no-error)
       ;; Collect parameterised test without params first.
       with param-test-cache = (make-hash-table :test 'equal)
       with param-index = nil
       do (setq param-index (seq-position (match-string 3) 91))
       when param-index
         with param-free-test = nil
         do (setq param-free-test (substring (match-string 3) 0 param-index))
         and unless (gethash param-free-test param-test-cache)
           do (puthash param-free-test t param-test-cache)
           and collect `(,(match-string 1)
                         ,(match-string 2)
                         ,param-free-test)

       collect `(;; Each entry is (FILE (? CLASS) TEST)
                 ,(match-string 1)
                 ,(match-string 2)
                 ,(match-string 3))
       ))))

;;;###autoload
(defun projection-multi-pytest-targets ()
  "`compile-multi' target generator function for Pytest projects."
  (let ((prefix-file "pytest-file")
        (prefix-class "pytest-class")
        (prefix-test "pytest-test")
        (file-cache (make-hash-table :test 'equal))
        (file+class-cache (make-hash-table :test 'equal)))
    (cl-loop
     for (file class test) in (projection-multi-python--tests)
     unless (gethash file file-cache)
       do (puthash file t file-cache)
       and collect (cons (concat prefix-file ":" file)
                         (projection--join-shell-command
                          `(,projection-multi-pytest-executable ,file)))

     with file+class = nil do (setq file+class (concat file "::" class))
     when (and class
               (not (gethash file+class file+class-cache)))
       do (puthash file+class t file+class-cache)
       and collect (cons (concat prefix-class ":" class)
                         (projection--join-shell-command
                          `(,projection-multi-pytest-executable ,(concat file "::" class))))

     collect (cons (concat prefix-test ":" (when class (concat class "#")) test)
                   (projection--join-shell-command
                    `(,projection-multi-pytest-executable
                      ,(concat file "::"
                               (when class (concat class "::"))
                               test)))))))

;;;###autoload
(defun projection-multi-pytest ()
  "`compile-multi' wrapper for only Pytest targets."
  (interactive)
  (projection-multi-compile--run
   (projection--current-project 'no-error)
   `((t ,#'projection-multi-pytest-targets))))

;;;###autoload
(defvar projection-project-type-django)
;;;###autoload
(defvar projection-project-type-python-pip)
;;;###autoload
(defvar projection-project-type-python-pkg)
;;;###autoload
(defvar projection-project-type-python-toml)
;;;###autoload
(defvar projection-project-type-python-tox)
;;;###autoload
(defvar projection-project-type-python-pipenv)
;;;###autoload
(defvar projection-project-type-python-poetry)
;;;###autoload
(with-eval-after-load 'projection-types
  (projection-type-append-compile-multi-targets
    (list projection-project-type-django
          projection-project-type-python-pip
          projection-project-type-python-pkg
          projection-project-type-python-toml
          projection-project-type-python-tox
          projection-project-type-python-pipenv
          projection-project-type-python-poetry)
    #'projection-multi-pytest-targets))

(provide 'projection-multi-pytest)
;;; projection-multi-pytest.el ends here
