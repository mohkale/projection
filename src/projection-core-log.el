;;; projection-core-log.el --- Debug logging support for `projection' -*- lexical-binding: t; -*-

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

;; Define a helper function and module for logging various at various points
;; in `projection' processing.

;;; Code:

(defcustom projection-log-buffer " *Projection log*"
  "Name of the projection log buffer.
Set to nil to disable all debug logging."
  :group 'projection
  :type '(optional string))

(defmacro projection--log (level msg &rest args)
  "Log, at level LEVEL, the message MSG formatted with ARGS.
LEVEL is passed to `display-warning', which is used to display
the warning. If this form is included in a file,
the generated warning contains an indication of the file that
generated it."
  ;; Adapted from `flymake-log'.
  (let* ((file (if (fboundp 'macroexp-file-name)
                   (macroexp-file-name)
                 (and (not load-file-name)
                      (bound-and-true-p byte-compile-current-file))))
         (sublog (if (stringp file)
                     (intern
                      (file-name-nondirectory
                       (file-name-sans-extension file))))))
    `(projection--log-1 ,level ',sublog ,msg ,@args)))

(defun projection--log-1 (level sublog msg &rest args)
  "Do actual work for `projection--log'.
See `projection--log' for a description of LEVEL MSG and ARGS.
SUBLOG is the name of the file that produced the log."
  (when projection-log-buffer
    (let ((warning-minimum-level :emergency)
          (warning-type-format
           (format " [%s]" (or sublog 'projection))))
      (ignore warning-minimum-level)
      (display-warning
       (list 'projection sublog)
       (apply #'format-message msg args)
       level
       projection-log-buffer)))
  nil)

(provide 'projection-core-log)
;;; projection-core-log.el ends here
