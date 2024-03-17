;;; projection-dape.el --- Projection integration for `dape' -*- lexical-binding: t; -*-

;; Author: Mohsin Kaleem <mohkale@kisara.moe>
;; Keywords: project, convenience
;; Package-Requires: ((emacs "29.1") (projection "0.1") (dape "0.8"))
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

;; TODO: commentary.

;;; Code:

(require 'dape)
(require 'projection-core)
(require 'projection-artifacts)

(defvar projection-dape--artifact-category-to-major-mode
  '((cmake-executable . c++-mode)
    (cmake-test . c++-mode)
    (go-package . go-mode))
  "Artifact cateogry to mode associations for supported debuggers.
This is used purely to filter available debuggers for a specific debug session.")

(defun projection-dape--select-adapter (artifact)
  "Select a debug adapter compatible with ARTIFACT."
  (if-let* ((type (alist-get 'type artifact))
            (major-mode (alist-get type projection-dape--artifact-category-to-major-mode)))
      (if-let ((available-configs
                (cl-loop for (key . config) in dape-configs
                         when (and (dape--config-mode-p config)
                                   (dape--config-ensure config))
                         collect (dape--config-to-string key nil))))
          (intern
           (or (when (eq (length available-configs) 1)
                 (car available-configs))
               (completing-read (projection--prompt
                                 "Debugger for %s: "
                                 (projection--current-project 'no-error)
                                 (alist-get 'name artifact))
                                available-configs
                                nil 'require-match)))
        (error "No known adapters are available for %S" artifact))
    (error "Do not know which adapters are supported for artifact=%S" artifact)))

(cl-defmethod projection-dape--artifact-type-options (type artifact)
  "Generate dape settings for debug target ARTIFACT.
TYPE is the type field from the ARTIFACT alist."
  (error "Do not know how to derive type=%s debugger config from artifact=%S"
         type artifact))

(cl-defmethod projection-dape--artifact-type-options ((_type (eql 'cmake-executable)) artifact)
  "Generate dape settings for cmake-executable debug target ARTIFACT."
  (let-alist artifact
    `(,@(when .working-directory
          (list :cwd .working-directory))
      :program ,.arg0
      ,@(when .environment
          (list :environment
                (cl-loop for (var . value) in .environment
                         collect (intern (concat ":" var))
                         collect value)))
      ,@(when .argv
          (list :args (apply #'vector .argv))))))

(cl-defmethod projection-dape--artifact-type-options ((_type (eql 'cmake-test)) artifact)
  "Generate dape settings for cmake-test debug target ARTIFACT."
  (projection-dape--artifact-type-options 'cmake-executable artifact))

(cl-defmethod projection-dape--artifact-type-options ((_type (eql 'go-package)) artifact)
  "Generate dape settings for go-package debug target ARTIFACT."
  (let-alist artifact
    `(:program ,.go-package)))

(defun projection-dape--artifact-to-dape-config (artifact)
  "Convert ARTIFACT to a debugger key and a dape debug config string."
  (pcase-let ((`(,key ,config)
               (thread-first
                 (cons (projection-dape--select-adapter artifact)
                       (projection-dape--artifact-type-options
                        (alist-get 'type artifact) artifact))
                 (prin1-to-string)
                 (substring 1 -1)
                 (dape--config-from-string))))
    (cons key (dape--config-eval key config))))



(defun projection-dape--read-debug-targets ()
  "Helper function to read artifacts that are debuggable."
  (projection-artifacts--read
   nil
   (lambda (artifact)
     (alist-get 'debuggable artifact))))

;;;###autoload
(defun projection-dape (dape-config)
  "Interactively select a debug target and run with dape.
Passes DAPE-CONFIG to `dape'."
  (interactive
   (pcase-let* ((artifact (projection-dape--read-debug-targets))
                (`(,key . ,evaled-config)
                 (projection-dape--artifact-to-dape-config artifact)))
     (setq dape-history
           (cons (dape--config-to-string key evaled-config)
                 dape-history))
     (list evaled-config)))
  (dape dape-config))

(provide 'projection-dape)
;;; projection-dape.el ends here
