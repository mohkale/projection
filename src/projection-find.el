;;; projection-find.el --- Jump between related files in a project. -*- lexical-binding: t; -*-

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

;; Facilities for jumping to related files within a project. For example this can
;; be used to jump between C++ declaration and implementation files. It can also
;; respect the project specific test configurations from `projection-types' to be
;; able to jump from implementation to test files and vice versa.

;;; Code:

(require 'project)
(require 'projection-core)

(defgroup projection-find nil
  "Project specific `find-file' helpers."
  :group 'projection)

(defcustom projection-find-other-file-suffix
  '(;; handle C/C++ extensions
    ("cpp" "h" "hpp" "ipp")
    ("ipp" "h" "hpp" "cpp")
    ("hpp" "h" "ipp" "cpp" "cc")
    ("cxx" "h" "hxx" "ixx")
    ("ixx" "h" "hxx" "cxx")
    ("hxx" "h" "ixx" "cxx")
    ("c"   "h")
    ("m"   "h")
    ("mm"  "h")
    ("h"   "c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm")
    ("cc"  "h" "hh" "hpp")
    ("hh"  "cc")

    ;; OCaml extensions
    ("ml" "mli")
    ("mli" "ml" "mll" "mly")
    ("mll" "mli")
    ("mly" "mli")
    ("eliomi" "eliom")
    ("eliom" "eliomi")

    ;; vertex shader and fragment shader extensions in glsl
    ("vert" "frag")
    ("frag" "vert")

    ;; handle files with no extension
    (nil    "lock" "gpg")
    ("lock" "")
    ("gpg"  ""))
  "Alist associating related files in a project by extension.
Configures relationships between files with similar base-names and different
extensions. For example foo.h is related to foo.cpp and can be jumped between
each other with `projection-find-other-file' by adding the following mappings
to this configuration:

    ((\"h\" \"cpp\")
     (\"cpp\" \"h\"))

In many cases the mapping between extensions should be reciprocal to ensure
you can jump between them from either file but this isn't required."
  :group 'projection-find
  :type '(alist :key-type string :value-type (list string)))

(defun projection-find--related-extensions (initial-extension)
  "Get list of file extensions related to INITIAL-EXTENSION.
Looks for extensions based on `projection-find-other-file-suffix'."
  (let ((extensions (make-hash-table :test #'equal))
        (searched-extensions (make-hash-table :test #'equal)))
    (puthash initial-extension t searched-extensions)
    (dolist (extention (cdr (assoc nil projection-find-other-file-suffix)))
      (puthash extention t extensions))

    (while (> (hash-table-count searched-extensions) 0)
      (let ((ext (car (hash-table-keys searched-extensions))))
        (remhash ext searched-extensions)
        (puthash ext t extensions)
        (cl-loop for ext in (cdr (assoc ext projection-find-other-file-suffix #'string-equal))
                 when (and ext (not (gethash ext extensions)))
                   do (puthash ext t searched-extensions))))

    (sort (hash-table-keys extensions) #'string<)))

(defun projection-find--related-file-basenames
    (file-name test-prefixes test-suffixes)
  "Get list of basenames for other-files to FILE-NAME.
TEST-PREFIXES and TEST-SUFFIXES are possible prefix and suffixes attached
to files alongside possible file-extension combinations to determine a test
file."
  (let* ((basename (file-name-nondirectory file-name))
         (extension (file-name-extension basename))
         (basename-no-ext
          (substring basename 0 (- (1+ (length extension)))))
         (related-extensions
          (projection-find--related-extensions extension))
         (related-basenames (make-hash-table :test #'equal)))
    ;; Prune out the test-prefix or test-suffix to ensure we have the original
    ;; base-name.
    (catch 'done
      (dolist (prefix test-prefixes)
        (when (string-prefix-p prefix basename-no-ext)
          (setq basename (substring basename (length prefix))
                basename-no-ext (substring basename-no-ext (length prefix)))
          (throw 'done nil)))
      (dolist (suffix test-suffixes)
        (when (string-suffix-p suffix basename-no-ext)
          (setq basename (concat (substring basename-no-ext 0 (- (length suffix)))
                                 extension)
                basename-no-ext (substring basename-no-ext 0 (- (length suffix))))
          (throw 'done nil))))

    (when (> (length basename-no-ext) 0)
      (dolist (extension related-extensions)
        (when (> (length extension) 0)
          (setq extension (concat "." extension)))
        ;; File name with just the extension added on.
        (puthash (concat basename-no-ext extension) t related-basenames)
        ;; Prefix the base-name with test-suffixes and then extension.
        (dolist (suffix test-suffixes)
          (puthash (concat basename-no-ext suffix extension) t related-basenames))
        ;; Suffix the base-name with test-suffixes and then extension.
        (dolist (prefix test-prefixes)
          (puthash (concat prefix basename-no-ext extension) t related-basenames))))
    related-basenames))

(defun projection-find--other-file-list (project file-name)
  "Get list of other files for the FILE-NAME in PROJECT."
  (unless (file-name-absolute-p file-name)
    (expand-file-name file-name (project-root project)))

  (let* ((project-config (projection-project-type (project-root project)))
         ;; Determine related file-names for the target file-name.
         (other-file-basenames
          (projection-find--related-file-basenames
           file-name
           (oref project-config test-prefix)
           (oref project-config test-suffix)))
         other-files)
    (dolist (file (project-files project))
      (when (gethash (file-name-nondirectory file) other-file-basenames)
        (push file other-files)))
    ;; Ensure current file-name is included in the other file list.
    (when (and (file-exists-p file-name)
               (not (gethash (file-name-nondirectory file-name)
                             other-file-basenames)))
      (push file-name other-files))
    ;; Return consistently ordered list of files.
    other-files))

(defun projection-find--other-file (select-interactively)
  "Select another file to jump to for `projection-find-other-file'.
See related function for a description of SELECT-INTERACTIVELY."
  (let* ((project (projection--current-project))
         (files (projection-find--other-file-list
                 project
                 (or buffer-file-name
                     (buffer-name))))
         ;; Existing position of the current file in the other-file list.
         (current-file-pos
          (when buffer-file-name
            (seq-position files buffer-file-name #'string-equal)))
         ;; Position of the next file in the other-file list.
         (other-file-pos (or (when current-file-pos
                               (unless (equal current-file-pos (1- (length files)))
                                 (1+ current-file-pos)))
                           0))
         ;; Other-files not including the current file.
         (files-not-current
          (if current-file-pos
              (append
               (seq-take files current-file-pos)
               (nthcdr   (1+ current-file-pos) files))
            (seq-copy files))))
    (cond
     ((not files-not-current)
      (error "No other files found"))
     (select-interactively
      (let ((default-directory (project-root project)))
        ;; Re-order to push the next file we would've switched to, to the top of
        ;; the list of candidates and then make all paths relative to the project
        ;; root.
        (setq files-not-current
              (cl-loop for file in
                       (append (nthcdr   current-file-pos files-not-current)
                               (seq-take files-not-current current-file-pos))
                       with relative-file = nil
                       do (setq relative-file (file-relative-name file))
                       if (string-prefix-p ".." relative-file)
                         collect file
                       else
                         collect relative-file))

        (expand-file-name
         (completing-read
          (projection--prompt "Find other file: " project)
          (projection-completion--completion-table
           :candidates files-not-current
           :cycle-sort-function #'identity
           :display-sort-function #'identity)
          nil t nil 'file-name-history))))
     ;; Select the next file relative to the current one
     (t
      (nth other-file-pos files)))))

;;;###autoload
(defun projection-find-other-file (&optional select-interactively)
  "Switch between similar files to the current file in this project.
This function will huerestically determine all files in the project similar
to the current file and then `find-file' it. For example this can be used to
switch between C++ header and implementation files assuming the two have the
same basename and a different extension. Similarly this function also includes
any files with test suffixes or prefixes associated with the current project
type. See `projection-type' and `projection-find-other-file-suffix' for
some of the options that impact file resolution.

The order of files cycled from this function is deterministic, and invoking it
repeatedly should cycle between related project files. When invoked with
SELECT-INTERACTIVELY and there's more than one possible file this function could
switch to then you will be dropped into a `completing-read' session with all
possible files and the first match being the one you would've switched to if
SELECT-INTERACTIVELY is not set."
  (interactive "P")
  (when-let ((file
              (projection-find--other-file select-interactively)))
    (funcall #'find-file file)))

(provide 'projection-find)
;;; projection-find.el ends here
