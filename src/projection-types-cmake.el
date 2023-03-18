;;; projection-types-cmake.el --- Projection project type definition for CMake. -*- lexical-binding: t; -*-

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

;; TODO

;;; Code:

(require 'json)

(require 'projection-core)
(require 'projection-utils)
(require 'projection-multi-cmake)



;; CMake reading presets.

(defconst projection-cmake-preset-files
  '("CMakePresets.json" "CMakeUserPresets.json")
  "List of files configuring CMake presets.")

(defcustom projection-cmake-cache-presets t
  "When true cache the list of CMake presets associated with this project."
  :type 'boolean
  :group 'projection-types)

(defconst projection-cmake--preset-labels
  '((install . buildPresets)
    (package . packagePresets)
    (test . testPresets)
    (build . buildPresets)
    (configure . configurePresets))
  "Variable associating project build types with CMake preset configurations.")

(defun projection-cmake--read-presets-from-file (preset-file)
  "Read CMake presets from PRESET-FILE.
Read presets from PRESET-FILE and group by the keys in
`projection-cmake--preset-labels'."
  (let ((preset-config (json-read-file preset-file)))
    (cl-loop
     for (preset-type . label) in projection-cmake--preset-labels
     with presets = nil
     do (setq presets
              (mapcar (apply-partially #'alist-get 'name)
                      (alist-get label preset-config)))
     when presets
       collect (cons preset-type presets))))

(defun projection-cmake--list-presets-for-build-type (build-type)
  "Fetch the available CMake presets for BUILD-TYPE.
When BUILD-TYPE is nil fetch the presets for all build types."
  (when-let ((presets (projection-cmake--list-presets)))
    (if build-type
        (alist-get build-type presets)
      (seq-uniq (apply #'append (mapcar #'cdr presets)) #'string-equal))))

(defun projection-cmake--list-presets ()
  "List CMake presets from preset config files respecting cache."
  (when-let ((preset-files
              (seq-filter #'file-exists-p projection-cmake-preset-files)))
    (projection--cache-get-with-predicate
     (projection--current-project 'no-error)
     'projection-cmake-presets
     (and projection-cmake-cache-presets
          (apply #'projection--cache-modtime-predicate preset-files))
     (apply-partially #'projection-cmake--list-presets2 preset-files))))

(defun projection-cmake--list-presets2 (preset-files)
  "List CMake presets from PRESET-FILES config files."
  (let (result)
    (dolist (it (reverse preset-files))
      (when (file-exists-p it)
        (cl-loop for (preset-type . presets) in
                 (projection-cmake--read-presets-from-file it)
                 with configured-value = nil
                 do (setq configured-value (assoc preset-type result))
                 if configured-value
                   do (setcdr configured-value (append presets (cdr configured-value)))
                 else
                   do (push (cons preset-type presets) result))))
    result))



;; CMake preset.

(defcustom projection-cmake-preset 'prompt-always-cache
  "Set which CMake preset to use for the current project.
See https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html."
  :type
  '(choice
    (const nil :tag "Ignore CMake presets.")
    (const prompt-always :tag "Always prompt for which preset to use.")
    (const prompt-multi :tag "Prompt when multiple presets are available.")
    (const prompt-always-cache
           :tag "Always prompt and then reuse the chosen preset.")
    (const prompt-multi-cache
           :tag "Prompt when multiple presets available and then reuse the chosen preset.")
    (string :tag "Use this value as the preset")
    (alist :key-type (symbol :tag "Build type.")
           :value-type (string :tag "CMake preset.")))
  :group 'projection-types)

(defun projection-cmake--preset-cache-var (build-type)
  "Fetch the project cache variable for the BUILD-TYPE CMake preset."
  (if build-type
      (intern (concat "projection-cmake-" (symbol-name build-type) "-preset"))
    'projection-cmake-default-preset))

(defun projection-cmake--read-preset (build-type presets)
  "Interactively select a preset from PRESETS for BUILD-TYPE."
  (completing-read
   (concat "CMake"
           (when build-type
             (concat " " (symbol-name build-type)))
           " preset: ")
   presets))

(defun projection-cmake--preset (&optional build-type)
  "Fetch the CMake preset for the current BUILD-TYPE respecting project cache."
  (let ((project (projection--current-project 'no-error))
        (cache-presets (member projection-cmake-preset
                               '(prompt-always-cache prompt-multi-cache)))
        (cache-var
         (projection-cmake--preset-cache-var build-type)))
    (cl-block nil
      (unless projection-cmake-preset
        (cl-return nil))

      (when (stringp projection-cmake-preset)
        (cl-return projection-cmake-preset))

      (when (consp projection-cmake-preset)
        (cl-return (alist-get (or build-type
                                  'default)
                              projection-cmake-preset)))

      (or
       (when (and project cache-presets)
         (projection--cache-get project cache-var))

       (when-let ((preset (projection-cmake--preset2 build-type)))
         (when (and project cache-presets)
           (projection--cache-put project cache-var preset))

         preset)))))

(defun projection-cmake--preset2 (&optional build-type)
  "Fetch the CMake preset for the current BUILD-TYPE.
If BUILD-TYPE is ommitted we determine the preset for all build-types."
  (when-let
      ((presets (projection-cmake--list-presets-for-build-type build-type)))
    (or
     (when (and (equal (length presets) 1)
                (member projection-cmake-preset
                        '(prompt-multi prompt-multi-cache)))
       (car presets))

     (let ((user-choice
            (projection-cmake--read-preset build-type presets)))
       (unless (string-empty-p user-choice)
         user-choice)))))

(defun projection-cmake-set-preset (build-type preset)
  "Set CMake preset for BUILD-TYPE to PRESET for all projects.
With the prefix argument is set PRESET as the default preset for all build
types."
  (interactive
   (let ((build-type (if current-prefix-arg
                         nil
                       (intern
                        (completing-read
                         "Set CMake preset for build type: "
                         (mapcar #'car projection-cmake--preset-labels)
                         nil 'require-match)))))
     (if-let* ((presets
                (projection-cmake--list-presets-for-build-type build-type))
               (preset (projection-cmake--read-preset
                        build-type (append '("*clear*") presets))))
         (list (or build-type 'default)
               (if (string-equal preset "*clear*") nil preset))
       (user-error "No CMake presets available for build-type: %s"
                   (if build-type
                       build-type
                     "default")))))
  (let ((new-config))
    (cond
     ((stringp projection-cmake-preset)
      (setq new-config `((default . ,projection-cmake-preset))))
     ((consp projection-cmake-preset)
      (setq new-config projection-cmake-preset)))

    (if preset
        (setf (alist-get build-type new-config) preset)
      (setq new-config (assq-delete-all build-type new-config)))
    (setq projection-cmake-preset new-config)))



(defcustom projection-cmake-build-directory "build"
  "Build directory for cmake project builds."
  :type 'string
  :group 'projection-types)

(defcustom projection-cmake-configure-options nil
  "Default CMake options when configured with projection.
Place any -D options or extra flags you always want to use (for example
-DCMAKE_EXPORT_COMPILE_COMMANDS) in this option variable."
  :type '(list (repeat string))
  :group 'projection-types)

(defun projection--cmake-command (&optional build-type target)
  "Generate a CMake command optionally to run TARGET for BUILD-TYPE."
  (projection--join-shell-command
   `("cmake"
     "--build" ,projection-cmake-build-directory
     ,@(when-let ((preset (projection-cmake--preset build-type)))
         (list "--preset" preset))
     ,@(when target (list "--target" target)))))

(projection-register-type 'cmake
  :predicate "CMakeLists.txt"
  ;; The configure step takes the source directory and the output build
  ;; directory.
  :configure (defun projection-cmake-run--configure ()
               (projection--join-shell-command
                `("cmake"
                  "-S" "."
                  "-B" ,projection-cmake-build-directory
                  ,@projection-cmake-configure-options)))
  ;; The remaining commands take the build directory and an optional target
  ;; with it.
  :build   (defun projection-cmake-run--build   () (projection--cmake-command 'build))
  :test    (defun projection-cmake-run--test    () (projection--cmake-command 'test    "ctest"))
  :install (defun projection-cmake-run--install () (projection--cmake-command 'install "install"))
  :package (defun projection-cmake-run--package () (projection--cmake-command 'package "package"))
  :targets #'projection-multi-cmake-targets)

(provide 'projection-types-cmake)
;;; projection-types-cmake.el ends here
