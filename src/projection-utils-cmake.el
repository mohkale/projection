;;; projection-utils-cmake.el --- Helpers for supporting CMake projects. -*- lexical-binding: t; -*-

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

;; Projection project-type helpers for CMake projects.

;;; Code:

(require 'json)

(require 'projection-core)
(require 'projection-core-log)
(require 'projection-utils)

(defgroup projection-type-cmake nil
  "Projection CMake project type."
  :group 'projection-types)



;; CMake reading presets.

(defconst projection-cmake-preset-files
  '("CMakePresets.json" "CMakeUserPresets.json")
  "List of files configuring CMake presets.")

(defcustom projection-cmake-cache-presets 'auto
  "When true cache the list of CMake presets associated with each project."
  :type '(choice
          (const auto :tag "Cache presets and invalidate cache automatically")
          (boolean :tag "Always/Never cache presets"))
  :group 'projection-type-cmake)

(defun projection-cmake--list-presets-for-build-type (build-type)
  "Fetch the available CMake presets for BUILD-TYPE.
When BUILD-TYPE is nil fetch the presets for all build types."
  (when-let ((presets (projection-cmake--list-presets)))
    (if build-type
        (alist-get build-type presets)
      (seq-uniq (apply #'append (mapcar #'cdr presets))
                (lambda (it1 it2)
                  (string-equal (car it1) (car it2)))))))

(defun projection-cmake--list-presets ()
  "List CMake presets from preset config files respecting cache."
  (when-let ((preset-files
              (seq-filter #'file-exists-p projection-cmake-preset-files)))
    (projection--cache-get-with-predicate
     (projection--current-project 'no-error)
     'projection-cmake-presets
     (cond
      ((eq projection-cmake-cache-presets 'auto)
       (apply #'projection--cache-modtime-predicate preset-files))
      (t projection-cmake-cache-presets))
     #'projection-cmake--list-presets2)))

(defun projection-cmake--list-presets2 ()
  "List CMake presets from PRESET-FILES config files."
  (projection--log :debug "Resolving available CMake presets")

  (projection--with-shell-command-buffer "cmake --list-presets=all"
    (let (result
          (build-type 'default)
          (presets nil))
      (save-match-data
        (while (search-forward-regexp
                (rx
                 (or (and bol "Available " (group-n 1 (one-or-more any)) " presets:" )
                     (and bol (+ space) "\"" (group-n 2 (+ (not "\""))) "\""
                          (optional (+ space) "-" (+ space) (group-n 3 (+ any))))))
                nil 'noerror)
          (cond
           ((match-string 1)
            (when presets
              (if build-type
                  (push (cons build-type (nreverse presets)) result)
                (projection--log :warning "Parsed presets=%S for no build type"
                                 presets)))
            (setq build-type (intern (match-string 1))
                  presets nil))
           ((match-string 2)
            (push (cons (match-string 2)
                        (match-string 3))
                  presets)))))
      ;; Add any presets remaining at the end of parsing.
      (when presets
        (if build-type
            (push (cons build-type (nreverse presets)) result)
          (projection--log :warning "Parsed preset=%S for no build type" presets)))
      (nreverse result))))



;; CMake presets.

(defcustom projection-cmake-preset 'prompt-once
  "Set which CMake preset to use for the current project.
See https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html."
  :type
  '(choice
    (choice (string :tag "Default preset")
            (const nil :tag "No default preset."))
    (const disable :tag "Do not supply a preset value to CMake.")
    (const silent :tag "Return configured preset non-interactively.")

    (const prompt-always :tag "Always prompt for which preset to use.")
    (const prompt-once
           :tag "Always prompt and then reuse the chosen preset.")
    (const prompt-once-when-multiple
           :tag "Prompt when multiple presets available and then reuse the chosen preset.")
    (alist :key-type (symbol :tag "Build type.")
           :value-type (string :tag "CMake preset.")))
  :group 'projection-type-cmake)

(defun projection-cmake--preset-cache-var (&optional build-type)
  "Fetch the project cache variable for the BUILD-TYPE CMake preset."
  (or build-type (setq build-type 'default))
  (intern (concat "projection-cmake-" (symbol-name build-type) "-preset")))

(defun projection-cmake--read-preset (prompt presets)
  "Interactively select a preset from PRESETS.
Prompt for the `completing-read' session will be PROMPT."
  (let (affixate result)
    (setq affixate
          (lambda (cands)
            (cl-loop
             for cand in cands
             with description = nil
               do (setq description
                        (alist-get cand presets nil nil #'string-equal))
             when description
               do (setq description
                        (concat (propertize
                                 " " 'display
                                 `(space :align-to (- right 1 ,(length description))))
                                (propertize description 'face 'completions-annotations)))
             collect (list cand "" description))))
    (setq result
          (completing-read
           prompt
           (lambda (str pred action)
             (if (eq action 'metadata)
                 `(metadata
                   (affixation-function . ,affixate))
               (complete-with-action action presets str pred)))))
    (unless (string-empty-p result)
      result)))

(defun projection-cmake--preset (&optional build-type)
  "Fetch the CMake preset for the current BUILD-TYPE respecting project cache."
  ;; TODO: Clean this up... it's a nightmare.
  (cl-block nil
    (when (eq projection-cmake-preset 'disable)
      (cl-return nil))

    (when (consp projection-cmake-preset)
      (when-let ((preset (alist-get (or build-type t)
                                    projection-cmake-preset)))
        (cl-return preset)))

    (let ((project (projection--current-project 'no-error))
          presets                       ; Collection of actual presets for BUILD-TYPE
          )

      ;; Customize option requires interactively selecting one so check if the
      ;; cache variable is set for it.
      (when (and
             project
             (not (eq projection-cmake-preset 'prompt-always)))
        (when-let
            ((cached-value
              (or (when build-type
                    (projection--cache-get project (projection-cmake--preset-cache-var build-type)))
                  (projection--cache-get project (projection-cmake--preset-cache-var)))))

          (cl-return cached-value)))

      (unless (eq projection-cmake-preset 'silent)
        (setq presets (projection-cmake--list-presets-for-build-type build-type))

        (when presets
          (let ((preset))
            (setq
             preset
             (cond
              ((and (member projection-cmake-preset
                            '(prompt-when-multiple prompt-once-when-multiple))
                    (eq (length presets) 1))
               (caar presets))
              ((member projection-cmake-preset
                       '(prompt-always prompt-once prompt-once-when-multiple))
               (projection-cmake--read-preset
                (projection--prompt
                 "Set CMake%s preset: " project
                 (if build-type
                     (concat " " (symbol-name build-type))
                   ""))
                presets))))

            (when preset
              (when (and build-type
                         (member projection-cmake-preset '(prompt-once prompt-once-when-multiple)))
                (projection--cache-put
                 project (projection-cmake--preset-cache-var build-type) preset))
              (cl-return preset)))))

      ;; Return the default preset directly.
      (when (stringp projection-cmake-preset)
        (cl-return projection-cmake-preset)))))

(defconst projection-cmake--preset-build-types
  '(configure build test package)
  "List of build-types that support CMake preset configurations.")

(defun projection-cmake-set-preset (project build-type preset)
  "Set CMake preset for BUILD-TYPE to PRESET for PROJECT."
  (interactive
   (let ((project (projection--current-project))
         build-type preset)
     ;; When `build-type' is not nil we only prompt for presets of that type.
     (setq build-type
           (unless current-prefix-arg
             (intern
              (completing-read
               (projection--prompt "Set CMake preset for build type: " project)
               projection-cmake--preset-build-types
               nil 'require-match))))
     (setq preset
           (projection-cmake--read-preset
            (projection--prompt
             "Set CMake%s preset" project
             (when build-type
               (concat " " (symbol-name build-type))))
            (projection-cmake--list-presets-for-build-type build-type)))
     (list project build-type preset)))
  (projection--cache-put
   project (projection-cmake--preset-cache-var build-type) preset))



;; CMake build type.

(defcustom projection-cmake-build-type nil
  "Build type for a CMake project.
Supplied as the default CMAKE_BUILD_TYPE definition when set."
  :type '(choice (const nil :tag "Do not supply")
                 (string :tag "Build type value"))
  :group 'projection-type-cmake)

(defconst projection-cmake--build-types
  '("Debug" "Release" "RelWithDebInfo" "MinSizeRel")
  "Common build types supported by CMake.")

(defvar projection-cmake--build-type-history nil
  "History variable for `projection-cmake-set-build-type'.")

;;;###autoload
(defun projection-cmake-set-build-type (project build-type)
  "Set the CMake build-type for PROJECT to BUILD-TYPE."
  (interactive
   (let ((project (projection--current-project))
         build-type)
     (setq build-type
           (completing-read
            (projection--prompt "Set CMake build type: " project)
            (seq-uniq
             (append
              (ensure-list (projection-cmake--build-type project))
              projection-cmake--build-types)
             #'string-equal)
            nil nil nil 'projection-cmake--build-type-history))
     (when (string-empty-p build-type)
       (setq build-type nil))
     (list project build-type)))
  (projection--cache-put project 'projection-cmake-build-type build-type))

(defun projection-cmake--build-type (&optional project)
  "Fetch the configured build-type for the PROJECT.
This accesses the value set in the project cache first and falls
back to the value in `projection-cmake-build-type'. When unset
PROJECT defaults to the current project."
  (or
   (when-let ((project (or project
                           (projection--current-project 'no-error))))
     (projection--cache-get project 'projection-cmake-build-type))
   projection-cmake-build-type))



;; CMake command utils.

(defcustom projection-cmake-build-directory "build"
  "Build directory for cmake project builds.
When unset no -B flag will be passed to CMake. You may want this if the build
directory is configured directly in the CMakePresets or elsewhere."
  :type '(optional string)
  :group 'projection-type-cmake)

(defcustom projection-cmake-configure-options nil
  "Default CMake options when configured with projection.
Place any -D options or extra flags you always want to use (for example
-DCMAKE_EXPORT_COMPILE_COMMANDS) in this option variable."
  :type '(list (repeat (string :tag "Argument")))
  :group 'projection-type-cmake)

(defun projection--cmake-command (&optional build-type target)
  "Generate a CMake command optionally to run TARGET for BUILD-TYPE."
  (projection--join-shell-command
   `("cmake"
     ,@(when projection-cmake-build-directory
         (list "--build" projection-cmake-build-directory))
     ,@(when-let ((preset (projection-cmake--preset build-type)))
         (list (concat "--preset=" preset)))
     ,@(when target (list "--target" target)))))

(defun projection--cmake-annotation (build-type target)
  "Generate an annotation for a cmake command to run TARGET for BUILD-TYPE."
  (format "cmake %s%s%s"
          (if projection-cmake-build-directory
              (concat "build:" projection-cmake-build-directory " ")
            "")
          (if-let ((preset (projection-cmake--preset build-type)))
              (concat "preset:" preset " ")
            "")
          target))



;; Reconfigure detection.

(defcustom projection-cmake-cache-file "CMakeCache.txt"
  "Path to configuration cache file relative to the CMake build directory.
This is used to detect if CMake has been configured and whether it has been
reconfigured by since we last may have cached some CMake state (like targets).
This file should change on every build reconfiguration."
  :type 'string
  :group 'projection-multi-cmake)

(defun projection--cmake-configure-modtime-p ()
  "Get when CMake was last configured based on `projection-cmake-cache-file'."
  (projection--cache-modtime-predicate
   (if projection-cmake-build-directory
       (expand-file-name projection-cmake-cache-file
                         projection-cmake-build-directory)
     (unless (file-name-absolute-p projection-cmake-cache-file)
       ;; This will probably always be unmodified since it checks from
       ;; `default-directory' and the file will never exist so we display
       ;; a warning.
       (projection--log
        :warning "Cannot check if CMake has been reconfigured when build \
directory is unknown and `projection-cmake-cache-file' is not absolute."))
     projection-cmake-cache-file)))



;; CMake compilation commands.

;; The configure step takes the source directory and the output build
;; directory.

(defun projection-cmake-run-configure ()
  "Configure command generator for CMake projects."
  (projection--join-shell-command
   `("cmake"
     "-S" "."
     ,@(when projection-cmake-build-directory
        (list "-B" projection-cmake-build-directory))
     ,@(when-let ((preset (projection-cmake--preset 'configure)))
         (list (concat "--preset=" preset)))
     ,@(when-let ((build-type (projection-cmake--build-type)))
         (list (concat "-DCMAKE_BUILD_TYPE=" build-type)))
     ,@projection-cmake-configure-options)))

;; The remaining commands take the build directory and an optional target
;; with it.

(defun projection-cmake-run-build ()
  "Build command generator for CMake projects."
  (projection--cmake-command 'build))

(defun projection-cmake-run-test ()
  "Test command generator for CMake projects."
  (projection--cmake-command 'test "test"))

(defun projection-cmake-run-install ()
  "Install command generator for CMake projects."
  (projection--cmake-command 'install "install"))

(provide 'projection-utils-cmake)
;;; projection-utils-cmake.el ends here
