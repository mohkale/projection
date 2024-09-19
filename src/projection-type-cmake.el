;;; projection-type-cmake.el --- Helpers for supporting CMake projects. -*- lexical-binding: t; -*-

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

(require 'f)
(require 'xdg)
(require 's)
(require 'json)

(require 'projection)
(require 'projection-core)
(require 'projection-core-misc)
(require 'projection-utils)

(defgroup projection-type-cmake nil
  "Projection CMake project type."
  :group 'projection-types)

(defcustom projection-cmake-build-directory "build"
  "Build directory for cmake project builds.
When unset no -B flag will be passed to CMake. You may want this if the build
directory is configured directly in the CMakePresets or elsewhere."
  :type '(optional string)
  :group 'projection-type-cmake)



;; List CMake presets.

(defconst projection-cmake-preset-files
  '("CMakePresets.json" "CMakeUserPresets.json")
  "List of files configuring CMake presets.")

(defcustom projection-cmake-cache-presets 'auto
  "When true cache the list of CMake presets associated with each project."
  :type '(choice
          (const :tag "Cache presets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache presets"))
  :group 'projection-type-cmake)

(defconst projection-cmake--preset-build-types
  '(configure build test package workflow)
  "List of build-types that support CMake preset configurations.")

(defconst projection-cmake--preset-build-types-tied-to-configure
  '(build test package)
  "Build types that can have specific configure presets associated with them.
Projection will avoid presenting presets for these build types that are
incompatible with the active configure preset.")

(defun projection-cmake--available-preset-files ()
  "List existing CMake preset files."
  (seq-filter #'file-exists-p projection-cmake-preset-files))

(defun projection-cmake--list-presets ()
  "List CMake presets from preset config files respecting cache."
  (when-let ((preset-files (projection-cmake--available-preset-files)))
    (projection--cache-get-with-predicate
     (projection--current-project 'no-error)
     'projection-cmake-presets
     (cond
      ((eq projection-cmake-cache-presets 'auto)
       (apply #'projection--cache-modtime-predicate preset-files))
      (t projection-cmake-cache-presets))
     (apply-partially #'projection-cmake--list-presets2 preset-files))))

(projection--declare-cache-var
  'projection-cmake-presets
  :title "CMake presets"
  :category "CMake"
  :description "CMake presets collection"
  :hide t)

(defun projection-cmake--list-presets2 (preset-files)
  "Read CMake preset configurations from PRESET-FILES."
  (projection--log :debug "Resolving available CMake presets from files=%S" preset-files)
  (condition-case err
      (let ((presets-result (mapcar #'list projection-cmake--preset-build-types)))
        (dolist (config (mapcar #'projection-cmake--read-presets-file preset-files))
          (dolist (preset-type projection-cmake--preset-build-types)
            (when-let ((preset-config
                        (thread-last
                          config
                          (alist-get (intern (concat (symbol-name preset-type) "Presets")))
                          (mapcar (lambda (preset)
                                    (append preset `((projection--preset-type . ,preset-type))))))))
              (setcdr (assoc preset-type presets-result)
                      (append (cdr (assoc preset-type presets-result))
                              preset-config)))))
        presets-result)
    ((file-missing json-readtable-error projection-cmake-code-model)
     (projection--log :error "error while reading CMake presets %s." (cdr err)))))

(cl-defsubst projection-cmake--read-presets-file (file)
  "Read JSON file FILE for the CMake file-api."
  (projection--log :debug "Reading CMake file-api file=%s." file)
  (let ((json-array-type 'list))
    (json-read-file file)))



;; Check CMake presets against preset conditions.

(defcustom projection-cmake-respect-preset-conditions t
  "When true filter available presets with CMake list-presets.
CMake presets can specify a condition to restrict a preset to specific platforms
or environment. The only way to respect these conditions is to let CMake itself
tell you which presets are valid or not. `projection' can exclude such presets
when this option is set by spawning an extra sub-process. If unset projection
will let you interactively set a preset that you may not actually be able to
use."
  :type 'boolean
  :group 'projection-type-cmake)

(defcustom projection-cmake-cache-available-presets 'auto
  "When true cache presets for `projection-cmake-respect-preset-conditions'."
  :type '(choice
          (const :tag "Cache presets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache presets"))
  :group 'projection-type-cmake)

(defun projection-cmake--available-presets ()
  "List CMake presets that pass their condition check respecting cache."
  (when projection-cmake-respect-preset-conditions
    (when-let ((preset-files
                (seq-filter #'file-exists-p projection-cmake-preset-files)))
      (projection--cache-get-with-predicate
       (projection--current-project 'no-error)
       'projection-cmake-available-presets
       (cond
        ((eq projection-cmake-cache-available-presets 'auto)
         (apply #'projection--cache-modtime-predicate preset-files))
        (t projection-cmake-cache-available-presets))
       #'projection-cmake--available-presets2))))

(projection--declare-cache-var
  'projection-cmake-available-presets
  :title "Available CMake presets"
  :category "CMake"
  :description "CMake presets passing condition checks"
  :hide t)

(defun projection-cmake--available-presets2 ()
  "List CMake presets that pass their condition check."
  (projection--log :debug "Resolving CMake presets passing condition checks")

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

(defcustom projection-cmake-preset 'prompt-once-when-multiple
  "Set which CMake preset to use for the current project.
See https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html .

This value and other preset oriented options only have significance when the
active `projection-cmake-configuration-backend' is presets.

It is recommended to set this locally in a project with .dir-locals.el if you
want to set the default preset configuration you always want selected. One
possible directory-locals entry can be:

  ((nil . ((projection-cmake-preset . ((configure . \"ninja-multi\")
                                       (build . \"debug\")
                                       (test . \"debug\")
                                       (t . prompt-once-when-multiple))))))

It is recommended to always define a failover preset value to the behaviour you
want when a configure or build preset change invalidates the default test or
build preset. Ommitting this will make projection assume a value of (t . nil)
meaning no preset choice will be set and commands will likely fail."
  :type
  '(choice
    (string :tag "Sole preset value")
    (const :tag "No preset" nil)
    (const :tag "Do not supply a preset value to CMake" disable)
    (const :tag "Return configured preset non-interactively" silent)
    (const :tag "Always prompt for which preset to use" prompt-always)
    (const :tag "Always prompt and then reuse the chosen preset"
           prompt-once)
    (const :tag "Prompt when multiple presets available and then reuse the chosen preset"
           prompt-once-when-multiple)
    (alist :key-type (choice (symbol :tag "Build type")
                             (const :tag "Failover build type" t)
                             (const :tag "Used when the active preset is not compatible with the configure preset"
                                    on-invalid))
           :value-type (sexp :tag "One of the other possible preset values")))
  :group 'projection-type-cmake)

(defun projection-cmake--preset-cache-var (&optional build-type)
  "Fetch the project cache variable for the BUILD-TYPE CMake preset."
  (or build-type (setq build-type 'default))
  (intern (concat "projection-cmake-" (symbol-name build-type) "-preset")))

(defun projection-cmake--list-presets-for-build-type-match-configure-preset
    (presets build-type)
  "Remove PRESETS with a configure-preset not matching the active configure preset.
This only filters out presets when BUILD-TYPE is not configure because configure
presets should always be visible."
  (if-let* ((build-type-is-not-configure (not (eq build-type 'configure)))
            ;; TODO(me): Fix this doesn't handle a value type preset in dir-locals.
            (projection-cmake-preset 'silent)
            (active-configure-preset (projection-cmake--preset 'configure)))
      (cl-remove-if
       (lambda (preset-config)
         (if-let ((build-type-has-required-configure-preset
                   (member (alist-get 'projection--preset-type preset-config)
                           projection-cmake--preset-build-types-tied-to-configure))
                  (required-configure-preset
                   (alist-get 'configurePreset preset-config)))
             (not (equal required-configure-preset active-configure-preset))
           nil))
       presets)
    presets))

(defun projection-cmake--list-presets-for-build-type-match-active-build-type
    (presets build-type)
  "Remove PRESETS with a build type not matching the active build-type.
This only filters out presets when BUILD-TYPE is not configure and not build
because build presets should already be filtered and unique based on the active
configure preset."
  (if-let* ((build-type-is-not-configure (not (eq build-type 'configure)))
            (build-type-is-not-build (not (eq build-type 'build)))
            (projection-cmake-preset 'silent)
            (build-type (projection-cmake--active-build-type)))
      (cl-remove-if
       (lambda (preset-config)
         (if-let ((preset-is-not-build-type
                   (not (eq 'build (alist-get 'projection--preset-type preset-config))))
                  (preset-build-type (alist-get 'configuration preset-config)))
             (not (equal build-type preset-build-type))
           nil))
       presets)
    presets))

(defun projection-cmake--list-presets-for-build-type (build-type)
  "Fetch the available CMake presets for BUILD-TYPE.
When BUILD-TYPE is nil return all presets ignoring those incompatible
with the active configure preset."
  (when-let* ((presets (projection-cmake--list-presets))
              ;; Flatten presets to avoid `build-type' based nesting.
              (presets (if build-type
                           (alist-get build-type presets)
                         (apply #'append (mapcar #'cdr presets))))
              ;; Remove preset options that are explicitly marked hidden.
              (presets (cl-remove-if (lambda (preset-config)
                                       (alist-get 'hidden (cdr preset-config)))
                                     presets))
              ;; Prune out not-applicable non-configure presets.
              (presets
               (thread-first
                 presets
                 (projection-cmake--list-presets-for-build-type-match-configure-preset build-type)
                 (projection-cmake--list-presets-for-build-type-match-active-build-type build-type)))
              ;; Filter out presets that are not available in the current environment.
              (presets (if-let ((available-presets (projection-cmake--available-presets)))
                           (cl-remove-if-not (lambda (preset-config)
                                               (assoc (alist-get 'name preset-config)
                                                      (alist-get
                                                       (alist-get 'projection--preset-type preset-config)
                                                       available-presets)))
                                             presets)
                         presets)))
    presets))

(defun projection-cmake--read-preset (prompt presets)
  "Interactively select a preset from PRESETS.
Prompt for the `completing-read' session will be PROMPT."
  (let* ((single-preset-type
          (equal (length (seq-uniq
                          (mapcar (apply-partially #'alist-get 'projection--preset-type)
                                  presets)))
                 1))
         (presets (cl-loop for preset in presets
                           with preset-type = nil with preset-name = nil
                           ;; Prefix each preset with the preset type and construct an alist.
                           do (setq preset-type (alist-get 'projection--preset-type preset)
                                    preset-name (or (alist-get 'displayName preset)
                                                    (alist-get 'name preset)))
                           collect (cons (if single-preset-type
                                             preset-name
                                           (concat (symbol-name preset-type) ":" preset-name))
                                         preset)))
         (preset-display-name-to-preset
          (lambda (cand)
            (cdr (assoc cand presets))))
         (completion-table
          (projection-completion--completion-table
           :candidates presets
           :group-function
           (lambda (cand transform)
             (let ((index (s-index-of ":" cand)))
               (cond
                (transform (substring cand (1+ index)))
                ((not single-preset-type)
                 (concat (capitalize (substring cand 0 index)) " preset")))))
           :annotation-function
           (projection-completion--annotation-function
            :key-function (lambda (cand)
                            (when-let ((cand-alist (funcall preset-display-name-to-preset cand)))
                              (or (alist-get 'description cand-alist)
                                  ;; Show configured name when no set display-name or
                                  ;; the set display-name is different to the preset
                                  ;; name.
                                  (let ((name (alist-get 'name cand-alist))
                                        (display-name (alist-get 'displayName cand-alist)))
                                    (when (and display-name (not (equal name display-name)))
                                      name))
                                  ))))))
         (result (completing-read prompt completion-table nil 'require-match)))
    (unless (string-empty-p result)
      (or (funcall preset-display-name-to-preset result)
          ;; This should only ever happen in tests which don't properly set expectations.
          (user-error (format "Read preset-name with unsupported preset target=%s from presets=%S"
                              result (mapcar #'car presets)))))))

(defun projection-cmake--preset-config (build-type)
  "Fetch CMake preset configuration for BUILD-TYPE."
  (when-let ((preset-name (projection-cmake--preset build-type)))
    (seq-find (lambda (preset-config)
                (equal (alist-get 'name preset-config) preset-name))
              (projection-cmake--list-presets-for-build-type build-type))))

(defun projection-cmake--preset (build-type)
  "Fetch a CMake preset for BUILD-TYPE respecting config options."
  (let (build-type-preset-option
        on-invalid-preset-option
        (list-presets-cached
         (let ((presets-value nil))
           (lambda ()
             (unless presets-value
               (setq presets-value
                     (cons t (projection-cmake--list-presets-for-build-type build-type))))
             (cdr presets-value)))))
    (if (consp projection-cmake-preset)
        (setq build-type-preset-option (alist-get
                                        build-type projection-cmake-preset
                                        (alist-get t projection-cmake-preset))
              on-invalid-preset-option (alist-get
                                        'on-invalid projection-cmake-preset
                                        (alist-get t projection-cmake-preset)))
      (setq build-type-preset-option projection-cmake-preset
            on-invalid-preset-option projection-cmake-preset))

    (catch 'preset-value
      (catch 'invalid-preset
        (throw 'preset-value
               (projection-cmake--preset2 build-type build-type-preset-option list-presets-cached)))
      (catch 'invalid-preset
        (throw 'preset-value
               (projection-cmake--preset2 build-type on-invalid-preset-option list-presets-cached)))
      (projection--log :debug "Double invalid preset resolution"))))

(defun projection-cmake--preset2 (build-type preset-option list-presets-callback)
  "Fetch a CMake preset for BUILD-TYPE respecting PRESET-OPTION and project cache.
LIST-PRESETS-CALLBACK is a injectable method for accessing the presets for
BUILD-TYPE. It should cache presets after the first list call."
  (cl-block nil
    (let ((project (projection--current-project 'no-error))
          (cache-var
           (projection-cmake--preset-cache-var build-type))
          presets                ; Collection of actual presets for BUILD-TYPE
          preset                 ; The preset value chosen by the user (for caching)
          )

      (when (stringp preset-option)
        (setq preset preset-option))

      ;; If we have a cached preset value it takes priority over the configuration
      ;; options. This is so you can update the options interactively and don't have
      ;; to tweak around with configuration variables.
      ;;
      ;; NOTE This preset is not validated against the active configure preset here
      ;; because the set-preset command will clear the related presets when this
      ;; changes and is no longer applicable.
      (when-let* ((project project)
                  (cached-preset (projection--cache-get project cache-var)))
        (setq preset cached-preset))

      ;; Always prefer the configured entries over prompting the user.
      (when (eq preset-option 'disable)
        (cl-return nil))

      (cond
       ((and (not (member build-type projection-cmake--preset-build-types-tied-to-configure))
             preset)
        ;; Preset is set and not affected by the configure preset so return immediately.
        (cl-return preset))
       ((not (setq presets (funcall list-presets-callback)))
        ;; No presets for `build-type' that are compatible with our configure preset.
        nil)
       (preset
        ;; Preset for `build-type' are set and is compatible with the configure preset.
        (if (seq-find (lambda (preset-config)
                        (equal (alist-get 'name preset-config) preset))
                      presets)
            (cl-return preset)
          (projection-cmake-set-preset project build-type nil)
          (throw 'invalid-preset nil)))
       ((eq preset-option 'silent)
        nil)
       ((setq preset
              (cond
               ((and (member preset-option
                             '(prompt-when-multiple prompt-once-when-multiple))
                     (eq (length presets) 1))
                (alist-get 'name (car presets)))

               ((member preset-option
                        '(prompt-always prompt-once prompt-once-when-multiple))
                (alist-get 'name
                           (projection-cmake--read-preset
                            (projection--prompt
                             "Set CMake%s preset: " project
                             (if build-type
                                 (concat " " (symbol-name build-type))
                               ""))
                            presets)))))
        ;; Cache the preset if configured to only prompt once.
        (when (member preset-option '(prompt-once prompt-once-when-multiple))
          (projection-cmake-set-preset project build-type preset))
        (cl-return preset))))))

;;;###autoload
(defun projection-cmake-set-preset (project build-type preset)
  "Set CMake preset for BUILD-TYPE to PRESET for PROJECT."
  (interactive
   (let* ((project (projection--current-project))
          (default-directory (project-root project)))
     (if-let ((presets (projection-cmake--list-presets-for-build-type nil)))
         (let* ((preset-config
                 (projection-cmake--read-preset
                  (projection--prompt "Set CMake preset: " project)
                  presets))
                (build-type (alist-get 'projection--preset-type preset-config))
                (preset (alist-get 'name preset-config)))
           (list project build-type preset))
       (user-error "No CMake presets found for the current project"))))
  (projection--cache-put project (projection-cmake--preset-cache-var build-type) preset))

(dolist (build-type projection-cmake--preset-build-types)
  (projection--declare-cache-var
    (projection-cmake--preset-cache-var build-type)
    :title (format "CMake %s preset" (symbol-name build-type))
    :category "CMake"
    :description (concat "The CMake preset for the " (symbol-name build-type))
    :hide t))



;; CMake Kits from [[https://github.com/microsoft/vscode-cmake-tools/blob/9c7e643/docs/kits.md][microsoft/vscode-cmake-tools]].

(defcustom projection-cmake-kit 'prompt-once
  "Set with CMake kit to use for the current project.
See https://github.com/microsoft/vscode-cmake-tools/blob/9c7e643/docs/kits.md .

This value and other kit oriented options only have significance when the
active `projection-cmake-configuration-backend' is kits.

See `projection-cmake-preset' for a description of the supported values defined
here."
  :type
  '(choice
    (string :tag "Name of kit")
    (const :tag "No kit" nil)
    (const :tag "Do not supply a preset value to CMake" disable)
    (const :tag "Return configured preset non-interactively" silent)
    (const :tag "Always prompt for which preset to use" prompt-always)
    (const :tag "Always prompt and then reuse the chosen preset"
           prompt-once)
    (const :tag "Prompt when multiple presets available and then reuse the chosen preset"
           prompt-once-when-multiple))
  :group 'projection-type-cmake)

(defcustom projection-cmake-kits-files
  `(".vscode/cmake-kits.json"
    ,(expand-file-name "CMakeTools/cmake-tools-kits.json" (xdg-data-home)))
  "List of locations where a CMakePresets file can exist.
These should either be absolute or will be expanded relative to the current
project root."
  :type '(list (repeat (string :tag "Kit file path")))
  :group 'projection-type-cmake)

(defcustom projection-cmake-cache-available-kits 'auto
  "When true cache the kits available for of each project."
  :type '(choice
          (const :tag "Cache targets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache targets"))
  :group 'projection-type-cmake)

(defun projection-cmake--list-kits ()
  "Query available kits respecting project cache."
  (when-let ((kits-files (seq-filter #'file-exists-p projection-cmake-kits-files)))
    (projection--cache-get-with-predicate
     (projection--current-project 'no-error)
     'projection-cmake-available-kits
     (cond
      ((eq projection-cmake-cache-available-kits 'auto)
       (apply #'projection--cache-modtime-predicate kits-files))
      (t projection-cmake-cache-available-kits))
     (apply-partially #'projection-cmake--list-kits2 kits-files))))

(projection--declare-cache-var
  'projection-cmake-available-kits
  :title "Available CMake kits"
  :category "CMake"
  :description "Available kits from `projection-cmake-kits-files'."
  :hide t)

(defun projection-cmake--list-kits2 (kits-files)
  "Read kits from all the files in KITS-FILES and concat them together."
  (cl-loop for kit-file in kits-files append
           (projection-cmake--read-kit-file kit-file)))

(defun projection-cmake--cmakefiy-kit-setting (key value &optional type)
  "Return a CMake configure flag for setting KEY to VALUE.
TYPE is an optional override for the type of VALUE to pass to CMake."
  (cl-destructuring-bind (cli-type . cli-value)
      (pcase value
        ((pred booleanp) (cons "BOOL" (if value "TRUE" "FALSE")))
        ((pred numberp) (cons "STRING" (number-to-string value)))
        ((and (pred consp) (guard (consp (car value))))
         (let-alist value (cons .type .value)))
        ((pred consp) (cons "STRING" (string-join value ";")))
        ((guard type) (cons type value))
        ((pred stringp) (cons "STRING" value))
        (_
         (projection--log
          :error "Invalid value=%s to convert to cmake value for key=%s" value key)
         (cons nil value)))
    (concat "-D" key (when cli-type (concat ":" cli-type)) "=" cli-value)))

(defun projection-cmake--read-kit-file (kit-file)
  "Read CMake kits from KIT-FILE.
Returns a alist with the name being kit name and the value being
an alist containing optional properties for description, environment
variables and configure options."
  (condition-case err
      (cl-loop
       for kit in (let ((json-array-type 'list)
                        (json-false nil))
                    (json-read-file kit-file))
       unless (alist-get 'name kit)
         do (signal 'projection-cmake-kit
                    (list "Encountered kit with no name" kit-file))
       unless (alist-get 'environmentSetupScript kit)
         do (projection--log :warning "The environmentSetupScript option of \
cmake-kits from kits-file=%s is unsupported by projection." kit-file)
       collect
       (let-alist kit
         `((name . ,.name)
           ,@(when .description
               `((description . ,.description)))
           ,@(when .environmentVariables
               `((environment . ,(cl-loop for (key . value) in .environmentVariables
                                          collect (cons (symbol-name key) value)))))
           (args
            ,@(when .preferredGenerator
                (if-let ((name (alist-get 'name .preferredGenerator)))
                    (list "-G" name)
                  (projection--log :warning "Kit with name=%s does \
not set generator as name property" .name)))
            ,@(when .toolchainFile
                (list "--toolchain" .toolchainFile))
            ,@(when .compilers
                (cl-loop
                 for (lang . compiler) in .compilers
                 collect (projection-cmake--cmakefiy-kit-setting
                          (concat "CMAKE_" (symbol-name lang) "_COMPILER")
                          compiler "FILEPATH")))
            ,@(cl-loop
               for (key . value) in .cmakeSettings
               collect (projection-cmake--cmakefiy-kit-setting (symbol-name key) value))))))
    ((file-missing json-readtable-error projection-cmake-kit)
     (projection--log :error "error while reading CMake kits-file=%s %S"
                      kit-file (cdr err)))))

(projection--declare-cache-var
  'projection-cmake-kit
  :title "CMake kit"
  :category "CMake"
  :description "Active CMake kit."
  :hide t)

(defun projection-cmake--kit ()
  "Return the CMake kit configuration for the current project.
This option respects the project cache and local Emacs settings."
  (cl-block nil
    (let ((project (projection--current-project 'no-error))
          kits kit)
      (when (eq projection-cmake-kit 'disable)
        (cl-return nil))

      (when-let* ((project project)
                  (cached-kit (projection--cache-get project 'projection-cmake-kit)))
        (cl-return cached-kit))

      (setq kits (projection-cmake--list-kits))
      (unless kits (cl-return nil))

      (when (stringp projection-cmake-kit)
        (if-let ((kit (seq-find (lambda (kit)
                                  (equal (alist-get 'name kit) projection-cmake-kit))
                                kits)))
            (cl-return kit)
          (user-error "Kit with name=%s not found in %S"
                      projection-cmake-kit projection-cmake-kits-files)))

      (cond
       ((and (eq projection-cmake-kit 'prompt-once-when-multiple)
             (eq (length kits) 1))
        (setq kit (car kits)))
       ((eq projection-cmake-kit 'silent) (cl-return nil))
       (t
        (setq kit (projection-cmake--read-kit
                   (projection--prompt "Set CMake kit: " project)
                   kits))))
      (when kit
        (when (member projection-cmake-kit '(prompt-once prompt-once-when-multiple))
          (projection--cache-put project 'projection-cmake-kit kit))
        (cl-return kit)))))

(defun projection-cmake--read-kit (prompt kits)
  "Interactively select a kit from KITS.
Prompt for the `completing-read' session will be PROMPT."
  (let* ((name-to-kit
          (cl-loop for kit in kits
                   with name = nil
                   do (setq name (alist-get 'name kit))
                   collect (cons name kit)))
         (completion-table
          (projection-completion--completion-table
           :candidates name-to-kit
           :annotation-function
           (projection-completion--annotation-function
            :key-function
            (lambda (cand)
              (let-alist (cdr (assoc cand name-to-kit))
                .description)))))
         (result (completing-read prompt completion-table nil 'require-match)))
    (unless (string-empty-p result)
      (or (cdr (assoc result name-to-kit))
          ;; This should only ever happen in tests which don't properly set expectations.
          (user-error (format "Read kit-name with unsupported kit target=%s from kits=%S"
                              result (mapcar #'car kits)))))))

;;;###autoload
(defun projection-cmake-set-kit (project kit)
  "Set CMake kit to KIT for PROJECT."
  (interactive
   (let* ((project (projection--current-project))
          (default-directory (project-root project)))
     (if-let ((kits (projection-cmake--list-kits)))
         (list project (projection-cmake--read-kit
                        (projection--prompt "Set CMake kit: " project)
                        kits))
       (user-error "No CMake kits found for the current project"))))
  (projection--cache-put project 'projection-cmake-kit kit))



;; Pluggable backends for setting configuration and build defaults.

(defcustom projection-cmake-configuration-backend '(presets kits)
  "Backends for default CMake configuration options."
  :type '(repeat (choice (const :tag "Source from CMakePresets" presets)
                         (const :tag "Source from CMake kits" kits)))
  :group 'projection-type-cmake)

(defun projection-cmake--command-options (build-type)
  "Query CMake and CTest command options for BUILD-TYPE.
Will walk through possible configuration backends in
`projection-cmake-configuration-backend' until one gives a result."
  (cl-loop
   for backend in projection-cmake-configuration-backend
   do (setq backend (projection-cmake--command-options2 build-type backend))
   when backend
     return backend))

(cl-defgeneric projection-cmake--command-options2 (_build-type _backend)
  "Generic support for querying CMake options from _BACKEND for _BUILD-TYPE."
  nil)

(cl-defmethod projection-cmake--command-options2 (build-type (backend (eql presets)))
  "Query CMake options for BUILD-TYPE from CMakePresets.
BACKEND is \\='presets."
  (when-let ((preset (projection-cmake--preset-config build-type)))
    (let-alist preset
      `((backend . ,backend)
        (name . ,.name)
        (args . (,(concat "--preset=" .name)))
        (configuration . ,.configuration)))))

(cl-defmethod projection-cmake--command-options2 ((_build-type (eql configure))
                                                  (backend (eql kits)))
  "Query CMake configure options from CMakeKits.
BACKEND is \\='kits."
  (when-let ((kit (projection-cmake--kit)))
    `((backend . ,backend)
      ,@kit)))

(cl-defmethod projection-cmake--command-options2 (_build-type (backend (eql kits)))
  "Query CMake configure options for _BUILD-TYPE from CMakeKits.
BACKEND is \\='kits."
  ;; Kits don't let you set build or test options so we just check
  ;; we're configuring through kits so we can send a blank payload.
  (when (projection-cmake--command-options2 'configure backend)
    `((backend . ,backend))))



;; CMake file API [[man:cmake-file-api(7)]].

(defconst projection-cmake--file-api-client "client-emacs-projection")

(defcustom projection-cmake-cache-code-model 'auto
  "When true cache the CMake code-model of each project."
  :type '(choice
          (const :tag "Cache targets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache targets"))
  :group 'projection-type-cmake)

(cl-defsubst projection-cmake--file-api-query-file-suffix ()
  "Subpath to the CMake query file for the projection client."
  (f-join ".cmake" "api" "v1" "query" projection-cmake--file-api-client "query.json"))

(cl-defsubst projection-cmake--file-api-reply-directory-suffix ()
  "Subpath to the CMake reply directory for the projection client."
  (f-join ".cmake" "api" "v1" "reply"))

(defun projection-cmake--file-api-code-model ()
  "Get the generated code-model object for the projection client.
This function respects `projection-cmake-cache-code-model'."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-cmake-code-model
   (cond
    ((eq projection-cmake-cache-code-model 'auto)
     #'projection-cmake--configure-modtime-p)
    (t projection-cmake-cache-code-model))
   #'projection-cmake--file-api-code-model2))

(projection--declare-cache-var
  'projection-cmake-code-model
  :title "CMake code model"
  :category "CMake"
  :description "CMake file-API code-model from last configure time"
  :hide t)

(defun projection-cmake--file-api-code-model2 ()
  "Get the generated code-model object for the projection client."
  (projection--log :debug "Reading CMake code-model")

  (condition-case err
      (let* ((api-replies (projection-cmake--file-api-bulk-read-reply-directory
                           (projection-cmake--build-directory 'expand)))
             (indexes (seq-filter (apply-partially #'string-match-p
                                                   (rx bol "index-" (+ any) ".json" eol))
                                  (mapcar #'car api-replies)))
             (index-file (car (cl-sort indexes #'string>))))
        (unless indexes
          (signal 'projection-cmake-code-model "Cannot query cmake codemodel because no indexes exist"))
        (when-let* ((codemodel-file
                     (projection-cmake--file-api-query-code-model-file
                      (alist-get index-file api-replies nil nil #'string-equal)))
                    (codemodel
                     (alist-get codemodel-file api-replies nil nil #'string-equal)))
          (let ((targets-by-config
                 (thread-last
                   codemodel
                   (alist-get 'configurations)
                   (mapcar (apply-partially
                            #'projection-cmake--file-api-query-config-targets api-replies)))))
            `((codemodel . ,codemodel)
              (targets-by-config . ,targets-by-config)))))
    ((file-missing json-error projection-cmake-code-model)
     (projection--log :error "error while querying CMake code-model %s." (cdr err)))))

(defun projection-cmake--file-api-bulk-read-reply-directory (build-dir)
  "Read all files in the CMake file API reply directory under BUILD-DIR at once.
This a performance optimisation for remote files to be able to parse the entire
CMake file API reply at once. The alternative is to do multiple separate read
lookups which can slow down the processing of the code-model immensely."
  (let ((default-directory build-dir)
        (reply-path (projection-cmake--file-api-reply-directory-suffix)))
    (projection--with-shell-command-buffer
      (concat
       "set -e; "
       "first=1; echo '['; "
       "for file in " (shell-quote-argument reply-path) "/*.json; do "
       "  if [ \"$first\" -eq 1 ]; then first=0; else echo \",\"; fi; "
       "  echo '{\"projection--file\": \"'\"$file\"'\", \"projection--file-content\": '; "
       "  cat \"$file\"; "
       "  echo '}'; "
       "done; "
       "echo ']'")

      (cl-loop
       for it in (let ((json-array-type 'list))
                   (json-read))
       collect (cons (f-filename (alist-get 'projection--file it))
                     (alist-get 'projection--file-content it))))))

(cl-defsubst projection-cmake--file-api-query-code-model-file (index-obj)
  "Read the base-name of the code-model file through INDEX-OBJ."
  (thread-last
    index-obj
    (alist-get 'reply)
    (alist-get (intern projection-cmake--file-api-client))
    (alist-get 'query.json)
    (alist-get 'responses)
    (car-safe)
    (alist-get 'jsonFile)))

(cl-defsubst projection-cmake--file-api-query-config-targets (api-replies config-obj)
  "Read all target configs from the CMake CONFIG-OBJ.
API-REPLIES is a collection of files parsed from the CMake reply directory."
  (if-let ((name (alist-get 'name config-obj)))
      (let ((target-configs (cl-loop for target in (alist-get 'targets config-obj)
                                      with target-config-file = nil
                                      do (setq target-config-file (alist-get 'jsonFile target))
                                      with target-config = nil
                                      do (setq target-config
                                               (alist-get target-config-file api-replies
                                                          nil nil #'string-equal))
                                      when target-config
                                        collect target-config)))
        (cons name target-configs))
    (signal 'projection-cmake-code-model "Encountered configuration object with no name")))

;;;###autoload
(defun projection-cmake--file-api-create-query-file ()
  "Create a file-api client query file for projection."
  (let ((query-file (f-join
                     (projection-cmake--build-directory 'expand)
                     (projection-cmake--file-api-query-file-suffix))))
    (unless (file-exists-p query-file)
      (with-temp-buffer
        (insert
         (json-serialize '((requests . [((kind . "codemodel") (version . 2))])
                           (client))
                         :false-object :json-false))
        (make-directory (f-dirname query-file) 'parents)
        (write-region nil nil query-file nil 'silent)))))

(defun projection-cmake--file-api-target-config ()
  "Get target metadata for the active CMake build config.
If none is active or we could not deduce the active config we default to the
first configured."
  (when-let* ((code-model (projection-cmake--file-api-code-model))
              (build-type (or (projection-cmake--active-build-type) ""))
              (target-configurations (alist-get 'targets-by-config code-model)))
    (or (cdr (assoc build-type target-configurations))
        (cdar target-configurations))))

(defun projection-cmake--cmake-project-p (project-types)
  "Helper to check whether one of the types in PROJECT-TYPES is CMake.
Advise this if you need more than just the CMake project type to have a
query file created before configuring."
  (member 'cmake (mapcar #'projection-type--name project-types)))

;;;###autoload
(cl-defun projection-cmake--file-api-create-query-hook (&key project &allow-other-keys)
  "Helper to create a CMake query file before configuring the CMake PROJECT."
  (if projection-cmake-build-directory
      (when (and (projection-cmake--cmake-project-p
                  (projection-project-types (project-root project))))
        (projection-cmake--file-api-create-query-file))
    (projection--log :warning "Skipping CMake file API setup because build directory \
is unset. This will disable some features like target selection.")))

;;;###autoload
(cl-defun projection-cmake--file-api-clear-cache-on-configure (&key project &allow-other-keys)
  "Clear CMake cache on reconfiguring PROJECT."
  (dolist (cache-var '(projection-cmake-code-model
                       projection-cmake-ctest-targets))
    (projection-cache-clear-single project cache-var projection--project-cache)))

;;;###autoload
(progn
  (add-hook 'projection-commands-pre-configure-hook #'projection-cmake--file-api-create-query-hook)
  (add-hook 'projection-commands-post-configure-hook #'projection-cmake--file-api-clear-cache-on-configure))



;;;###autoload (autoload 'projection-cmake-set-build-type "projection-type-cmake" nil 'interactive)
(projection--declare-project-type-option 'build-type
  :project 'projection-cmake
  :options '("Debug" "Release" "RelWithDebInfo" "MinSizeRel")
  :category "CMake"
  :title "CMake build type"
  :custom-type '(choice (const :tag "Do not supply" nil)
                        (string :tag "Build type value"))
  :custom-group 'projection-type-cmake
  :custom-docstring "Build type for a CMake project.
Supplied as the default CMAKE_BUILD_TYPE definition when set.")

(defun projection-cmake--active-build-type ()
  "Fetch current CMake build type from config options or preset."
  (or (projection-cmake--build-type)
      (let* ((projection-cmake-preset 'silent)
             (options (projection-cmake--command-options 'build)))
        (alist-get 'configuration options))))

;;;###autoload (autoload 'projection-cmake-set-configure-log-level "projection-type-cmake" nil 'interactive)
(projection--declare-project-type-option 'configure-log-level
  :project 'projection-cmake
  :options '("ERROR" "WARNING" "NOTICE" "STATUS" "VERBOSE" "DEBUG" "TRACE")
  :category "CMake"
  :title "CMake configure log-level"
  :custom-type '(choice (const :tag "Default CMake log-level" nil)
                        (string :tag "Log level value"))
  :custom-group 'projection-type-cmake
  :custom-docstring "Set log-level of CMake configure.")

;;;###autoload (autoload 'projection-cmake-set-build-verbosely "projection-type-cmake" nil 'interactive)
(projection--declare-project-type-option 'build-verbosely
  :project 'projection-cmake
  :category "CMake"
  :title "CMake build verbosely"
  :custom-type 'boolean
  :custom-group 'projection-type-cmake
  :custom-docstring "Run CMake build commands with the --verbose flag.
This will cause CMake to print out the compilation commands before running them.")



;; CMake CTest targets

(defcustom projection-cmake-ctest-cache-targets 'auto
  "When true cache the CMake CTest targets of each project."
  :type '(choice
          (const :tag "Cache targets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache targets"))
  :group 'projection-type-cmake)

(defun projection-cmake-ctest--targets ()
  "Resolve available ctest targets for a project respecting the project cache."
  (let* ((project (projection--current-project 'no-error))
         (default-directory (or (when project (project-root project))
                                default-directory))
         (command-options (projection-cmake--command-options 'test))
         (command-option-key (alist-get 'name command-options)))
    (projection--cache-get-with-predicate
     project
     'projection-cmake-ctest-targets
     (cond
      ((eq projection-cmake-ctest-cache-targets 'auto)
       (apply-partially #'projection-cmake--ctest-modtime-p command-option-key))
      (t projection-cmake-ctest-cache-targets))
     (apply-partially #'projection-cmake-ctest--targets2 command-options))))

(defun projection-cmake--ctest-modtime-p (command-option-key plist)
  "Calculate a modification time for reading CTest targets.
COMMAND-OPTION-KEY is an identifier for the configuration-backend. If the
identifier has changed from a previous call to this function then the cache
will be immediately invalidated. PLIST is the property list used by
`projection-cache'."
  (cl-destructuring-bind (&key value &allow-other-keys) plist
    (if (equal (alist-get 'projection--command-options-key value)
               command-option-key)
        (seq-max
         `(,(projection-cmake--configure-modtime-p plist)
           ,@(when-let ((preset-files (projection-cmake--available-preset-files)))
               (list (apply #'projection--cache-modtime-predicate preset-files)))))
      (projection--cache-now))))

(projection--declare-cache-var
  'projection-cmake-ctest-targets
  :title "CMake CTest targets"
  :category "CMake"
  :description "CTest tests tied to this project"
  :hide t)

(defun projection-cmake-ctest--targets2 (command-option-key)
  "Resolve available CTest targets for a project.
COMMAND-OPTION-KEY is an identifier for the configuration-backend. This will
be either a CMake preset or a kit name.."
  (projection--log :debug "Resolving available CMake CTest targets")
  (when-let ((ctest-targets
              (projection--with-shell-command-buffer
                (projection-cmake--ctest-command 'argv '("--show-only=json-v1"))
                (let ((json-array-type 'list))
                  (json-read)))))
    (append ctest-targets `((projection--command-options-key . ,command-option-key)))))



;; Set build and test targets through projection-multi-embark.

(projection--declare-cache-var
  'projection-cmake-build-target
  :title "CMake build target"
  :category "CMake"
  :description "Default CMake target for compiling"
  :hide t)

(cl-defun projection-cmake--set-build-target (&key type command project
                                                   cmake-target &allow-other-keys)
  "Set CMAKE-TARGET as the primary build target.
See `projection-multi-embark' TYPE, COMMAND, and PROJECT."
  (unless (eq type 'build)
    (user-error "Cannot set CMake target for build-type=%s" type))
  (unless cmake-target
    (user-error "Do not know how to set CMake target from command=%S" command))
  (projection--cache-put
   project 'projection-cmake-build-target cmake-target))

(projection--declare-cache-var
  'projection-cmake-ctest-target
  :title "CMake ctest target"
  :category "CMake"
  :description "Default CMake target for testing"
  :hide t)

(cl-defun projection-cmake--set-ctest-target  (&key type command project
                                                    ctest-target &allow-other-keys)
  "Set CTEST-TARGET as the primary CTest target.
See `projection-multi-embark' TYPE, COMMAND, and PROJECT."
  (unless (eq type 'test)
    (user-error "Cannot set CTest target for build-type=%s" type))
  (unless ctest-target
    (user-error "Do not know how to set CTest target from command=%S" command))
  (projection--cache-put
   project 'projection-cmake-ctest-target ctest-target))



;; CMake command utils.

(defcustom projection-cmake-environment-variables
  '(("CLICOLOR_FORCE" . "1"))
  "Default CMake environment variables.
When set any cmake command will be invoked through the env command with each
key value pair set."
  :type '(alist :key-type (string :tag "Environment variable")
                :value-type (string :tag "Value of variable"))
  :group 'projection-type-cmake)

(defcustom projection-cmake-build-directory-remote t
  "Assert whether build directory is on the same host as the project.
This option only has significance when `projection-cmake-build-directory' is
absolute. This may be the case when using docker tramp and setting the CMake
build directory to some shared volume. In this case the remote part may not
be deterministic of time so you may omit it from the above option but then
projection cannot find the build area for target caching or other checks.

When set to true projection will prefix the absolute build directory path
with the remote part of the project. This is done automatically when the
build path is relative."
  :type '(choice
          (const :tag "Reuse the remote component of the project" t)
          (string :tag "Specify the remote component directly")
          (const :tag "Do not do remote matching, the build area will always be local" nil))
  :group 'projection-type-cmake)

(defcustom projection-cmake-configure-options nil
  "Default CMake options when configured with projection.
Place any -D options or extra flags you always want to use (for example
-DCMAKE_EXPORT_COMPILE_COMMANDS) in this option variable."
  :type '(list (repeat (string :tag "Argument")))
  :group 'projection-type-cmake)

(defun projection-cmake--build-directory (&optional expand)
  "Get the CMake build directory.
When EXPAND is true this function will resolve the complete path to the
build directory in a form that can be queried directly from elisp
including any remote components of the project when
`projection-cmake-build-directory-remote' is configured correctly."
  (when projection-cmake-build-directory
    (if expand
        (projection--expand-file-name-in-project
         projection-cmake-build-directory
         projection-cmake-build-directory-remote)
      projection-cmake-build-directory)))

(defun projection-cmake--command (&optional build-type target)
  "Generate a CMake command optionally to run TARGET for BUILD-TYPE."
  (thread-first
    (let-alist (projection-cmake--command-options build-type)
      (projection--join-shell-command
       `(,@(projection--env-shell-command-prefix
            (append .environment projection-cmake-environment-variables))
         "cmake"
         ,@(when-let ((build projection-cmake-build-directory))
             (list "--build" build))
         ,@(when (eq build-type 'build)
             (when-let ((job-count (projection--guess-parallelism
                                    projection-build-jobs)))
               (list (concat "--parallel=" (number-to-string job-count)))))
         ,@(when-let ((verbose (projection-cmake--build-verbosely)))
             (list "--verbose"))
         ,@.args
         ,@(when target (list "--target" target)))))

    (projection--attach-set-build-target-properties
     (when (and (eq build-type 'build) target)
       #'projection-cmake--set-build-target)
     'cmake-target target)))

(defun projection-cmake--annotation (build-type target)
  "Generate an annotation for a cmake command to run TARGET for BUILD-TYPE."
  (format "cmake %s%s"
          (if-let ((opts (projection-cmake--command-options build-type)))
              (let-alist opts
                (concat (symbol-name .backend) ":" .name " "))
            "")
          target))

(defun projection-cmake--workflow-command (preset)
  "Generate a CMake command to run the workflow PRESET."
  (projection--join-shell-command
   `("cmake"
     "--workflow"
     ,(concat "--preset=" preset))))

(defun projection-cmake--workflow-annotation (preset)
  "Generate an annotation for a cmake command to run a workflow PRESET."
  (format "cmake %s%s%s"
          (concat "preset:" preset " ")
          (if-let ((build (projection-cmake--build-directory)))
              (concat "build:" build " ")
            "")
          "workflow"))



;; CTest command utils.

(defcustom projection-cmake-ctest-options '("--output-on-failure"
                                            "--schedule-random")
  "Default CTest invocation options.
Set the extra command line options to pass to ctest."
  :type '(list string)
  :group 'projection-type-cmake)

(defcustom projection-cmake-ctest-environment-variables
  '(("CLICOLOR_FORCE" . "1")
    ("GTEST_COLOR" . "1"))
  "Default CTest environment variables options.
When set any ctest commands will be invoked through the env command with each
key value pair set."
  :type '(alist :key-type (string :tag "Environment variable")
                :value-type (string :tag "Value of variable"))
  :group 'projection-type-cmake)

(defun projection-cmake--ctest-command2 (&rest argv)
  "Helper function to  generate a CTest command.
ARGV if provided will be appended to the command."
  (let-alist (projection-cmake--command-options 'test)
    (projection--join-shell-command
     `(,@(projection--env-shell-command-prefix
          (append .environment
                  projection-cmake-ctest-environment-variables))
       "ctest"
       ,@(when-let ((build (projection-cmake--build-directory)))
           (list "--test-dir" build))
       ,@(when-let ((job-count (projection--guess-parallelism
                                projection-test-jobs)))
           (list "--parallel" (number-to-string job-count)))
       ,@.args
       ,@projection-cmake-ctest-options
       ,@argv))))

(defun projection-cmake--ctest-command (&optional type args)
  "Generate CTest command of type TYPE with ARGS.
TYPE should be one of \\='target, \\='label, \\'not-label, \\'argv. If
TYPE is unset a CTest command to run all tests wil be returned."
  (thread-first
    (pcase type
      ('target    (projection-cmake--ctest-command2 "-R" (concat "^" args "$")))
      ('label     (projection-cmake--ctest-command2 "-L" (concat "^" args "$")))
      ('not-label (projection-cmake--ctest-command2 "-LE" (concat "^" args "$")))
      ('argv      (apply #'projection-cmake--ctest-command2 args))
      ((guard (not type)) (projection-cmake--ctest-command2))
      (_ (error "Unsupported CTest target type %s" type)))

    (projection--attach-set-build-target-properties
     (when (and type args)
       #'projection-cmake--set-ctest-target)
     'ctest-target (list type args))))

(defun projection-cmake--ctest-annotation (&optional type args)
  "Generate an annotation for a ctest command to run TYPE with ARGS."
  (format "ctest %s%s"
          (if-let ((opts (projection-cmake--command-options 'test)))
              (let-alist opts
                (concat (symbol-name .backend) ":" .name " "))
            "")
          (pcase type
            ('target    args)
            ('label     (concat "label:" args))
            ('not-label (concat "except-label:" args))
            ((guard (not type)) "")
            ((or 'argv _) (format "%S" (list type args))))))



;; Reconfigure detection.

(defcustom projection-cmake-cache-file "CMakeCache.txt"
  "Path to configuration cache file relative to the CMake build directory.
This is used to detect if CMake has been configured and whether it has been
reconfigured since we last may have cached some CMake state (like targets).
This file should change on every build reconfiguration."
  :type 'string
  :group 'projection-type-cmake)

(defun projection-cmake--configure-modtime-p (&rest _)
  "Get when CMake was last configured based on `projection-cmake-cache-file'."
  (projection--cache-modtime-predicate
   (if-let ((build-directory (projection-cmake--build-directory 'expand)))
       (expand-file-name projection-cmake-cache-file build-directory)
     (unless (file-name-absolute-p projection-cmake-cache-file)
       ;; This will probably always be unmodified since it checks from
       ;; `default-directory' and the file will never exist so we display
       ;; a warning.
       (projection--log
        :warning "Cannot check if CMake has been reconfigured when build \
directory is unknown and `projection-cmake-cache-file' is not absolute."))
     projection-cmake-cache-file)))



;;; Artifacts

(defconst projection-cmake--artifact-types
  '(("EXECUTABLE"
     (type . cmake-executable)
     (debuggable . t))
    ("STATIC_LIBRARY" (type . cmake-library))
    ("SHARED_LIBRARY" (type . cmake-library))))

(defun projection-cmake-list-artifacts ()
  "List CMake target artifacts."
  (let* ((build-directory
          (projection-cmake--build-directory 'expand))
         (build-directory-remote (file-remote-p build-directory))
         (build-directory (substring build-directory
                                     (when build-directory-remote
                                       (length build-directory-remote))))
         (targets (projection-cmake--file-api-target-config)))
    (let ((result nil))
      (dolist (target targets)
        (let-alist target
          (when-let ((props
                      (alist-get .type projection-cmake--artifact-types nil nil #'string-equal)))
            (dolist (artifact .artifacts)
              (setq artifact (alist-get 'path artifact))
              (push
               `((name . ,artifact)
                 ,@props
                 (category . ,(concat "CMake "
                                      (string-remove-prefix
                                       "cmake-" (symbol-name (alist-get 'type props)))))
                 (arg0 . ,(f-join build-directory artifact)))
               result)))))
      (nreverse result))))

(defun projection-ctest--read-property (props name)
  "Fetch the property from the CMake target PROPS with NAME."
  (alist-get
   'value
   (seq-find
    (lambda (it)
      (let-alist it
        (string-equal .name name)))
    props)))

(defun projection-ctest-list-artifacts ()
  "List CTest target artifacts."
  (let (result)
    (dolist (test-target (alist-get 'tests (projection-cmake-ctest--targets)))
      (let-alist test-target
        (if .command
            (push
             `((name . ,.name)
               (category . "CTest")
               (arg0 . ,(car .command))
               ,@(when-let ((argv (cdr .command)))
                   `((argv . ,argv)))
               (type . cmake-test)
               (debuggable . t)
               ,@(when-let ((working-directory
                             (projection-ctest--read-property .properties "WORKING_DIRECTORY")))
                   `((working-directory . ,working-directory)))
               ,@(when-let ((environment
                             (projection-ctest--read-property .properties "ENVIRONMENT")))
                   (setq environment
                         (cl-loop
                          for env-variable in environment
                          do (setq env-variable (s-split-up-to "=" env-variable 1))
                          collect (cons (car env-variable)
                                        (or (cadr env-variable) ""))))
                   `((environment . ,environment))))
             result)
          (projection--log :warning "CTest target=%S has no defined command.\
 This is known to happen when the test executable hasn't been compiled yet." test-target)
          )))
    (nreverse result)))

(cl-defmethod projection-artifacts--serialise-artifact (artifact (_type (eql cmake-executable)))
  "Serialize cmake-executable ARTIFACT as executables."
  (projection-artifacts--serialise-artifact artifact 'executable))

(cl-defmethod projection-artifacts--serialise-artifact (artifact (_type (eql cmake-test)))
  "Serialize cmake-test ARTIFACT as executables."
  (projection-artifacts--serialise-artifact artifact 'executable))

(cl-defmethod projection-artifacts--serialise-artifact (artifact (_type (eql cmake-library)))
  "Serialize cmake-library ARTIFACT as executables."
  (projection-artifacts--serialise-artifact artifact 'library))



;;;###autoload (autoload 'projection-cmake-clear-build-directory "projection-type-cmake" nil 'interactive)
(defalias 'projection-cmake-clear-build-directory
  (projection--create-clear-directory-command
   #'projection-cmake--build-directory))



;; CMake compilation commands.

;; The configure step takes the source directory and the output build
;; directory.

(defun projection-cmake-run-configure ()
  "Configure command generator for CMake projects."
  (let-alist (projection-cmake--command-options 'configure)
    (projection--join-shell-command
     `(,@(projection--env-shell-command-prefix .environment)
       "cmake"
       "-S" "."
       ,@(when-let ((build (projection-cmake--build-directory)))
           (list "-B" build))
       ,@(when-let ((log-level (projection-cmake--configure-log-level)))
           (list (concat "--log-level=" log-level)))
       ,@(when-let ((build-type (projection-cmake--build-type)))
           (list (concat "-DCMAKE_BUILD_TYPE=" build-type)))
       ,@.args
       ,@projection-cmake-configure-options))))

;; The remaining commands take the build directory and an optional target
;; with it.

(defun projection-cmake-run-build ()
  "Build command generator for CMake projects."
  (projection-cmake--command
   'build
   (projection--cache-get 'query 'projection-cmake-build-target)))

(defun projection-cmake-run-test ()
  "Test command generator for CMake projects."
  (apply
   #'projection-cmake--ctest-command
   (projection--cache-get 'query 'projection-cmake-ctest-target)))

(defun projection-cmake-run-install ()
  "Install command generator for CMake projects."
  (projection-cmake--command 'install "install"))

(provide 'projection-type-cmake)
;;; projection-type-cmake.el ends here
