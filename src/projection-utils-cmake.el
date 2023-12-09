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

(require 'f)
(require 'json)

(require 'projection-core)
(require 'projection-core-completion)
(require 'projection-core-commands)
(require 'projection-core-log)
(require 'projection-utils)

(defgroup projection-type-cmake nil
  "Projection CMake project type."
  :group 'projection-types)

(defcustom projection-cmake-target-backend 'help-target
  "Which data source to query CMake targets from."
  :type '(choice
          (const :tag "Call the help target and parse the output." help-target)
          (const :tag "Use the CMake file api and parse the targets from the codemodel.
This is a much more reliable source for querying CMake targets because it doesn't include any phony
targets CMake might add just for build framework integrations or dummy tasks like directory creation."
                 code-model))
  :group 'projection-type-cmake)



;; CMake reading presets.

(defconst projection-cmake-preset-files
  '("CMakePresets.json" "CMakeUserPresets.json")
  "List of files configuring CMake presets.")

(defcustom projection-cmake-cache-presets 'auto
  "When true cache the list of CMake presets associated with each project."
  :type '(choice
          (const :tag "Cache presets and invalidate cache automatically" auto)
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

(projection--declare-cache-var
  'projection-cmake-presets
  :title "CMake presets"
  :category "CMake"
  :description "CMake presets collection"
  :hide t)

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

(defcustom projection-cmake-preset 'prompt-once-when-multiple
  "Set which CMake preset to use for the current project.
See https://cmake.org/cmake/help/latest/manual/cmake-presets.7.html."
  :type
  '(choice
    (choice (string :tag "Default preset")
            (const :tag "No default preset" nil))
    (const :tag "Do not supply a preset value to CMake" disable)
    (const :tag "Return configured preset non-interactively" silent)

    (const :tag "Always prompt for which preset to use" prompt-always)
    (const :tag "Always prompt and then reuse the chosen preset"
           prompt-once)
    (const :tag "Prompt when multiple presets available and then reuse the chosen preset"
           prompt-once-when-multiple)
    (alist :key-type (symbol :tag "Build type")
           :value-type (sexp :tag "One of the other possible preset values")))
  :group 'projection-type-cmake)

(defun projection-cmake--preset-cache-var (&optional build-type)
  "Fetch the project cache variable for the BUILD-TYPE CMake preset."
  (or build-type (setq build-type 'default))
  (intern (concat "projection-cmake-" (symbol-name build-type) "-preset")))

(defun projection-cmake--read-preset (prompt presets)
  "Interactively select a preset from PRESETS.
Prompt for the `completing-read' session will be PROMPT."
  (let* ((completion-table
          (projection-completion--completion-table
           :candidates presets
           :annotation-function
           (projection-completion--annotation-function
            :key-function (lambda (cand) (cdr (assoc cand presets))))))
         (result (completing-read prompt completion-table)))
    (unless (string-empty-p result)
      result)))

(defun projection-cmake--preset (&optional build-type)
  "Fetch the CMake preset for the current BUILD-TYPE respecting project cache."
  (cl-block nil
    ;; When an alist preset configuration re-invoke with the configuration for
    ;; the build type instead of the alist. This allows reusing the preset choice
    ;; for each build-type independently.
    (when-let ((projection-cmake-preset
                (when (consp projection-cmake-preset)
                  (alist-get (or build-type t) projection-cmake-preset))))
      (cl-return (projection-cmake--preset build-type)))

    (let* ((project (projection--current-project 'no-error))
           (cache-var
            (projection-cmake--preset-cache-var build-type))
           (default-cache-var (projection-cmake--preset-cache-var))
           presets ; Collection of actual presets for BUILD-TYPE
           preset  ; The preset value chosen by the user (for caching)
           )

      ;; If we have a cached preset value it takes priority over the configuration
      ;; options. This is so you can update the options interactively and don't
      ;; have to tweak around with configuration variables.
      ;;
      ;; TODO: Should prompt-always override the configuration entry? Maybe
      ;; it should be prompt-always-unless-cached?
      (when-let ((cached-preset
                  (or
                   (when (and project
                              (not (eq projection-cmake-preset 'prompt-always)))
                     (projection--cache-get project cache-var))
                   (projection--cache-get project default-cache-var))))
        (cl-return cached-preset))

      ;; Always prefer the configured entries over prompting the user.
      (when (eq projection-cmake-preset 'disable)
        (cl-return nil))
      (when (stringp projection-cmake-preset)
        (cl-return projection-cmake-preset))

      ;; Read the presets for build-type and prompt the user with them.
      (when
          (and
           (not (eq projection-cmake-preset 'silent))
           (setq presets
                 (projection-cmake--list-presets-for-build-type build-type))
           (setq preset
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
                    presets)))))
        ;; Cache the preset if configured to only prompt once.
        (when (and build-type
                   (member projection-cmake-preset
                           '(prompt-once prompt-once-when-multiple)))
          (projection-cmake-set-preset project build-type preset))
        (cl-return preset)))))

(defconst projection-cmake--preset-build-types
  '(configure build test package workflow)
  "List of build-types that support CMake preset configurations.")

;;;###autoload
(defun projection-cmake-set-preset (project build-type preset)
  "Set CMake preset for BUILD-TYPE to PRESET for PROJECT."
  (interactive
   (let* ((project (projection--current-project))
          (default-directory (project-root project))
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
             "Set CMake%s preset: " project
             (if build-type
                 (concat " " (symbol-name build-type))
               ""))
            (projection-cmake--list-presets-for-build-type build-type)))
     (list project build-type preset)))
  (projection--cache-put
   project (projection-cmake--preset-cache-var build-type) preset))

(dolist (build-type projection-cmake--preset-build-types)
  (projection--declare-cache-var
    (projection-cmake--preset-cache-var build-type)
    :title (format "CMake %s preset" (symbol-name build-type))
    :category "CMake"
    :description (concat "The CMake preset for the " (symbol-name build-type))
    :hide t))

(projection--declare-cache-var
  (projection-cmake--preset-cache-var nil)
  :title "CMake default preset"
  :category "CMake"
  :description (concat "The default CMake preset")
  :hide t)



;;;###autoload (autoload 'projection-cmake-set-build-type "projection-utils-cmake" nil 'interactive)
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



;; CMake file API [[man:cmake-file-api(7)]].

(defconst projection-cmake--file-api-client "client-emacs-projection")

(defcustom projection-cmake-cache-code-model 'auto
  "When true cache the CMake code-model of each project."
  :type '(choice
          (const :tag "Cache targets and invalidate cache automatically" auto)
          (boolean :tag "Always/Never cache targets"))
  :group 'projection-type-cmake)

(cl-defsubst projection-cmake--file-api-directory ()
  "Path to the CMake file-api directory under the build directory."
  (f-join (projection-cmake--build-directory 'expand) ".cmake" "api" "v1"))

(cl-defsubst projection-cmake--file-api-query-file-suffix ()
  "Subpath to the CMake query file for the projection client."
  (f-join "query" projection-cmake--file-api-client "query.json"))

(cl-defsubst projection-cmake--file-api-reply-directory-suffix ()
  "Subpath to the CMake reply directory for the projection client."
  (identity "reply"))

(defun projection-cmake--file-api-code-model ()
  "Get the generated code-model object for the projection client.
This function respects `projection-cmake-cache-code-model'."
  (projection--cache-get-with-predicate
   (projection--current-project 'no-error)
   'projection-cmake-code-model
   (cond
    ((eq projection-cmake-cache-code-model 'auto)
     (projection--cmake-configure-modtime-p))
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

  (if-let* ((api-directory (projection-cmake--file-api-directory))
            (reply-directory
             (f-join api-directory (projection-cmake--file-api-reply-directory-suffix)))
            (index-file
             (car (cl-sort (file-expand-wildcards (f-join reply-directory "index-*.json"))
                           #'string>))))
      (condition-case err
          (let* ((codemodel (thread-last
                              index-file
                              (projection-cmake--file-api-read-file)
                              (projection-cmake--file-api-query-code-model-file)
                              (f-join reply-directory)
                              (projection-cmake--file-api-read-file)))
                 (targets-by-config
                  (thread-last
                    codemodel
                    (alist-get 'configurations)
                    (mapcar (apply-partially
                             #'projection-cmake--file-api-query-config-targets
                             reply-directory)))))
            `((codemodel . ,codemodel)
              (targets-by-config . ,targets-by-config)))
        ((file-missing json-readtable-error projection-cmake-code-model)
         (projection--log :error "error while querying CMake code-model %s." (cdr err))))
    (projection--log :warning "Cannot query cmake codemodel because no indexes exist.")))

(cl-defsubst projection-cmake--file-api-read-file (file)
  "Read JSON file FILE for the CMake file-api."
  (with-temp-buffer
    (projection--log :debug "Reading codemodel file=%s." file)
    (insert-file-contents file)
    (let ((json-array-type 'list)) (json-read))))

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

(cl-defsubst projection-cmake--file-api-query-config-targets (reply-directory config-obj)
  "Read target entries from the code-model CONFIG-OBJ in REPLY-DIRECTORY."
  (if-let ((name (alist-get 'name config-obj)))
      (cons name
            (thread-last
              config-obj
              (alist-get 'targets)
              (mapcar (apply-partially #'alist-get 'jsonFile))
              (mapcar (apply-partially #'f-join reply-directory))
              (mapcar #'projection-cmake--file-api-read-file)))
    (signal 'projection-cmake-code-model "Encountered configuration object with no name")))

;;;###autoload
(defun projection-cmake--file-api-create-query-file ()
  "Create a file-api client query file for projection."
  (let ((query-file (f-join
                     (projection-cmake--file-api-directory)
                     (projection-cmake--file-api-query-file-suffix))))
    (unless (file-exists-p query-file)
      (with-temp-buffer
        (insert
         (json-serialize '((requests . [((kind . "codemodel") (version . 2))])
                           (client))
                         :false-object :json-false))
        (make-directory (f-dirname query-file) 'parents)
        (write-region nil nil query-file nil 'silent)))))

;;;###autoload
(progn
  (defun projection-cmake--cmake-project-p (project-types)
    "Helper to check whether one of the types in PROJECT-TYPES is CMake.
Advise this if you need more than just the CMake project type to be have a
query file created before configuring."
    (member 'cmake (mapcar #'projection-type--name project-types)))

  (add-hook
   'projection-commands-pre-configure-hook
   (defun projection-cmake--file-api-create-query-hook (project)
     "Helper to create a CMake query file before configuring for CMake projects."
     (when (and (projection-cmake--cmake-project-p
                 (projection-project-types (project-root project)))
                (eq projection-cmake-target-backend 'code-model))
       (projection-cmake--file-api-create-query-file)))))



;; CMake command utils.

(defcustom projection-cmake-build-directory "build"
  "Build directory for cmake project builds.
When unset no -B flag will be passed to CMake. You may want this if the build
directory is configured directly in the CMakePresets or elsewhere."
  :type '(optional string)
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

(defun projection--cmake-command (&optional build-type target)
  "Generate a CMake command optionally to run TARGET for BUILD-TYPE."
  (projection--join-shell-command
   `("cmake"
     ,@(when-let ((build projection-cmake-build-directory))
         (list "--build" build))
     ,@(when-let ((preset (projection-cmake--preset build-type)))
         (list (concat "--preset=" preset)))
     ,@(when target (list "--target" target)))))

(defun projection--cmake-annotation (build-type target)
  "Generate an annotation for a cmake command to run TARGET for BUILD-TYPE."
  (format "cmake %s%s%s"
          (if-let ((preset (projection-cmake--preset build-type)))
              (concat "preset:" preset " ")
            "")
          (if-let ((build (projection-cmake--build-directory)))
              (concat "build:" build " ")
            "")
          target))

(defun projection--cmake-workflow-command (preset)
  "Generate a CMake command to run the workflow PRESET."
  (projection--join-shell-command
   `("cmake"
     "--workflow"
     ,(concat "--preset=" preset))))

(defun projection--cmake-workflow-annotation (preset)
  "Generate an annotation for a cmake command to run a workflow PRESET."
  (format "cmake %s%s%s"
          (concat "preset:" preset " ")
          (if-let ((build (projection-cmake--build-directory)))
              (concat "build:" build " ")
            "")
          "workflow"))



;; ctest command utils.

(defcustom projection-cmake-ctest-options '("--output-on-failure")
  "Default CTest invocation options.
Set the extra command line options to pass to ctest."
  :type '(list string)
  :group 'projection-type-cmake)

(defcustom projection-cmake-ctest-jobs nil
  "Value of the --parallel option passed to CTest when set."
  :type '(optional
          (choice
           (const -1 :tag "Use `num-processors'.")
           (const -2 :tag "Use half of `num-processors'.")
           (integer :tag "Use this value as the number of jobs.")))
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

(defun projection-cmake-ctest--jobs ()
  "Query the number of parallel jobs to use for ctest."
  (when projection-cmake-ctest-jobs
    (pcase projection-cmake-ctest-jobs
      (-1 (num-processors))
      (-2 (/ (num-processors) 2))
      (0 nil)
      ((cl-type integer) projection-cmake-ctest-jobs)
      (_ (projection--log
          :warning "Unsupported `projection-cmake-ctest-jobs' value: %S."
          projection-cmake-ctest-jobs)
         nil))))

(defun projection--cmake-ctest-command (&rest argv)
  "Helper function to  generate a CTest command.
ARGV if provided will be appended to the command."
  (projection--join-shell-command
   `(,@(when projection-cmake-ctest-environment-variables
         (append (list "env")
                 (cl-loop for (key . value) in
                          projection-cmake-ctest-environment-variables
                          collect (concat key "=" value))))
     "ctest"
     ,@(when-let ((build (projection-cmake--build-directory)))
         (list "--test-dir" build))
     ,@(when-let ((preset (projection-cmake--preset 'test)))
         (list (concat "--preset=" preset)))
     ,@(when-let ((job-count (projection-cmake-ctest--jobs)))
         (list (concat "--parallel=" (number-to-string job-count))))
     ,@projection-cmake-ctest-options
     ,@argv)))

(defun projection--cmake-ctest-annotation (target)
  "Generate an annotation for a ctest command to run TARGET."
  (format "ctest %s%s%s"
          (if-let ((preset (projection-cmake--preset 'test)))
              (concat "preset:" preset " ")
            "")
          (if-let ((build (projection-cmake--build-directory)))
              (concat "build:" build " ")
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



;;;###autoload
(defun projection-cmake-clear-build-directory ()
  "Interactively run a compilation command to remove the build directory."
  (interactive)
  (let ((build (projection-cmake--build-directory))
        (build-expanded (projection-cmake--build-directory 'expand)))
    (cond
     ((or (not build) (not build-expanded))
      (user-error "Cannot remove build directory at unconfigured location. \
See `projection-cmake-build-directory'"))
     ((not (file-exists-p build-expanded))
      (user-error "Build directory %s already does not exist" build-expanded))
     ((yes-or-no-p (format "Really remove build directory at `%s'?" build-expanded))
      (compile (projection--join-shell-command (list "rm" "-rf" build))))
     (t (message "Aborted removal of build directory at %s" build-expanded)))))



;; CMake compilation commands.

;; The configure step takes the source directory and the output build
;; directory.

(defun projection-cmake-run-configure ()
  "Configure command generator for CMake projects."
  (projection--join-shell-command
   `("cmake"
     "-S" "."
     ,@(when-let ((build (projection-cmake--build-directory)))
        (list "-B" build))
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
  (projection--cmake-ctest-command "test"))

(defun projection-cmake-run-install ()
  "Install command generator for CMake projects."
  (projection--cmake-command 'install "install"))

(provide 'projection-utils-cmake)
;;; projection-utils-cmake.el ends here
