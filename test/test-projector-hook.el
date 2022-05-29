;; -*- lexical-binding: t -*-

(require 'f)
(require 'projector-hook)

(describe "Projector hook"
  :var (projector-types dir default-directory projector-hook--cache)
  (before-all
    (setq dir (make-temp-file "buttercup-test-" t)
          default-directory dir))
  (after-all (delete-directory dir t))

  (before-each
    (setq projector-types nil)
    (projector-reset-project-cache projector-hook--cache)
    (projector-register-type 'foo :predicate ".foo")
    (dolist (it (list ".foo" "foo" "bar" "baz"))
      (f-touch it))
    (call-process "git" nil nil nil "init")
    (call-process "git" nil nil nil "commit" "-a" "-m" ""))

  (describe "Hook cache"
    :var (result)

    (it "Can be set and is remembered"
      ;; GIVEN
      ;;   I set the hook for the current project to read-only-mode.
      (projector-hook '(read-only-mode))

      ;; WHEN
      ;;   I fetch the current hooks value for the current project.
      (setq result (projector--cache-get
                    (projector--current-project)
                    'hooks projector-hook--cache))

      ;; THEN
      ;;   The saved hooks for the project match what I set using
      ;;   `projector-hook'.
      (expect result :to-equal '(read-only-mode))))

  ;; (describe "Enabler and disabler"
  ;;   (it "Enables all functions saved in the hook")
  ;;   (it "Uses all functions saved in the hook"))

  (describe "On find-file"
    (before-each
      (projector-hook '(read-only-mode))
      (global-projector-hook-mode +1)
      (spy-on #'projector-hook--execute-hook-list :and-return-value 0))

    (after-each
      (dolist (it '("foo" "bar" "baz"))
        (when-let ((buf (find-buffer-visiting it)))
          (kill-buffer buf))))

    (it "Sets up hooks in each project buffer"
      ;; GIVEN
      ;;   I have a file foo and a hook for read-only-mode setup in the current
      ;;   project.
      ;; WHEN
      ;;   I find a new file in the project.
      (switch-to-buffer (find-file "foo"))

      ;; THEN
      ;;   * projector-hook-mode is enabled in the newly opened file.
      ;;   * Projector enables a hook list containing '(read-only-mode) to be
      ;;   enabled.
      (expect projector-hook-mode :to-be t)
      ;; NOTE:
      ;;   See [[https://lists.gnu.org/archive/html/bug-gnu-emacs/2019-08/msg01094.html][here]] for why this happens twice instead of just once.
      (expect 'projector-hook--execute-hook-list :to-have-been-called-times 2)
      (expect 'projector-hook--execute-hook-list
              :to-have-been-called-with
              "enabling" '(read-only-mode) nil))

    (it "Updates existing project buffers on hook change"
      ;; GIVEN
      ;;   * A project with multiple files, two of which have been opened as
      ;;   buffers.
      ;;   * A project hook of `read-only-mode' having been setup in the current
      ;;   project.
      ;;   * Resulting in read-only-mode being enabled through the hook on each
      ;;   of the open project buffers.
      (let ((f1 (switch-to-buffer (find-file "foo")))
            (f2 (find-file "bar")))
        (dolist (it (list f1 f2))
          (expect (buffer-local-value 'projector-hook-mode it) :to-be t)))

      (expect 'projector-hook--execute-hook-list :to-have-been-called-times 4)
      (expect 'projector-hook--execute-hook-list
              :to-have-been-called-with
              "enabling" '(read-only-mode) nil)
      (expect 'projector-hook--execute-hook-list
              :not :to-have-been-called-with
              "disabling" '(read-only-mode) nil)

      ;; WHEN
      ;;   I update the hook for the current project to be `auto-fill-mode'
      ;;   instead of `read-only-mode'.
      (projector-hook '(auto-fill-mode))

      ;; THEN
      ;;   * projector disabled read-only-mode in each project buffer.
      ;;   * projector enabled auto-fill-mode in each project buffer.
      (expect 'projector-hook--execute-hook-list :to-have-been-called-times 8)
      (expect 'projector-hook--execute-hook-list
              :to-have-been-called-with
              "disabling" '(read-only-mode) 'silent)
      (expect 'projector-hook--execute-hook-list
              :to-have-been-called-with
              "enabling" '(auto-fill-mode) 'silent))))

