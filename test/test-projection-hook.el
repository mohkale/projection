;; -*- lexical-binding: t -*-

(require 'f)
(require 'projection-hook)

(describe "Projection hook"
  :var (projection-types dir default-directory projection-hook--cache)
  (before-all
    (setq dir (make-temp-file "buttercup-test-" t)
          default-directory dir))
  (after-all (delete-directory dir t))

  (before-each
    (setq projection-types nil)
    (projection-reset-project-cache projection-hook--cache)
    (projection-register-type 'foo :predicate ".foo")
    (dolist (it (list ".foo" "foo" "bar" "baz"))
      (f-touch it))
    (call-process "git" nil nil nil "init")
    (call-process "git" nil nil nil "commit" "-a" "-m" ""))

  (describe "Hook cache"
    :var (result)

    (it "Can be set and is remembered"
      ;; GIVEN
      ;;   I set the hook for the current project to read-only-mode.
      (projection-hook '(read-only-mode))

      ;; WHEN
      ;;   I fetch the current hooks value for the current project.
      (setq result (projection--cache-get
                    (projection--current-project)
                    'hooks projection-hook--cache))

      ;; THEN
      ;;   The saved hooks for the project match what I set using
      ;;   `projection-hook'.
      (expect result :to-equal '(read-only-mode))))

  ;; (describe "Enabler and disabler"
  ;;   (it "Enables all functions saved in the hook")
  ;;   (it "Uses all functions saved in the hook"))

  (describe "On find-file"
    (before-each
      (projection-hook '(read-only-mode))
      (global-projection-hook-mode +1)
      (spy-on #'projection-hook--execute-hook-list :and-return-value 0))

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
      ;;   * projection-hook-mode is enabled in the newly opened file.
      ;;   * Projection enables a hook list containing '(read-only-mode) to be
      ;;   enabled.
      (expect projection-hook-mode :to-be t)
      ;; NOTE:
      ;;   See [[https://lists.gnu.org/archive/html/bug-gnu-emacs/2019-08/msg01094.html][here]] for why this happens twice instead of just once.
      (expect 'projection-hook--execute-hook-list :to-have-been-called-times 2)
      (expect 'projection-hook--execute-hook-list
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
          (expect (buffer-local-value 'projection-hook-mode it) :to-be t)))

      (expect 'projection-hook--execute-hook-list :to-have-been-called-times 4)
      (expect 'projection-hook--execute-hook-list
              :to-have-been-called-with
              "enabling" '(read-only-mode) nil)
      (expect 'projection-hook--execute-hook-list
              :not :to-have-been-called-with
              "disabling" '(read-only-mode) nil)

      ;; WHEN
      ;;   I update the hook for the current project to be `auto-fill-mode'
      ;;   instead of `read-only-mode'.
      (projection-hook '(auto-fill-mode))

      ;; THEN
      ;;   * projection disabled read-only-mode in each project buffer.
      ;;   * projection enabled auto-fill-mode in each project buffer.
      (expect 'projection-hook--execute-hook-list :to-have-been-called-times 8)
      (expect 'projection-hook--execute-hook-list
              :to-have-been-called-with
              "disabling" '(read-only-mode) 'silent)
      (expect 'projection-hook--execute-hook-list
              :to-have-been-called-with
              "enabling" '(auto-fill-mode) 'silent))))

