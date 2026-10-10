;; -*- lexical-binding: t -*-

(require 'projection-type-cmake)

(describe "CMake file API reply reader"
  :var (build-dir reply-dir)

  (before-each
    (setq build-dir (make-temp-file "projection Café cmake replies " t)
          reply-dir (expand-file-name
                     (projection-cmake--file-api-reply-directory-suffix)
                     build-dir))
    (make-directory reply-dir t))

  (after-each
    (delete-directory build-dir t))

  (it "Reads local replies without invoking a shell"
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file (expand-file-name "target-run_app.json" reply-dir)
        (insert "{\"name\":\"run_app\",\"type\":\"UTILITY\","
                "\"paths\":{\"source\":\"C:/Project Café\"},"
                "\"dependencies\":[{\"id\":\"app\"}]}"))
      ;; Non-JSON files must not interfere with target discovery.
      (with-temp-file (expand-file-name "notes.txt" reply-dir)
        (insert "not JSON")))
    (spy-on 'projection--shell-command-to-string
            :and-call-fake (lambda (&rest _) (error "Unexpected shell command")))

    ;; The reply format must not depend on the user's JSON preferences.
    (let* ((json-array-type 'vector)
           (json-object-type 'hash-table)
           (json-key-type 'string)
           (replies (projection-cmake--file-api-bulk-read-reply-directory build-dir)))
      (expect replies
              :to-equal
              '(("target-run_app.json"
                 (name . "run_app")
                 (type . "UTILITY")
                 (paths (source . "C:/Project Café"))
                 (dependencies ((id . "app")))))))
    (expect 'projection--shell-command-to-string :not :to-have-been-called))

  (it "Preserves JSON arrays, nulls, and booleans"
    (with-temp-file (expand-file-name "values.json" reply-dir)
      (insert "{\"items\":[1,2],\"empty\":[],\"null\":null,"
              "\"true\":true,\"false\":false}"))
    ;; Preserve both the defaults and customized legacy JSON sentinels.
    (dolist (sentinels (list (list json-null json-false)
                            '(custom-null custom-false)))
      (let* ((json-null (car sentinels))
             (json-false (cadr sentinels))
             (replies (projection-cmake--file-api-bulk-read-reply-directory build-dir)))
        (expect (cdr (assoc "values.json" replies))
                :to-equal
                (list '(items 1 2)
                      '(empty)
                      (cons 'null json-null)
                      '(true . t)
                      (cons 'false json-false))))))

  (it "Extracts custom targets from local code-model replies"
    (with-temp-file (expand-file-name "index-2026.json" reply-dir)
      (insert "{\"reply\":{\"client-emacs-projection\":{\"query.json\":{"
              "\"responses\":[{\"jsonFile\":\"codemodel.json\"}]}}}}"))
    (with-temp-file (expand-file-name "codemodel.json" reply-dir)
      (insert "{\"configurations\":[{\"name\":\"Debug\","
              "\"targets\":[{\"jsonFile\":\"target-run_app.json\"}]}]}"))
    (with-temp-file (expand-file-name "target-run_app.json" reply-dir)
      (insert "{\"name\":\"run_app\",\"type\":\"UTILITY\"}"))
    (spy-on 'projection-cmake--build-directory :and-return-value build-dir)
    (spy-on 'projection--shell-command-to-string
            :and-call-fake (lambda (&rest _) (error "Unexpected shell command")))

    (let* ((model (projection-cmake--file-api-code-model2))
           (targets (cdr (assoc "Debug" (alist-get 'targets-by-config model)))))
      (expect (mapcar (lambda (target) (alist-get 'name target)) targets)
              :to-equal '("run_app"))))

  (it "Returns no replies for an empty directory"
    (expect (projection-cmake--file-api-bulk-read-reply-directory build-dir)
            :to-be nil))

  (it "Signals a missing reply directory so the caller can handle it"
    (expect (projection-cmake--file-api-bulk-read-reply-directory
             (expand-file-name "missing" build-dir))
            :to-throw 'file-missing))

  (it "Lets the code-model reader gracefully handle malformed JSON"
    (with-temp-file (expand-file-name "broken.json" reply-dir)
      (insert "{"))
    (spy-on 'projection-cmake--build-directory :and-return-value build-dir)

    (expect (projection-cmake--file-api-code-model2) :to-be nil))

  (it "Keeps the bulk reader for remote build directories"
    (let ((remote-build-dir "/ssh:example:/project/build/")
          (replies '(("target.json" (name . "main")))))
      (spy-on 'file-remote-p :and-return-value "/ssh:example:")
      (spy-on 'projection--shell-command-to-string
              :and-call-fake (lambda (&rest _) (error "Unexpected shell command")))
      (spy-on 'projection-cmake--file-api-bulk-read-remote-reply-directory
              :and-return-value replies)

      (expect (projection-cmake--file-api-bulk-read-reply-directory remote-build-dir)
              :to-equal replies)
      (expect 'projection-cmake--file-api-bulk-read-remote-reply-directory
              :to-have-been-called-with remote-build-dir))))
