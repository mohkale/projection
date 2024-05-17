;; -*- lexical-binding: t -*-

(require 'projection-artifacts)
(require 'projection-types)
(require 'projection-utils)

(require 'projection-test-utils)

(describe "Project type VSCode Tasks"
  (+projection-test-setup)

  (before-each
    (setq projection-project-types (list projection-project-type-cmake))

    (+projection-setup-project
     '((".vscode"
        ("tasks.json" . "{
                          \"version\": \"2.0.0\",
                          \"tasks\": [
                                 {
                                     \"type\": \"unsupported-type\",
                                     \"label\": \"Run unsupported type\",
                                     \"command\": \"./scripts/test.sh\"
                                 },
                                 {
                                     \"label\": \"Run without type\",
                                     \"command\": \"./scripts/test.sh\"
                                 },
                                 {
                                     \"type\": \"shell\",
                                     \"label\": \"Run without command\"
                                 },
                                 {
                                     \"type\": \"shell\",
                                     \"command\": \"no label command\"
                                 },
                                 {
                                     \"type\": \"process\",
                                     \"label\": \"Run with type process\",
                                     \"command\": \"./scripts/test.sh\"
                                 },
                                 {
                                     \"type\": \"shell\",
                                     \"label\": \"Run basic\",
                                     \"command\": \"./scripts/test.sh\"
                                 },
                                 {
                                     \"type\": \"shell\",
                                     \"label\": \"Run with group\",
                                     \"group\": \"groupped\",
                                     \"command\": \"./scripts/test.sh\"
                                 },
                                 {
                                     \"type\": \"shell\",
                                     \"label\": \"Run with properties group\",
                                     \"group\": {
                                         \"kind\": \"groupped\",
                                         \"property\": \"x\"
                                     },
                                     \"command\": \"./scripts/test.sh\"
                                 },
                                 {
                                     \"type\": \"shell\",
                                     \"label\": \"Run with directory option\",
                                     \"command\": \"./scripts/test.sh\",
                                     \"options\": {
                                         \"cwd\": \"the-directory\"
                                     }
                                 },
                                 {
                                     \"type\": \"shell\",
                                     \"label\": \"Run with env option\",
                                     \"command\": \"./scripts/test.sh\",
                                     \"options\": {
                                         \"env\": {
                                             \"foo\": \"bar\",
                                             \"baz\": \"bag\",
                                             \"bam\": \"boom\"
                                         }
                                     }
                                 },
                                 {
                                     \"type\": \"shell\",
                                     \"label\": \"Run with args\",
                                     \"command\": \"./scripts/test.sh\",
                                     \"args\": [\"arg1\", \"arg2\", \"arg3\"]
                                 },
                                 {
                                     \"type\": \"shell\",
                                     \"label\": \"Run with args struct\",
                                     \"command\": \"./scripts/test.sh\",
                                     \"args\": [\"arg1\", {\"value\": \"arg2\", \"property\": \"x\"}, \"arg3\"]
                                 }
                             ]
                         }")))))

  (describe "Multi compile"
    (before-all (require 'projection-multi-vscode-tasks))

    (it "Can extract task targets from tasks.json"
      ;; WHEN
      (let ((targets (projection-multi-compile-vscode-targets)))
        ;; THEN
        (expect targets :to-equal
                '(("vscode:Run without type" . "./scripts/test.sh")
                  ("vscode:no label command" . "no\\ label\\ command")
                  ("vscode:Run with type process" . "./scripts/test.sh")
                  ("vscode:Run basic" . "./scripts/test.sh")
                  ("vscode:groupped:Run with group" . "./scripts/test.sh")
                  ("vscode:groupped:Run with properties group" . "./scripts/test.sh")
                  ("vscode:Run with directory option"
                   . "env --chdir\\=the-directory ./scripts/test.sh")
                  ("vscode:Run with env option"
                   . "env foo\\=bar baz\\=bag bam\\=boom ./scripts/test.sh")
                  ("vscode:Run with args" . "./scripts/test.sh arg1 arg2 arg3")
                  ("vscode:Run with args struct" . "./scripts/test.sh arg1 arg2 arg3")))))))
