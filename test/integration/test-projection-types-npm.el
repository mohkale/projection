;; -*- lexical-binding: t -*-

(require 'projection-types)

(require 'projection-test-utils)
(require 'projection-multi-npm-scripts)

(describe "Project type NPM"
  (+projection-test-setup)

  (before-each
    (setq projection-project-types (list projection-project-type-npm))

    (+projection-setup-project
     '(("package.json" . "{
  \"name\": \"projection-test\",
  \"version\": \"1.0.0\",
  \"description\": \"\",
  \"main\": \"index.js\",
  \"scripts\": {
    \"foo\": \"foo bar\",
    \"baz\": \"baz bag\"
  },
  \"author\": \"\",
  \"license\": \"ISC\",
  \"dependencies\": {
    \"is-boolean\": \"^0.0.2\"
  }
}")
       ("package-lock.json" . "{
  \"name\": \"projection-test\",
  \"version\": \"1.0.0\",
  \"lockfileVersion\": 2,
  \"requires\": true,
  \"packages\": {},
  \"dependencies\": {}
}"))))

  (it "Can be identified"
    (+projection-project-matches-p 'npm))

  (describe "Multi compile"
    (it "Can extract available scripts"
      ;; WHEN
      (let ((targets (projection-multi-npm-script-targets)))
        ;; THEN
        (expect targets :to-equal
                '(("npm:foo" . "npm run foo")
                  ("npm:baz" . "npm run baz")))))))
