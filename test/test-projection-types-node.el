;; -*- lexical-binding: t -*-

(require 'projection-types)
(require 'projection-multi-npm-scripts)

(describe "Projection node project"
  :var (original-directory
        test-directory
        cmake-types
        projection-project-types)
  ;; Save the original directory before we run any tests.
  (before-all
    (setq original-directory default-directory))
  ;; For each test change to a temporary working directory.
  (before-each
    (cd (setq test-directory (make-temp-file "buttercup-test-" t)))
    (projection-reset-project-cache t))
  ;; And change back to the original directory and delete the test directory
  ;; after the test finishes.
  (after-each
    (when (file-equal-p default-directory test-directory)
      (cd default-directory)
      (delete-directory test-directory t)))

  (before-each
    (setq projection-project-types
          (list projection-project-type-npm)))

  (describe "npm project"
    (before-each
      (projection-find-test--setup-project
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
  \"packages\": {
    \"\": {
      \"name\": \"projection-test\",
      \"version\": \"1.0.0\",
      \"license\": \"ISC\",
      \"dependencies\": {
        \"is-boolean\": \"^0.0.2\"
      }
    },
    \"node_modules/deep-equal\": {
      \"version\": \"0.2.2\",
      \"resolved\": \"https://registry.npmjs.org/deep-equal/-/deep-equal-0.2.2.tgz\",
      \"integrity\": \"sha512-FXgye2Jr6oEk01S7gmSrHrPEQ1ontR7wwl+nYiZ8h4SXlHVm0DYda74BIPcHz2s2qPz4+375IcAz1vsWLwddgQ==\"
    },
    \"node_modules/defined\": {
      \"version\": \"0.0.0\",
      \"resolved\": \"https://registry.npmjs.org/defined/-/defined-0.0.0.tgz\",
      \"integrity\": \"sha512-zpqiCT8bODLu3QSmLLic8xJnYWBFjOSu/fBCm189oAiTtPq/PSanNACKZDS7kgSyCJY7P+IcODzlIogBK/9RBg==\"
    },
    \"node_modules/glob\": {
      \"version\": \"3.2.11\",
      \"resolved\": \"https://registry.npmjs.org/glob/-/glob-3.2.11.tgz\",
      \"integrity\": \"sha512-hVb0zwEZwC1FXSKRPFTeOtN7AArJcJlI6ULGLtrstaswKNlrTJqAA+1lYlSUop4vjA423xlBzqfVS3iWGlqJ+g==\",
      \"dependencies\": {
        \"inherits\": \"2\",
        \"minimatch\": \"0.3\"
      },
      \"engines\": {
        \"node\": \"*\"
      }
    },
    \"node_modules/inherits\": {
      \"version\": \"2.0.4\",
      \"resolved\": \"https://registry.npmjs.org/inherits/-/inherits-2.0.4.tgz\",
      \"integrity\": \"sha512-k/vGaX4/Yla3WzyMCvTQOXYeIHvqOKtnqBduzTHpzpQZzAskKMhZ2K+EnBiSM9zGSoIFeMpXKxa4dYeZIQqewQ==\"
    },
    \"node_modules/is-boolean\": {
      \"version\": \"0.0.2\",
      \"resolved\": \"https://registry.npmjs.org/is-boolean/-/is-boolean-0.0.2.tgz\",
      \"integrity\": \"sha512-NB+rhMWvvrA8RkN4zQ2/EE7bXU7NnXZHIHNyNh5nlvIC0BzQzkQt13VjEVwXe6N4RbqWoANWa8EbemJszzSd2w==\",
      \"dependencies\": {
        \"tape\": \"^3.0.3\"
      }
    },
    \"node_modules/lru-cache\": {
      \"version\": \"2.7.3\",
      \"resolved\": \"https://registry.npmjs.org/lru-cache/-/lru-cache-2.7.3.tgz\",
      \"integrity\": \"sha512-WpibWJ60c3AgAz8a2iYErDrcT2C7OmKnsWhIcHOjkUHFjkXncJhtLxNSqUmxRxRunpb5I8Vprd7aNSd2NtksJQ==\"
    },
    \"node_modules/minimatch\": {
      \"version\": \"0.3.0\",
      \"resolved\": \"https://registry.npmjs.org/minimatch/-/minimatch-0.3.0.tgz\",
      \"integrity\": \"sha512-WFX1jI1AaxNTZVOHLBVazwTWKaQjoykSzCBNXB72vDTCzopQGtyP91tKdFK5cv1+qMwPyiTu1HqUriqplI8pcA==\",
      \"deprecated\": \"Please update to minimatch 3.0.2 or higher to avoid a RegExp DoS issue\",
      \"dependencies\": {
        \"lru-cache\": \"2\",
        \"sigmund\": \"~1.0.0\"
      },
      \"engines\": {
        \"node\": \"*\"
      }
    },
    \"node_modules/object-inspect\": {
      \"version\": \"0.4.0\",
      \"resolved\": \"https://registry.npmjs.org/object-inspect/-/object-inspect-0.4.0.tgz\",
      \"integrity\": \"sha512-8WvkvUZiKAjjsy/63rJjA7jw9uyF0CLVLjBKEfnPHE3Jxvs1LgwqL2OmJN+LliIX1vrzKW+AAu02Cc+xv27ncQ==\"
    },
    \"node_modules/resumer\": {
      \"version\": \"0.0.0\",
      \"resolved\": \"https://registry.npmjs.org/resumer/-/resumer-0.0.0.tgz\",
      \"integrity\": \"sha512-Fn9X8rX8yYF4m81rZCK/5VmrmsSbqS/i3rDLl6ZZHAXgC2nTAx3dhwG8q8odP/RmdLa2YrybDJaAMg+X1ajY3w==\",
      \"dependencies\": {
        \"through\": \"~2.3.4\"
      }
    },
    \"node_modules/sigmund\": {
      \"version\": \"1.0.1\",
      \"resolved\": \"https://registry.npmjs.org/sigmund/-/sigmund-1.0.1.tgz\",
      \"integrity\": \"sha512-fCvEXfh6NWpm+YSuY2bpXb/VIihqWA6hLsgboC+0nl71Q7N7o2eaCW8mJa/NLvQhs6jpd3VZV4UiUQlV6+lc8g==\"
    },
    \"node_modules/tape\": {
      \"version\": \"3.6.1\",
      \"resolved\": \"https://registry.npmjs.org/tape/-/tape-3.6.1.tgz\",
      \"integrity\": \"sha512-Qy+shSMwr+bg5NwJhrdCKNOS7BEoo/SEjE+dF4a2OYR73f6ocH5ioLIHE6TuttjONmR3HYlOXwSqFTxUDFJtGg==\",
      \"dependencies\": {
        \"deep-equal\": \"~0.2.0\",
        \"defined\": \"~0.0.0\",
        \"glob\": \"~3.2.9\",
        \"inherits\": \"~2.0.1\",
        \"object-inspect\": \"~0.4.0\",
        \"resumer\": \"~0.0.0\",
        \"through\": \"~2.3.4\"
      },
      \"bin\": {
        \"tape\": \"bin/tape\"
      }
    },
    \"node_modules/through\": {
      \"version\": \"2.3.8\",
      \"resolved\": \"https://registry.npmjs.org/through/-/through-2.3.8.tgz\",
      \"integrity\": \"sha512-w89qg7PI8wAdvX60bMDP+bFoD5Dvhm9oLheFp5O4a2QF0cSBGsBX4qZmadPMvVqlLJBBci+WqGGOAPvcDeNSVg==\"
    }
  },
  \"dependencies\": {
    \"deep-equal\": {
      \"version\": \"0.2.2\",
      \"resolved\": \"https://registry.npmjs.org/deep-equal/-/deep-equal-0.2.2.tgz\",
      \"integrity\": \"sha512-FXgye2Jr6oEk01S7gmSrHrPEQ1ontR7wwl+nYiZ8h4SXlHVm0DYda74BIPcHz2s2qPz4+375IcAz1vsWLwddgQ==\"
    },
    \"defined\": {
      \"version\": \"0.0.0\",
      \"resolved\": \"https://registry.npmjs.org/defined/-/defined-0.0.0.tgz\",
      \"integrity\": \"sha512-zpqiCT8bODLu3QSmLLic8xJnYWBFjOSu/fBCm189oAiTtPq/PSanNACKZDS7kgSyCJY7P+IcODzlIogBK/9RBg==\"
    },
    \"glob\": {
      \"version\": \"3.2.11\",
      \"resolved\": \"https://registry.npmjs.org/glob/-/glob-3.2.11.tgz\",
      \"integrity\": \"sha512-hVb0zwEZwC1FXSKRPFTeOtN7AArJcJlI6ULGLtrstaswKNlrTJqAA+1lYlSUop4vjA423xlBzqfVS3iWGlqJ+g==\",
      \"requires\": {
        \"inherits\": \"2\",
        \"minimatch\": \"0.3\"
      }
    },
    \"inherits\": {
      \"version\": \"2.0.4\",
      \"resolved\": \"https://registry.npmjs.org/inherits/-/inherits-2.0.4.tgz\",
      \"integrity\": \"sha512-k/vGaX4/Yla3WzyMCvTQOXYeIHvqOKtnqBduzTHpzpQZzAskKMhZ2K+EnBiSM9zGSoIFeMpXKxa4dYeZIQqewQ==\"
    },
    \"is-boolean\": {
      \"version\": \"0.0.2\",
      \"resolved\": \"https://registry.npmjs.org/is-boolean/-/is-boolean-0.0.2.tgz\",
      \"integrity\": \"sha512-NB+rhMWvvrA8RkN4zQ2/EE7bXU7NnXZHIHNyNh5nlvIC0BzQzkQt13VjEVwXe6N4RbqWoANWa8EbemJszzSd2w==\",
      \"requires\": {
        \"tape\": \"^3.0.3\"
      }
    },
    \"lru-cache\": {
      \"version\": \"2.7.3\",
      \"resolved\": \"https://registry.npmjs.org/lru-cache/-/lru-cache-2.7.3.tgz\",
      \"integrity\": \"sha512-WpibWJ60c3AgAz8a2iYErDrcT2C7OmKnsWhIcHOjkUHFjkXncJhtLxNSqUmxRxRunpb5I8Vprd7aNSd2NtksJQ==\"
    },
    \"minimatch\": {
      \"version\": \"0.3.0\",
      \"resolved\": \"https://registry.npmjs.org/minimatch/-/minimatch-0.3.0.tgz\",
      \"integrity\": \"sha512-WFX1jI1AaxNTZVOHLBVazwTWKaQjoykSzCBNXB72vDTCzopQGtyP91tKdFK5cv1+qMwPyiTu1HqUriqplI8pcA==\",
      \"requires\": {
        \"lru-cache\": \"2\",
        \"sigmund\": \"~1.0.0\"
      }
    },
    \"object-inspect\": {
      \"version\": \"0.4.0\",
      \"resolved\": \"https://registry.npmjs.org/object-inspect/-/object-inspect-0.4.0.tgz\",
      \"integrity\": \"sha512-8WvkvUZiKAjjsy/63rJjA7jw9uyF0CLVLjBKEfnPHE3Jxvs1LgwqL2OmJN+LliIX1vrzKW+AAu02Cc+xv27ncQ==\"
    },
    \"resumer\": {
      \"version\": \"0.0.0\",
      \"resolved\": \"https://registry.npmjs.org/resumer/-/resumer-0.0.0.tgz\",
      \"integrity\": \"sha512-Fn9X8rX8yYF4m81rZCK/5VmrmsSbqS/i3rDLl6ZZHAXgC2nTAx3dhwG8q8odP/RmdLa2YrybDJaAMg+X1ajY3w==\",
      \"requires\": {
        \"through\": \"~2.3.4\"
      }
    },
    \"sigmund\": {
      \"version\": \"1.0.1\",
      \"resolved\": \"https://registry.npmjs.org/sigmund/-/sigmund-1.0.1.tgz\",
      \"integrity\": \"sha512-fCvEXfh6NWpm+YSuY2bpXb/VIihqWA6hLsgboC+0nl71Q7N7o2eaCW8mJa/NLvQhs6jpd3VZV4UiUQlV6+lc8g==\"
    },
    \"tape\": {
      \"version\": \"3.6.1\",
      \"resolved\": \"https://registry.npmjs.org/tape/-/tape-3.6.1.tgz\",
      \"integrity\": \"sha512-Qy+shSMwr+bg5NwJhrdCKNOS7BEoo/SEjE+dF4a2OYR73f6ocH5ioLIHE6TuttjONmR3HYlOXwSqFTxUDFJtGg==\",
      \"requires\": {
        \"deep-equal\": \"~0.2.0\",
        \"defined\": \"~0.0.0\",
        \"glob\": \"~3.2.9\",
        \"inherits\": \"~2.0.1\",
        \"object-inspect\": \"~0.4.0\",
        \"resumer\": \"~0.0.0\",
        \"through\": \"~2.3.4\"
      }
    },
    \"through\": {
      \"version\": \"2.3.8\",
      \"resolved\": \"https://registry.npmjs.org/through/-/through-2.3.8.tgz\",
      \"integrity\": \"sha512-w89qg7PI8wAdvX60bMDP+bFoD5Dvhm9oLheFp5O4a2QF0cSBGsBX4qZmadPMvVqlLJBBci+WqGGOAPvcDeNSVg==\"
    }
  }
}"))))

    (it "Can extract available scripts"
      ;; GIVEN
      ;;   A npm project.
      ;; WHEN
      ;;   I extract available npm script targets.
      (let ((targets (projection-multi-npm-script-targets)))
        ;; THEN
        ;;   The targets were extracted from the projects scripts correctly.
        (expect targets :to-equal
                '(("npm:foo" . "npm run foo")
                  ("npm:baz" . "npm run baz"))))))
  )
