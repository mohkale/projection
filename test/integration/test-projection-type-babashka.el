;; -*- lexical-binding: t -*-

(require 'projection-types)

(require 'projection-multi-bb)
(require 'projection-test-utils)

(describe "Project type Babashka"
          (+projection-test-setup)

          (before-each
           (setq projection-project-types (list projection-project-type-babashka))
           (+projection-setup-project
            '(("bb.edn" . "
{:tasks
 {:requires ([babashka.fs :as fs]
             [clojure.edn :as edn]
             [clojure.pprint :as pp]
             [clojure.string :as str])

  clean {:doc \"Delete build artifacts.\"
         :task (do (fs/delete-tree \".shadow-cljs\")
                   (fs/delete-tree \"lib\"))}

  compile {:doc \"Build the project.\"
           :task
           (build
             (wrap-cmd \"-M -m shadow.cljs.devtools.cli --force-spawn compile modules\")
             *command-line-args*)}

  dev {:doc \"Run shadow in watch mode with tests enabled.\"
       :task
       (binding [*test* true]
         (println \"Starting shadow-cljs in watch mode.\")
         (clojure \"-M -m shadow.cljs.devtools.cli --force-spawn watch modules\"))}

  task-without-doc (shell \"no doc!!\")

  release {:depends [clean]
           :doc \"Compiles release build.\"
           :task (shell \"build build build\")}}}"))))

          (it "Can be identified"
              (+projection-project-matches-p 'babashka))

          (describe "Multi compile"
                    (before-all (require 'projection-multi-bb))

                    (it "Can extract tasks from bb.edn"
                        (let ((targets (projection-multi-bb--tasks)))
                          (expect targets :to-equal
                                  '(("clean" "Delete build artifacts.")
                                    ("compile" "Build the project.")
                                    ("dev" "Run shadow in watch mode with tests enabled.")
                                    ("task-without-doc" "")
                                    ("release" "Compiles release build.")))))))
