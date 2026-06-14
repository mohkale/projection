;; -*- mode: eask; lexical-binding: t -*-

(package "projection"
         "0.1"
         "Project type support for `project'")

(website-url "https://github.com/mohkale/projection")
(keywords "project" "convenience")

(files "src/*.el"
       "src/projection-dape/*.el"
       "src/projection-multi/*.el"
       "src/projection-multi-embark/*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "29.1")
(depends-on "project" "0.9.8")
(depends-on "compat" "29.1.4.1")
(depends-on "s" "1.13")
(depends-on "f" "0.20")

;; projection-multi
(depends-on "compile-multi" "0.5")

;; projection-multi-embark
(depends-on "compile-multi-embark" "0.5")

;; projection-dape
(depends-on "dape" "0.8")

(development
  (depends-on "buttercup"))
