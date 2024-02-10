;;; projection-core.el --- Core library for `projection' -*- lexical-binding: t; -*-

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

;; Core functions for usage of `projection' including project registration, value
;; caching and helper functions needed across multiple other `projection' Lisp
;; files.

;;; Code:

(require 'projection-core-cache)
(require 'projection-core-completion)
(require 'projection-core-log)
(require 'projection-core-match)
(require 'projection-core-misc)
(require 'projection-core-type)
;; (require 'projection-core-commands)

(provide 'projection-core)
;;; projection-core.el ends here
