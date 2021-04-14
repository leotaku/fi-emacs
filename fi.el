;;; fi.el --- Various utilities for fi-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@le0.gs>
;; Created: 19 July 2019
;; Homepage: https://github.com/leotaku/fi-emacs
;; Keywords: fi-emacs, configuration, extension, lisp
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This meta-package collectively loads the various facilities
;; provided by all fi-prefixed packages in the fi-emacs repository.

(require 'fi-config)
(require 'fi-helpers)

;;; Code:

(provide 'fi)

;;; fi.el ends here
