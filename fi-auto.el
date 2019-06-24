;;; fi-auto --- improved emacs autoloading -*- lexical-binding: t; -*-

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 20 June 2019
;; Homepage: https://github.com/leotaku/fi-emacs
;; Keywords: fi, fi-emacs, autoloading
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

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

;;; Commentary

;;; Code

(require 'fi-subr)

(defun fi-auto-at (place file)
  "Autoload a feature `file' from an external function `place'."
  (advice-add place :before
              `(lambda () (require ,file))))

(defmacro fi-auto-bind (key bind file &optional keymap)
  "Autoload feature `file' from a key that executes `bind'.

All arguments but `bind' are evaluated and thus should be quoted.
`bind' is not evaluated, as it may contain keymaps that have yet to be defined."
  (let* ((keymap (or (eval keymap) 'global-map))
         (key (eval key))
         (file (eval file)))
    `(define-key ,keymap ,key
       (lambda () (interactive)
         (require ',file)
         (define-key ,keymap ,key ,bind)
         (fi-simulate-key ,key ,keymap)))))

(provide 'fi-auto)
