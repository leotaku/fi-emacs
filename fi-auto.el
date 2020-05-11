;;; fi-auto.el --- Small improvements to autoloading -*- lexical-binding: t; -*-

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 20 June 2019
;; Homepage: https://github.com/leotaku/fi-emacs
;; Keywords: fi-emacs, configuration, lisp
;; Package-Version: 0.1.0
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

;;; Commentary:

(require 'fi-subr)

;;; Code:

(defun fi-auto-at (place file)
  "Autoload a feature FILE from an external function PLACE."
  (advice-add place :before
              `(lambda () (require ,file))))

(defun fi-auto-keymap (key bind file &optional keymap)
  "Autoload feature FILE from a key that then executes the keymap BIND."
  (let* ((keymap (or keymap 'global-map)))
    (eval `(define-key ,keymap ,key
             (lambda () (interactive)
               (require ',file)
               (define-key ,keymap ,key ,bind)
               (fi-simulate-key ,key ,keymap))))))

(provide 'fi-auto)

;;; fi-auto.el ends here
