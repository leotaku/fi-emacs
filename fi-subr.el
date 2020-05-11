;;; fi-subr.el --- Miscellaneous subroutines for use in fi-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 21 June 2019
;; Homepage: https://github.com/leotaku/fi-emacs
;; Keywords: fi-emacs, configuration, lisp
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

;;; Code

(require 'seq)

(defun fi-simulate-key (key &optional keymap)
  "Send fake keypresses for KEY in KEYMAP.
KEY should be a key sequence in internal Emacs notation."
  (let ((overriding-local-map (or keymap global-map)))
    (setq unread-command-events
          (nconc
           (mapcar (lambda (ev) (cons t ev))
                   (listify-key-sequence key))
           unread-command-events))))

(defun fi-insert-at (list n item)
  "Return LIST with ITEM inserted at position `n'."
  (nconc (seq-take list n) (cons item (seq-drop list n))))

(defun fi-insert-after (list after item)
  "Return LIST with ITEM inserted right after AFTER."
  (let ((n (1+ (seq-position list after))))
    (if n
        (fi-insert-at list n item)
      list)))

(defun fi-insert-before (list before item)
  "Return LIST with ITEM inserted right before BEFORE."
  (let ((n (seq-position list before)))
    (if n
        (fi-insert-at list n item)
      list)))

(defun fi-call-silent (fun &rest args)
  "Call FUN with ARGS, wrapped in a `inhibit-message` expression.
Intended mainly for advising existing functions."
  (let ((inhibit-message t))
    (apply fun args)))

(provide 'fi-subr)

;;; fi-subr.el ends here
