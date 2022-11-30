;;; eu.el --- Keybinding utilities for fi-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@le0.gs>
;; Created: 19 July 2019
;; Homepage: https://github.com/leotaku/fi-emacs
;; Keywords: fi-emacs configuration lisp
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

;; This package provides a some simple utilities to organize large
;; amounts of keybindings with a pleasant macro syntax.  It only
;; depends on facilities that are already loaded per default.
;;
;; Please consult the individual elisp docstrings for documentation.

;;; Code:

(defmacro eu-keys (&rest definitions)
  "Define the given KEY/DEFINITION pairs in the current global map.

The target map can be overriden by providing the name of a keymap
as a keyword symbol, that is, the name of the keymap preceded by
the `:' colon character.

Options can be given as keywords before or after the
KEY/DEFINITION pairs.  The last occurrence of an option always
takes precedence, others are ignored.  Available keywords are:

:package    If non-nil, require the given package symbol before any keys are bound

KEY/DEFINITION pairs are as KEY and DEF in `define-key'.

In addition to KEY/DEFINITION pairs, an unlimited number of lists
consisting of valid arguments to this macro may also be given.
These are parsed recursively and executed after any bindings in
the parent argument list.

\(fn &optional KEYMAP &key PACKAGE &rest [KEY DEFINITION]...)"
  `(eu-keys-with-default (current-global-map) ,@definitions))

(defmacro eu-keys* (&rest definitions)
  "Define the given KEY/DEFINITION pairs in a global override map.

This macro accepts all of the same arguments as `eu-keys',
although overriding KEYMAP or PACKAGE is not recommended as it
does not make sense when defining keys on the override map.

\(fn &optional KEYMAP &key PACKAGE &rest [KEY DEFINITION]...)"
  `(eu-keys-with-default eu-key-override-global-map ,@definitions))

(defmacro eu-keys-with-default (default-keymap &rest definitions)
  "Parse and execute key bindings according to DEFINITIONS.

Keys are set in DEFAULT-KEYMAP, if no other keymap is given as
part of DEFINITIONS that would override it."
  (let (keymap package keys recursive)
    (when-let ((symbol (car definitions))
               (name (and (symbolp symbol) (symbol-name symbol))))
      (when (and (string-prefix-p ":" name)
                 (not (eq symbol :package)))
        (setq keymap (intern (string-trim-left name ":")))
        (pop definitions)))
    (while (and definitions)
      (let ((value (pop definitions)))
        (cond ((eq value :package)
               (setq package (pop definitions)))
              ((and (stringp (car-safe value))
                    (symbolp (cdr-safe value)))
               (push (cons (kbd (car value)) (cdr value)) keys))
              ((and (vectorp (car-safe value))
                    (symbolp (cdr-safe value)))
               (push (cons (car value) (cdr value)) keys))
              (t (push `(eu-keys-with-default ,default-keymap ,@value) recursive)))))
    `(prog1 nil
       ,@(when package `((require ',package)))
       ,@(mapcar (lambda (pair)
                   `(define-key ,(or keymap default-keymap)
                                ,(car pair) #',(cdr pair)))
                 keys)
       ,@recursive)))

(defvar eu-key-override-global-map (make-keymap)
  "The eu-override-global-mode keymap.")

(define-minor-mode eu-key-override-global-mode
  "A minor mode so that keymap settings override other modes."
  :init-value t
  :lighter "")

(add-to-list
 'emulation-mode-map-alists
 `((eu-key-override-global-mode . ,eu-key-override-global-map)))

(provide 'eu)

;;; eu.el ends here
