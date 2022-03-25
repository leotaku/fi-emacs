;;; fi.el --- Configuration helpers for fi-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021 Leo Gaskin

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

;; This package provides a number of useful configuration utilities
;; that are missing from the Emacs standard library.  It only depends
;; on facilities that are already loaded per default.
;;
;; Please consult the individual elisp docstrings for documentation.

;;; Code:

(defmacro fi-csetq (sym value)
  "Set the default VALUE of SYM, respecting its custom-set property."
  `(fi-cset ',sym ,value))

(defun fi-cset (symbol value)
  "Set the default VALUE of SYMBOL, respecting its custom-set property."
  (funcall (or (get symbol 'custom-set)
               'set-default)
           symbol value))

(defmacro fi-with-gui (&rest body)
  "Evaluate BODY whenever the Emacs GUI is ready.

If the GUI is already running or has previously been started,
execute BODY immediately.

Note that this function does not consider terminal frames a GUI."
  `(if (display-graphic-p)
       (progn ,@body)
     (add-function :after after-focus-change-function #'fi--run-at-gui)
     (add-hook 'fi--run-at-gui-body (lambda () ,@body))))

(defvar fi--run-at-gui-body nil)
(defun fi--run-at-gui ()
  (when (and (display-graphic-p))
    (unwind-protect (run-hooks 'fi--run-at-gui-body)
      (remove-function after-focus-change-function #'fi--run-at-gui))))

(defun fi-call-silent (fun &rest args)
  "Call FUN with ARGS, wrapped in a `inhibit-message` expression.
Intended mainly for advising existing functions."
  (let ((inhibit-message t))
    (apply fun args)))

(defmacro fi-keys (&rest definitions)
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
  `(fi-keys-with-default (current-global-map) ,@definitions))

(defmacro fi-keys* (&rest definitions)
  "Define the given KEY/DEFINITION pairs in a global override map.

This macro accepts all of the same arguments as `fi-keys',
although overriding KEYMAP or PACKAGE is not recommended as it
does not make sense when defining keys on the override map.

\(fn &optional KEYMAP &key PACKAGE &rest [KEY DEFINITION]...)"
  `(fi-keys-with-default fi-key-override-global-map ,@definitions))

(defmacro fi-keys-with-default (default-keymap &rest definitions)
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
              (t (push `(fi-keys-with-default ,default-keymap ,@value) recursive)))))
    `(prog1 nil
       ,@(when package `((require ',package)))
       ,@(mapcar (lambda (pair)
                   `(define-key ,(or keymap default-keymap)
                                ,(car pair) #',(cdr pair)))
                 keys)
       ,@recursive)))

(defvar fi-key-override-global-map (make-keymap)
  "The fi-override-global-mode keymap.")

(define-minor-mode fi-key-override-global-mode
  "A minor mode so that keymap settings override other modes."
  :init-value t
  :lighter "")

(add-to-list
 'emulation-mode-map-alists
 `((fi-key-override-global-mode . ,fi-key-override-global-map)))

(provide 'fi)

;;; fi.el ends here
