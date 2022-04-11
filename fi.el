;;; fi.el --- Configuration helpers for fi-emacs -*- lexical-binding: t; -*-

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

(defun fi-advice-silent (fun &rest args)
  "Call FUN with ARGS, wrapped in a `inhibit-message` expression.
Intended mainly for advising existing functions."
  (let ((inhibit-message t))
    (apply fun args)))

(provide 'fi)

;;; fi.el ends here
