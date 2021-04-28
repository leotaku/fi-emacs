;;; sd.el --- Unit system for fi-emacs -*- lexical-binding: t -*-

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

;; This package provides an unit system partly inspired by systemd.
;;
;; Please consult the individual elisp docstrings for documentation.

;;; Code:

(defvar sd-startup-list '()
  "List used for unit description lookup.
Every entry is a unit object, see `sd-make-unit' for documentation.")

(defsubst sd-make-unit (name)
  "Simplified constructor for `sd-unit' objects.

SD-UNIT FORMAT:
  \(NAME state form dependencies . dependents)
WHERE:
  \(symbolp NAME)
  \(or (memq state '(available success)) (consp state))
  \(or (null f) (functionp f))
  \(and (listp dependencies) (all (mapcar #'symbolp dependencies)))
  \(and (listp dependents) (all (mapcar #'symbolp dependents)))"
  (nconc (list name 'available #'ignore nil) nil))

(defsubst sd-access-unit (name)
  "Access a local representation of the unit named NAME."
  (assq name sd-startup-list))

(defsubst sd-update-unit (unit)
  "Ensure that all changes to local UNIT are registered."
  (setf (alist-get (car unit) sd-startup-list) (cdr unit)))

(defsubst sd-unit-name (unit)
  "Access slot UNIT of UNIT object.
This function acts as a generalized variable."
  (car unit))
(defsubst sd-unit-state (unit)
  "Access slot STATE of UNIT object.
This function acts as a generalized variable."
  (cadr unit))
(defsubst sd-unit-function (unit)
  "Access slot FUNCTION of UNIT object.
This function acts as a generalized variable."
  (caddr unit))
(defsubst sd-unit-dependencies (unit)
  "Access slot DEPENDENCIES of UNIT object.
This function acts as a generalized variable."
  (cadddr unit))
(defsubst sd-unit-dependents (unit)
  "Access slot DEPENDENTS of UNIT object.
This function acts as a generalized variable."
  (cddddr unit))

(gv-define-setter sd-unit-name (value item)
  `(setf (car ,item) ,value))
(gv-define-setter sd-unit-state (value item)
  `(setf (cadr ,item) ,value))
(gv-define-setter sd-unit-function (value item)
  `(setf (caddr ,item) ,value))
(gv-define-setter sd-unit-dependencies (value item)
  `(setf (cadddr ,item) ,value))
(gv-define-setter sd-unit-dependents (value item)
  `(setf (cddddr ,item) ,value))

(defsubst sd--run-unit (unit)
  (condition-case err
      (prog1 (quote success)
        (funcall (sd-unit-function unit)))
    ((debug error) (cons 'eval err))))

(defsubst sd--extract-state (unit)
  (let* ((state (sd-unit-state unit))
         (failed nil))
    (if (not (eq state 'available))
        state
      (dolist (dependency (sd--all-dependencies unit))
        (setq state (sd-unit-state (sd-access-unit dependency)))
        (unless (eq state 'success)
          (push dependency failed)))
      (when failed
        (cons 'dependencies failed)))))

(defsubst sd--reach-only-unit (unit)
  (setf (sd-unit-state unit)
        (or (sd--extract-state unit)
            (sd--run-unit unit)))
  (sd-update-unit unit)
  (sd-unit-state unit))

(defun sd--all-dependencies (unit)
  (let ((name (sd-unit-name unit))
        (reverse nil))
    (dolist (unit sd-startup-list)
      (when (memq name (sd-unit-dependents unit))
        (push (sd-unit-name unit) reverse)))
    (append reverse (sd-unit-dependencies unit))))

(defun sd--dependency-relations (unit-alist)
  (let ((result nil))
    (dolist (unit unit-alist)
      (setf (alist-get (sd-unit-name unit) result)
            (sd-unit-dependencies unit)))
    (dolist (unit unit-alist result)
      (dolist (dependent (sd-unit-dependents unit))
        (setf (alist-get dependent result)
              (cons (sd-unit-name unit)
                    (alist-get dependent result)))))))

(defun sd--build-unit-sequence (name reached-list dep-alist)
  (if (memq name reached-list)
      reached-list
    (let* ((deps (assq name dep-alist))
           (func (lambda (it)
                   (sd--build-unit-sequence it (cons name reached-list) dep-alist)))
           (full (apply #'append (mapcar func deps))))
      (cons name (delq name full)))))

(defun sd--generate-sequence (name)
  (let* ((dep-alist (sd--dependency-relations sd-startup-list))
         (uncleaned (sd--build-unit-sequence name nil dep-alist)))
    (delete-dups (nreverse uncleaned))))

;;; Interface

(defun sd-register-unit (name &optional f dependencies dependents)
  "Define a UNIT named NAME with execution function F, depending on
the units DEPENDENCIES, wanted by the units DEPENDENTS.

This function will error if another unit with the same name has
been defined, unless OVERRIDEP is set to a non-nil value."
  (unless (and (symbolp name)
               (or (null f) (functionp f))
               (listp dependencies)
               (listp dependents))
    (error "Wrong type argument to register-unit"))
  (let ((unit (sd-make-unit name)))
    ;; set unit fields
    (setf (sd-unit-state unit) 'available)
    (setf (sd-unit-function unit) (or f #'ignore))
    (setf (sd-unit-dependencies unit) dependencies)
    (setf (sd-unit-dependents unit) dependents)
    ;; register unit
    (sd-update-unit unit)))

(defun sd-format-error (name &optional parents)
  "Format the error for unit with NAME in an user-readable manner.
Optional argument PARENTS is used internally to keep track of
units in a dependency chain."
  (let* ((unit (sd-access-unit name))
         (state (sd-unit-state unit))
         (reason (or (car-safe state) state))
         (context (cdr-safe state))
         (prefix (length parents)))
    (cond
     ((eq reason 'success)
      nil)
     ((or (null unit))
      (format "%s:`%s' failed because it does not exist." prefix name))
     ((eq reason 'available)
      (format "%s:`%s' failed because it has not been reached yet." prefix name))
     ((eq reason 'eval)
      (format "%s:`%s' failed because an error occurred: %s" prefix name context))
     ((memq name parents)
      (format "%s:`%s' failed because it has a recursive dependency on itself." prefix name))
     ((eq reason 'dependencies)
      (concat (format "%s:`%s' failed because:\n" prefix name)
              (mapconcat
               (lambda (it) (sd-format-error it (cons name parents)))
               context "\n")))
     (t
      (format "%s:`%s' failed because of improper setup: %s" prefix name unit)))))

(defun sd-reach-target (name)
  "Manually reach the unit named NAME.
Returns an error when the unit has errored, nil if it has succeeded."
  (let ((sequence (sd--generate-sequence name))
        (unit nil))
    (dolist (name sequence)
      (setq unit (sd-access-unit name))
      (when unit
        (sd--reach-only-unit unit)))
    (sd-unit-state (sd-access-unit name))))

(provide 'sd)

;;; sd.el ends here
