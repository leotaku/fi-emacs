;;; sd.el --- Unit system for fi-emacs -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
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

;; TODO: Simplify sequence generation code
;; TODO: Document complex subroutines

;;; Code:

(defconst sd-startup-list '()
  "List used for unit description lookup.
Every entry is a unit object, see `sd-make-unit' for documentation.")

(defconst sd--in-unit-setup-phase t
  "If true, new units may be defined.")

(defsubst sd-make-unit (name)
  "Simplified constructor for `sd-unit' objects.

SD-UNIT FORMAT:
  \(NAME state form . dependencies)
WHERE:
  \(symbolp NAME)
  \(or (memq state '(mention avail sucess)) (listp state))
  \(listp form)
  \(and (listp dependencies) (all (mapcar 'symbolp dependencies)))"
  (cons name (cons 'mention (cons nil nil))))

(defsubst sd-unit-name (unit)
  "Access slot UNIT of UNIT object.
This function acts as a generalized variable."
  (car unit))
(defsubst sd-unit-state (unit)
  "Access slot STATE of UNIT object.
This function acts as a generalized variable."
  (cadr unit))
(defsubst sd-unit-form (unit)
  "Access slot FORM of UNIT object.
This function acts as a generalized variable."
  (caddr unit))
(defsubst sd-unit-dependencies (unit)
  "Access slot DEPENDENCIES of UNIT object.
This function acts as a generalized variable."
  (cdddr unit))

(gv-define-setter sd-unit-name (value item)
  `(setf (car ,item) ,value))
(gv-define-setter sd-unit-state (value item)
  `(setf (cadr ,item) ,value))
(gv-define-setter sd-unit-form (value item)
  `(setf (caddr ,item) ,value))
(gv-define-setter sd-unit-dependencies (value item)
  `(setf (cdddr ,item) ,value))

(defun sd-register-unit (name &optional form requires wanted-by overridep)
  "Define a UNIT named NAME with execution form FORM, requiring
the units REQUIRES, wanted by the units WANTED-BY.

This function will error if other units with the same name have
been defined, unless OVERRIDEP is set to a non-nil value or any
units have already been started when it is run."
  (unless (and (symbolp name)
               (listp requires)
               (listp form)
               (listp wanted-by))
    (error "Wrong type argument to register-unit"))
  (unless sd--in-unit-setup-phase
    (error "Registering new units after a target has been reached is illegal"))
  (let ((unit (assq name sd-startup-list)))
    ;; construct new unit
    (if (or overridep (null unit))
        (setq unit (sd-make-unit name))
      (unless (eq (sd-unit-state unit) 'mention)
        (error "An unit with the same name has already been registered")))
    ;; set unit fields
    (setf (sd-unit-state unit) 'avail)
    (setf (sd-unit-dependencies unit)
          (nconc requires
                 (sd-unit-dependencies unit)))
    (setf (sd-unit-form unit) form)
    (sd--destructive-set-unit unit)
    ;; handle wanted-by
    (dolist (wants-name wanted-by)
      (sd--add-unit-dependencies wants-name (list name)))))

(defsubst sd--add-unit-dependencies (name dependencies)
  (let ((unit (assq name sd-startup-list)))
    (when (null unit)
      (setq unit (sd-make-unit name)))
    (setf (sd-unit-dependencies unit)
          (nconc (sd-unit-dependencies unit)
                 dependencies))
    (sd--destructive-set-unit unit)))

(defsubst sd--destructive-set-unit (unit)
  (setf (alist-get (sd-unit-name unit) sd-startup-list) (cdr unit)))

(defun sd--reach-only-unit (name)
  "Try reaching the unit named NAME.
Fails if any dependencies have failed or not have been reached yet."
  (let* ((unit (assq name sd-startup-list))
         (state (sd-unit-state unit)))
    (cond
     ;; available
     ((eq state 'avail)
      (sd--reach-known-unit unit))
     ;; mentioned, success, errored
     (unit state)
     ;; nonexist
     (t nil))))

(defsubst sd--reach-known-unit (unit)
  (let* ((name (sd-unit-name unit))
         (form (sd-unit-form unit))
         (deps (sd-unit-dependencies unit))
         (report (sd--get-failure-report deps)))
    (setf (sd-unit-state unit)
          (cond
           ;; dependecies failed
           ((and report (listp report))
	        (cons 'dependencies report))
           ;; dependecies are recursive
           (report (cons 'recursive nil))
           ;; all dependencies succeeded
           (t (sd--execute-unit unit))))
    (sd--destructive-set-unit unit)
    (sd-unit-state unit)))

(defsubst sd--execute-unit (unit)
  (let ((eval-error (condition-case-unless-debug err
                        (prog1 nil
                          (eval (sd-unit-form unit) nil))
                      (error err))))
    (if eval-error
        ;; eval error
        (cons 'eval eval-error)
      ;; eval success
      'success)))

(defsubst sd--get-failure-report (names)
  (let (report failed-deps state)
    (dolist (name names)
      (setq state (sd-unit-state (assq name sd-startup-list)))
      (cond ((eq state 'success))
            ((eq state 'avail)
             (setq report 'recursive))
            (t
             (push name failed-deps))))
    (or report failed-deps)))

(defun sd--generate-unit-sequence (name list)
  (if (memq name list)
      list
    (let* ((unit (assq name sd-startup-list))
           (deps (and unit (sd-unit-dependencies unit)))
           (fun (lambda (it) (sd--generate-unit-sequence it (cons name list))))
           (full (apply 'append (mapcar fun deps))))
      (cons name (delq name full)))))

(defun sd--setup-unit-polling (name callback stop-callback)
  (let* ((sequence (nreverse (sd--generate-unit-sequence name nil))))
    (lambda ()
      (let ((head (car sequence))
            (tail (cdr sequence)))
        (cond
         ((not head))
         ((not tail)
          (funcall
           stop-callback
           (funcall callback head)))
         (t
          (funcall callback head)))
        (setq sequence tail)))))

;;; Interface

(defun sd-format-error (name &optional prefix)
  "Format the error for unit with NAME in an user-readable manner.
Optional argument PREFIX should be used to describe the recursion
level at which this error has occurred."
  (let* ((unit (assq name sd-startup-list))
         (state (sd-unit-state unit))
         (reason (or (car-safe state) state))
         (context (cdr-safe state))
         (prefix (or prefix 0)))
    (cond
     ((eq reason 'success)
      nil)
     ((or (null unit) (eq reason 'mention))
      (format "%s:`%s' failed because it does not exist." prefix name))
     ((eq reason 'avail)
      (format "%s:`%s' failed because it has not been reached yet." prefix name))
     ((eq reason 'eval)
      (format "%s:`%s' failed because an error occurred: %s" prefix name context))
     ((eq reason 'recursive)
      (format "%s:`%s' failed because it has at least one recursive dependency." prefix name))
     ((eq reason 'dependencies)
      (concat (format "%s:`%s' failed because:\n" prefix name)
              (mapconcat
               (lambda (name)
                 (sd-format-error
                  name
                  (1+ prefix)))
               context "\n")))
     (t
      (format "%s:`%s' failed because of improper setup: %s" prefix name unit)))))

(defun sd-poll-target (name delay &optional notify callback)
  "Manually reach the unit named NAME, polling every DELAY seconds.
Calls CALLBACK with the state of the finished unit."
  (setq sd--in-unit-setup-phase nil)
  (let* ((timer (timer-create))
         (finish (lambda (state)
                   (cancel-timer timer)
                   (funcall callback state)))
         (update (lambda (name)
                   (let ((time (current-time)))
                     (prog1 (sd--reach-only-unit name)
                       (when notify
                         (message
                          "Reaching unit: %s in %.06f"
                          name (float-time (time-since time))))))))
         (poll (sd--setup-unit-polling name update finish)))
    (timer-set-idle-time timer delay delay)
    (timer-set-function
     timer poll)
    (timer-activate timer)))

(defun sd-reach-target (name)
  "Manually reach the unit named NAME.
Returns an error when the unit has errored, nil if it has succeeded."
  (setq sd--in-unit-setup-phase nil)
  (let ((sequence (nreverse (sd--generate-unit-sequence name nil)))
	    (state))
    (dolist (name sequence)
      (setq state (sd--reach-only-unit name)))
    state))

(provide 'sd)

;;; sd.el ends here
