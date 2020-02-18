;;; sd.el --- Unit-script system for fastiter -*- lexical-binding: t -*-

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 19 July 2019
;; Homepage: https://github.com/leotaku/fi-emacs
;; Keywords: fi-emacs, configuration, extension, lisp
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
;; 

;; TODO: Make API better
;; TODO: proper format for success/failure
;; TODO: non-numerical returns from `sd--reach-unit'

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
  \(or (memq state '(-1 0 1 2)) (listp state))
  \(listp form)
  \(and (listp dependencies) (all (mapcar 'symbolp dependencies)))"
  (cons name (cons -1 (cons nil nil))))

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

(defun sd-register-unit (name &optional form requires wanted-by)
  "Define a UNIT named NAME with execution form FORM, requiring
the units REQUIRES, wanted by the units WANTED-BY.

This function will error if other units with the same name have
been defined or any units have already been started when it is
run."
  (unless (and (symbolp name)
               (listp requires)
               (listp form)
               (listp wanted-by))
    (error "Wrong type argument to register-unit"))
  (unless sd--in-unit-setup-phase
    (error "Registering new units after a target has been reached is illegal"))
  (let ((unit (assq name sd-startup-list)))
    ;; construct new unit
    (if (null unit)
        (setq unit (sd-make-unit name))
      (unless (eq (sd-unit-state unit) -1)
        (error "An unit with the same name has already been registered")))
    ;; set unit fields
    (setf (sd-unit-state unit) 0)
    (setf (sd-unit-dependencies unit)
          (nconc requires
                 (sd-unit-dependencies unit)))
    (setf (sd-unit-form unit) form)
    (setf (sd-unit-state unit) 1)
    (sd--destructive-set-unit unit)
    ;; handle special dependencies
    (dolist (name requires)
      (let ((feature (sd--get-name-special name)))
        (when feature
          (let ((unit (sd-make-unit name)))
            (setf (sd-unit-form unit) `(require ',feature))
            (setf (sd-unit-state unit) 1)
            (sd--destructive-set-unit unit)))))
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
  (setf (alist-get (car unit) sd-startup-list) (cdr unit)))

(defsubst sd--get-name-special (name)
  (let ((string (symbol-name name)))
    (if (eq t (compare-strings
               string 0 1
               "." 0 1))
	    (intern (substring string 1))
      nil)))

(defun sd--reach-only-unit (name)
  "Try reaching the unit named NAME.
Fails if any dependencies have failed or not have been reached yet."
  (let* ((unit (assq name sd-startup-list))
         (old-state (sd-unit-state unit)))
    (cond
     ;; available
     ((eq old-state 1)
      (sd--reach-known-unit unit))
     ;; unavailable
     ((or (eq old-state -1) (eq old-state 0))
      (setf (sd-unit-state unit)
            (list name 'noexist))
      (sd--destructive-set-unit unit)
      (sd-unit-state unit))
     ;; done
     ((eq old-state 2)
      2)
     ;; nonexist
     ((null unit)
      (list name 'noexist))
     ;; errored
     ((listp old-state)
      old-state))))

(defsubst sd--reach-known-unit (unit)
  (let* ((name (sd-unit-name unit))
         (form (sd-unit-form unit))
         (failed-deps
          (seq-remove
           'null
           (seq-map
            'sd--get-unit-state
            (sd-unit-dependencies unit)))))
    (setf (sd-unit-state unit)
          (if (null failed-deps)
              ;; all dependencies succeeded 
              (let ((eval-error (condition-case-unless-debug err
                                    (prog1 nil
                                      (eval (sd-unit-form unit) nil))
                                  (error err))))
                (if eval-error
                    ;; eval error
                    (cons name (cons 'eval eval-error))
                  ;; eval success
                  2))
            ;; dependecies failed
            (cons name (cons 'dependencies failed-deps))))
    (sd--destructive-set-unit unit)
    (sd-unit-state unit)))

(defun sd--get-unit-state (name)
  (let* ((unit (assoc name sd-startup-list))
         (state (sd-unit-state unit)))
    (cond
     ((eq state 2)
      nil)
     ((null state)
      (list name 'noexist))
     (t
      state))))

(defun sd--generate-unit-sequence (name)
  (let* ((unit (assoc name sd-startup-list))
         (deps (and unit (sd-unit-dependencies unit))))
    (cons name (seq-mapcat 'sd--generate-unit-sequence deps))))

(defun sd--setup-unit-polling (target callback stop-callback)
  (let* ((sequence (nreverse (sd--generate-unit-sequence target))))
    (lambda ()
      (let ((head (car sequence))
            (tail (cdr sequence)))
        (cond
         ((not head))
         ((not tail)
          (funcall stop-callback (funcall callback head)))
         (t
          (funcall callback head)))
        (setq sequence tail)))))

(defun sd--format-error (state &optional prefix)
  "Format the error STATE returned by `sd--reach-unit' in an user-readable manner.
Optional argument PREFIX should be used to describe the recursion
level at which this error has occurred."
  (let ((unit (car state))
        (reason (cadr state))
        (context (cddr state))
        (prefix (or prefix 0)))
    (cond
     ((eq reason 'eval)
      (format "%s:`%s' failed because an error occurred: %s" prefix unit context))
     ((eq reason 'noexist)
      (format "%s:`%s' failed because it does not exist." prefix unit))
     ((eq reason 'recursive)
      (format "%s:`%s' failed because it depends on itself." prefix unit))
     ((eq reason 'dependencies)
      (concat (format "%s:`%s' failed because:\n" prefix unit)
              (mapconcat
               (lambda (state)
                 (sd--format-error state (1+ prefix)))
               context "\n"))))))

(defun sd-poll-target (target delay &optional notify callback)
  "Manually reach the unit named NAME, polling every DELAY seconds.
Calls CALLBACK with an user-readable error when the unit has errored, nil if it has succeeded."
  (setq sd--in-unit-setup-phase nil)
  (let* ((timer (timer-create))
         (finish (lambda (state)
                   (cancel-timer timer)
                   (funcall callback
                            (when (listp state)
                              (sd--format-error state)))))
         (update (lambda (name)
                   (let ((inhibit-message nil)
                         (time (current-time)))
                     (prog1 (sd--reach-only-unit name)
                       (when notify
                         (message
                          "Reaching unit: %s in %.06f"
                          name (float-time (time-since time))))))))
         (poll (sd--setup-unit-polling target update finish)))
    (timer-set-time timer delay delay)
    (timer-set-function
     timer poll)
    (timer-activate timer)))

(defun sd-reach-target (name)
  "Manually reach the unit named NAME.
Returns an user-readable error when the unit has errored, nil if it has succeeded."
  (setq sd--in-unit-setup-phase nil)
  (let ((sequence (nreverse (sd--generate-unit-sequence name)))
	    (state))
    (dolist (name sequence)
      (setq state (sd--reach-only-unit name)))
    (when (listp state)
      (sd--format-error state))))

(provide 'sd)

;;; sd.el ends here
