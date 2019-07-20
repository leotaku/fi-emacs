;; sd.el --- unit-script system for fastiter -*- lexical-binding: t -*-

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 19 July 2019
;; Homepage: https://github.com/leotaku/fi-emacs
;; Keywords: fi-emacs, sd, startup
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

(require 'gv)

;;; Code:

(defconst sd-unit-list '()
  "List used for unit description lookup.
Every entry is a unit object, see `sd-make-unit' for documentation.")

(defconst sd-in-unit-setup-phase t
  "If true, new units may be defined.")

(defsubst sd-make-unit (name)
  "Simplified constructor for `sd-unit' object.

A `sd-unit' object is a cons list with the following structure:
\(NAME state form . dependencies)
where:
\(symbolp NAME)
OUTDATED: \(or (boolp state) (listp state))
\(listp form)
\(and (listp dependencies) (all (mapcar 'symbolp dependencies)))"
  (cons name (cons t (cons nil nil))))

(defsubst sd-unit-name (unit)
  "Access slot \"unit\" of `sd-unit' object.
This function acts as a generalized variable."
  (car unit))
(defsubst sd-unit-state (unit)
  "Access slot \"state\" of `sd-unit' object.
This function acts as a generalized variable."
  (cadr unit))
(defsubst sd-unit-form (unit)
  "Access slot \"form\" of `sd-unit' object.
This function acts as a generalized variable."
  (caddr unit))
(defsubst sd-unit-dependencies (unit)
  "Access slot \"dependencies\" of `sd-unit' object.
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
  (unless (and (symbolp name)
               (listp requires)
               (listp form)
               (listp wanted-by))
    (error "Wrong type argument to register-unit"))
  (unless sd-in-unit-setup-phase
    (error "Registering new units after a target has been reached is illegal"))
  (let ((unit (assq name sd-unit-list)))
    (when (null unit)
      (setq unit (sd-make-unit name)))
    ;; wanted-by is handled by the dep system
    (when (eq (sd-unit-state unit) t)
      (prog1 t
        (setf (sd-unit-dependencies unit)
              (nconc requires
                     (sd-unit-dependencies unit)))
        (setf (sd-unit-form unit) form)
        (setf (sd-unit-state unit) nil)
        (sd--destructive-set-unit unit)
        (dolist (wants-name wanted-by)
          (sd--add-unit-dependency wants-name name))))))

(defun sd-reach-unit (name)
  (setq sd-in-unit-setup-phase nil)
  (let* ((unit (assq name sd-unit-list))
         (state (if unit (sd-unit-state unit) t)))
    (cond
     ((null state)
      (let (errors rec-error)
        (dolist (dep (sd-unit-dependencies unit))
          (if (eq dep name)
              (push (list name 'recursive) errors)
            (let ((err (sd-reach-unit dep)))
              (when err
                (push err errors)))))
        ;; ad-hoc feature integration
        (require name nil t)
        (setf (sd-unit-state unit)
              (if errors
                  (cons name (cons 'dependencies errors))
                (let ((eval-error (condition-case err
                                      (prog1 nil
                                        (eval (sd-unit-form unit) nil))
                                    (error err))))
                  (if eval-error
                      (cons name (cons 'eval eval-error))
                    'done)))))
      (sd--destructive-set-unit unit)
      (if (eq 'done (sd-unit-state unit))
          nil
        (sd-unit-state unit)))
     ((eq state 'done)
      nil)
     ((listp state)
      state)
     (t
      (unless unit
        (setq unit (sd-make-unit name)))
      (if (require name nil t)
          (progn
            (setf (sd-unit-state unit) nil)
            (sd--destructive-set-unit unit)
            (sd-reach-unit name))
        (setf (sd-unit-state unit) (list name 'noexist))
        (sd--destructive-set-unit unit)
        (sd-unit-state unit))))))

(defsubst sd--add-unit-dependency (name dep-name)
  (let ((unit (assq name sd-unit-list)))
    (when (null unit)
      (setq unit (sd-make-unit name)))
    (setf (sd-unit-dependencies unit)
          (nconc (sd-unit-dependencies unit)
                 (list dep-name)))
    (sd--destructive-set-unit unit)))

(defsubst sd--destructive-set-unit (unit)
  (setq sd-unit-list (assq-delete-all (sd-unit-name unit) sd-unit-list))
  (push unit sd-unit-list))

(defun sd-message-error (err &optional prefix)
  (when err
    (let ((unit (car err))
          (reason (cadr err))
          (context (cddr err))
          (prefix (or prefix 0)))
      (cond
       ((eq reason 'eval)
        (message "%s:`%s' failed because an error occurred: %s" prefix unit context))
       ((eq reason 'noexist)
        (message "%s:`%s' failed because it does not exist." prefix unit))
       ((eq reason 'recursive)
        (message "%s:`%s' failed because it depends on itself." prefix unit))
       ((eq reason 'dependencies)
        (message "%s:`%s' failed because:" prefix unit)
        (mapc (lambda (err) (sd-message-error err (1+ prefix)))
              context))))))

(defun sd-reach-target (name)
  (let ((err (sd-reach-unit name)))
    (if (not err)
        (message "Target `%s' succeded." name)
      (message "Target `%s' failed because:" name)
      (sd-message-error err))))

(provide 'sd)

;;; sd.el ends here
