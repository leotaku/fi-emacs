;;; bk.el --- Block-style init management based on sd.el -*- lexical-binding: t -*-

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

;; This package provides an use-package replacement based on `sd.el`.
;;
;; Please consult the individual elisp docstrings for documentation.

;; TODO: get file for blocks from `load-file-name'

(require 'sd)
(require 'fi)
(require 'eu)

;;; Code:

;;;; Variables:

(defgroup bk-block nil
  "Block-style init management based on sd.el"
  :group 'lisp
  :prefix "bk-")

(defcustom bk-expansion-alist
  '((:at-load   - + pre ~)
    (:load      * + pre `(load ,(expand-file-name ~ user-emacs-directory) nil t))
    (:config    - + pst ~)
    (:wanted-by * = wnt ~)
    (:requires  * + req ~)
    (:hook      * + pst `(add-hook ',(car ~) ',(cdr ~)))
    (:start     * + pst `(,~))
    (:custom    * + pst `(fi-csetq ,(car ~) ,(cdr ~)))
    (:bind      * + pst `(eu-keys ,~))
    (:bind*     * + pst `(eu-keys* ,~))
    (:mode      * + pst  `(add-to-list
                           'auto-mode-alist
                           ',(cons
                              (or (car-safe ~) ~)
                              (or (cdr-safe ~) name)))))
  "An alist mapping keyword to a reader, place and quoted expression."
  :group 'bk-block
  :type '(alist :value-type
                (list
                 (radio (symbol *)
                        (symbol -)
                        (symbol d))
                 (radio (symbol +)
                        (symbol =))
                 (radio (symbol pre)
                        (symbol pst)
                        (symbol req)
                        (symbol wnt))
                 sexp)))

(defvar bk--expansion nil
  "The compiled expansion function used for parsing keyword arguments.")

;;;; Implementation:

(defun bk--gen-expansion-case (entry)
  (let* ((key (nth 0 entry))
         (applicator (nth 1 entry))
         (setter (nth 2 entry))
         (place (nth 3 entry))
         (expr (nth 4 entry))
         (transform
          (cond ((eq applicator '-) expr)
                ((eq applicator '*)
                 `(mapcar (lambda (~) ,expr) ~))
                ((eq applicator 'd)
                 `(mapcar (lambda (~) (when (eq t ~) (set '~ name)) ,expr) ~)))))
    (cond ((eq setter '+)
           `((eq key ,key) (setq ,place (nconc ,place ,transform))))
          ((eq setter '=)
           `((eq key ,key) (setq ,place ,transform))))))

(defun bk--gen-expansions (list)
  `(lambda (name alist)
     (let (pre pst req wnt)
       (dolist (entry alist)
         (let ((key (car entry))
               (~ (cdr entry)))
           (cond
            ,@(mapcar #'bk--gen-expansion-case list)
            (t (error "Unrecognized keyword `%s' in `%s'" key name)))))
       (list pre pst req wnt))))

(defun bk--construct-alist (args)
  (let (result current)
    (dolist (it args)
      (if (and (symbolp it) (string-prefix-p ":" (symbol-name it)))
          (progn
            (push (nreverse current) result)
            (setq current (list it)))
        (push it current)))
    (push (nreverse current) result)
    (cdr (nreverse result))))

(defun bk--gen-special-requirements (names)
  (let ((result nil))
    (dolist (name names result)
      (let ((string (symbol-name name))
            (feature nil))
        (when (eq t (compare-strings string 0 1 "." 0 1))
	      (setq feature (intern (substring string 1)))
          (push `(sd-register-unit ',name (lambda () (require ',feature)))
                result))))))

(defun bk-generate-expansions ()
  "Generate and compile the function used for parsing keyword arguments.
Reads the description from the special `bk-expansion-alist' variable."
  (setq
   bk--expansion
   (byte-compile (bk--gen-expansions bk-expansion-alist))))

(prog1 "Compile expansion"
  (bk-generate-expansions))

(defun bk--warn (format-string &rest args)
  (display-warning
   'bk-block
   (apply #'format format-string args)
   :warning))

(defmacro bk-block0 (name &rest args)
  (declare (indent 1))
  (let* ((alist (bk--construct-alist args))
         (result (funcall bk--expansion name alist))
         (pre (nth 0 result))
         (pst (nth 1 result))
         (req (nth 2 result))
         (wnt (nth 3 result)))
    `(condition-case-unless-debug error
         (prog1 ',name
           ,@pre
           ,@(bk--gen-special-requirements req)
           (sd-register-unit
            ',name
            (lambda () ,@pst)
            ',req
            ',wnt)
           (when (null load-file-name)
             (bk-reach-target ',name)))
       (error
        (bk--warn "Error in block `%s' during setup: %s" ',name error)))))

;;;; Interface:

(defmacro bk-block (name &rest args)
  (declare (indent 1))
  `(bk-block0 ,name
     :wanted-by default-target
     ,@args))

(defmacro bk-block* (name &rest args)
  (declare (indent 1))
  `(bk-block ,name
     :requires ,(intern (concat "." (symbol-name name)))
     ,@args))

(defun bk-reach-target (name)
  "Try reaching the target NAME.

Displays warnings for all errors that have ocurred."
  (let ((state (sd-reach-target name)))
    (if (eq 'success state)
        (message "Target `%s' succeeded." name)
      (message "Target `%s' failed." name)
      (dolist (line (split-string (sd-format-error name) "\n"))
        (bk--warn line)))))

(defun bk-register-target (name &optional dependencies)
  "Register an empty unit without dependencies or code.
These can be used to group together units using `:wanted-by'."
  (sd-register-unit name #'ignore dependencies nil))

;;;; Integrations:

(defconst bk-font-lock-keywords
  '(("(\\(bk-block[^ ]*\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(with-eval-after-load 'font-lock
  (font-lock-add-keywords 'emacs-lisp-mode bk-font-lock-keywords))

(with-eval-after-load 'lispy
  (dolist (block '(bk-block bk-block! bk-block!* bk-block* bk-block0))
    (push
     (cons block 1)
     (alist-get 'emacs-lisp-mode lispy-tag-arity))))

(provide 'bk)

;;; bk.el ends here
