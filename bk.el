;;; bk.el --- Block-style init management based on sd.el -*- lexical-binding: t -*-

;; Copyright (C) 2019-2020 Leo Gaskin

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 19 July 2019
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

;; This package provides an use-package replacement based on `sd.el`.
;;
;; Please consult the individual elisp docstrings for documentation.

;; TODO: get file for blocks from `load-file-name'

(require 'sd)
(require 'fi-config)

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
    (:bind      * + pst `(leaf-keys ,~))
    (:bind*     * + pst `(leaf-keys* ,~))
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

(defcustom bk-post-init-style 'allow
  "How block evaluation should be handled after initialization.
Valid values are: warn, error, allow and fail-silent"
  :group 'bk-block
  :type '(radio
          (symbol warn)
          (symbol error)
          (symbol allow)
          (symbol fail-silent)))

(defvar bk--expansion nil
  "The compiled expansion function used for parsing keyword arguments.")

;;;; Implementation:

(defun bk--gen-expansion-case (entry)
  (let ((key (nth 0 entry))
        (applicator (nth 1 entry))
        (setter (nth 2 entry))
        (listvar (nth 3 entry))
        (expr (nth 4 entry)))
    `((eq key ',key)
      (list
       ',setter
       ',listvar
       ,(cond
         ((eq applicator '-)
          expr)
         ((eq applicator '*)
          `(mapcar
            (lambda (~)
              ,expr)
            ~))
         ((eq applicator 'd)
          `(mapcar
            (lambda (~)
              (when (eq t ~)
                (set '~ name))
              ,expr)
            ~))
         (t
          (error "bk-expansion-alist: Unknown applicator `%s'" applicator)))))))

(defun bk--gen-expansions (list)
  `(lambda (name pair)
     (let ((key (car pair))
           (~ (cdr pair)))
       (cond
        ,@(mapcar
           #'bk--gen-expansion-case
           list)
        (t
         (error "Unrecognized keyword `%s' in `%s'" key name))))))

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

(defsubst bk--get-name-feature (name)
  (let ((string (symbol-name name)))
    (if (eq t (compare-strings
               string 0 1
               "." 0 1))
	    (intern (substring string 1)))))

(defun bk--gen-special-requirements (names)
  (mapcar
   (lambda (name)
     (let ((feature (bk--get-name-feature name)))
       (when feature
         `(sd-register-unit ',name '(require ',feature) nil nil t))))
   names))

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
  (let ((alist (bk--construct-alist args))
        pre pst req wnt)
    (dolist (entry alist)
      (let* ((triple (funcall bk--expansion name entry))
             (place (nth 1 triple))
             (setter (nth 0 triple))
             (value (nth 2 triple)))
        (cond
         ((eq setter '+)
          (cond
	       ((eq place 'pre)
	        (setq pre (nconc pre value)))
	       ((eq place 'pst)
	        (setq pst (nconc pst value)))
	       ((eq place 'req)
	        (setq req (nconc req value)))
	       ((eq place 'wnt)
	        (setq wnt (nconc wnt value)))))
         ((eq setter '=)
          (cond
	       ((eq place 'pre)
	        (setq pre value))
	       ((eq place 'pst)
	        (setq pst value))
	       ((eq place 'req)
	        (setq req value))
	       ((eq place 'wnt)
	        (setq wnt value))))
         (t
          (error "bk-expansion-alist: Unknown setter `%s'" applicator)))))
    (bk--gen-block name pre pst req wnt)))

(defun bk--gen-block (name pre pst req wnt)
  (cond
   (sd--in-unit-setup-phase
    `(progn
       ,@(bk--gen-special-requirements req)
       (condition-case-unless-debug err
           (prog1 ',name
             ,@pre
             (sd-register-unit
              ',name
              '(progn ,@pst)
              ',req
              ',wnt))
         (error
          (bk--warn "Error in block `%s' during setup: %s" ',name err)
          (sd-register-unit
           ',name
           '(error "This unit could not be set up properly!")
           ',req
           ',wnt)))))
   ((eq bk-post-init-style 'warn)
    `(prog1 ',name
       (bk--warn "Running block `%s' without dependency checks!" ',name)
       ,@pre
       ,@pst))
   ((eq bk-post-init-style 'error)
    `(error "Not running block `%s' without after initialization!" ',name))
   ((eq bk-post-init-style 'allow)
    `(prog1 ',name
       ,@pre
       ,@pst))
   ((eq bk-post-init-style 'fail-silent)
    nil)
   (t
    (error "Invalid value for `bk-post-init-style': `%s'" bk-post-init-style))))

;;;; Interface:

(defmacro bk-block (name &rest args)
  (declare (indent 1))
  `(bk-block0 ,name
     :wanted-by gui-target
     ,@args))

(defmacro bk-block! (name &rest args)
  (declare (indent 1))
  `(bk-block0 ,name
     :wanted-by init-target
     ,@args))

(defmacro bk-block* (name &rest args)
  (declare (indent 1))
  `(bk-block ,name
     :requires ,(intern (concat "." (symbol-name name)))
     ,@args))

(defmacro bk-block!* (name &rest args)
  (declare (indent 1))
  `(bk-block! ,name
     :requires ,(intern (concat "." (symbol-name name)))
     ,@args))

(defun bk-reach-target (name)
  "Try reaching the target NAME.

Displays warnings for all errors that have ocurred."
  (let ((state (sd-reach-target name)))
    (if (eq 'success state)
        (message "Target `%s' succeded." name)
      (bk--warn "Target `%s' failed because:\n%s"
                name
                (sd-format-error name)))))

(defun bk-poll-target (name &optional after)
  "Try reaching the target NAME asynchronously.
Call AFTER, after this has finished.

Displays warnings for all errors that have ocurred."
  (sd-poll-target
   name 0.05 nil
   (lambda (state)
     (if (eq 'success state)
         (message "Target `%s' succeded." name)
       (bk--warn "Target `%s' failed because:\n%s"
                 name
                 (sd-format-error name)))
     (when after (funcall after)))))

(defun bk-register-target (name &optional dependencies)
  "Register an empty unit without dependencies or code.
These can be used to group together units using `:wanted-by'."
  (sd-register-unit name nil dependencies nil))

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
