;;; bk-block.el --- Block-style init management based on sd.el -*- lexical-binding: nil -*-

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 19 July 2019
;; Homepage: https://github.com/leotaku/fi-emacs
;; Keywords: fi-emacs, configuration, lisp
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

;; CANCELED: maybe change pre to mean loading after deps but before self

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
  "An alist mapping every symbol to a bk-generation expression."
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
  "Specify how block evaluation should be handled after initialization has concluded.

Valid values are: warn, error, allow and fail-silent"
  :group 'bk-block
  :type '(radio
          (symbol warn)
          (symbol error)
          (symbol allow)
          (symbol fail-silent)))

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
           'bk--gen-expansion-case
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

(defun bk-gen-compiled (list)
  (byte-compile (bk--gen-expansions list)))

(prog1 "Compile expansion"
  (setq bk--expansion (bk-gen-compiled bk-expansion-alist)))

(defun bk--warn (format-string &rest args)
  (display-warning
   'bk-block
   (apply 'format format-string args)
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
          (set place (nconc (symbol-value place) value)))
         ((eq setter '=)
          (set place value))
         (t
          (error "bk-expansion-alist: Unknown setter `%s'" applicator)))))
    (bk--gen-block name pre pst req wnt)))

(defun bk--gen-block (name pre pst req wnt)
  `(if sd--in-unit-setup-phase
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
           ',wnt)))
     ,(cond
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
        (error "Invalid value for `bk-post-init-style': `%s'" bk-post-init-style)))))

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

(defun bk-reach-target (unit-name)
  "Try reaching the target UNIT-NAME.

Displays warnings for all errors that have ocurred."
  (let ((return (sd-reach-target unit-name)))
    (cond
     ((eq return t)
      (message "Target `%s' succeded." unit-name))
     ((eq return nil)
      nil)
     (t
      (bk--warn "Target `%s' failed because:\n%s" unit-name return)))))

(defalias 'bk-register-unit 'sd-register-unit
  "Alias for `sd-register-unit' so no functions from the sd
package need to be used directly.")

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

(provide 'bk-block)

;;; bk-block.el ends here
