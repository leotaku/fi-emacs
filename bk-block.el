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

(require 'sd)
(require 'fi-config)

;;; Code:

(defconst bk-expansion-alist
  '((:straight  d + pre `(straight-use-package ',~))
    (:init      - + pre ~)
    (:load      * + pre `(load ,(expand-file-name ~ user-emacs-directory) nil t))
    (:load-at   * + pst `(load ,(expand-file-name ~ user-emacs-directory) nil t))
    (:config    - + pst ~)
    (:wanted-by * = wnt ~)
    (:requires  * + req ~)
    (:hook      * + pst `(add-hook ',(car ~) ',(cdr ~)))
    (:start     * + pst `(,~))
    (:custom    * + pre `(fi-csetq ,(car ~) ,(cdr ~)))
    (:bind      * + pre `(leaf-keys ,~))
    (:bind*     * + pre `(leaf-keys* ,~)))
  "An alist mapping every symbol to a bk-generation expression.")

(defun bk-gen-expansion-case (entry)
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
                (set '~ default))
              ,expr)
            ~)))))))

(defun bk-gen-expansions (list)
  `(lambda (default pair)
     (let ((key (car pair))
           (~ (cdr pair)))
       (cond
        ,@(mapcar
           'bk-gen-expansion-case
           list)
        (t
         (error "Unrecognized keyword `%s' in `%s'" key default))))))

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
  (byte-compile (bk-gen-expansions list)))

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
             (setter (nth 0 triple))
             (symbol (nth 1 triple))
             (value (nth 2 triple)))
        (set symbol
             (nconc
              (when (eq setter '+)
                (symbol-value symbol))
              value))))
    `(prog1 ',name
       ,(if sd--in-unit-setup-phase
            ;; case: actually setting up units
            `(condition-case-unless-debug err
                 (progn
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
          ;; case: manually run
          `(prog1
               (bk--warn
                "`%s' block run after startup without dependency checks." ',name)
             ,@pre
             ,@pst)))))

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

(defconst bk-font-lock-keywords
  '(("(\\(bk-block[^ ]*\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))

(font-lock-add-keywords 'emacs-lisp-mode bk-font-lock-keywords)

(provide 'bk-block)

;;; bk-block.el ends here
