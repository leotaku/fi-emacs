;;; bk-block.el --- block-based init management based on sd.el

;; Author: Leo Gaskin <leo.gaskin@brg-feldkirchen.at>
;; Created: 19 July 2019
;; Homepage: https://github.com/leotaku/fi-emacs
;; Keywords: fi-emacs, bk, bk-block
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
    (:load      * + pre `(load (expand-file-name ,~ user-emacs-directory)))
    (:load-at   * + pst `(load (expand-file-name ,~ user-emacs-directory)))
    (:config    - + pst ~)
    (:wanted-by * = wnt ~)
    (:requires  * = req ~)
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
        ,@(seq-map
           'bk-gen-expansion-case
           list)
        (t
         (warn "Unrecognized keyword `%s' in `%s'" key default))))))

(defun bk-gen-compiled (list)
  (byte-compile (bk-gen-expansions list)))

(prog1 "Compile"
  (setq bk--expansion (bk-gen-compiled bk-expansion-alist)))

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
       ,@pre
       ,(if sd-in-unit-setup-phase
            `(sd-register-unit
              ',name
              '(progn ,@pst)
              ',req
              ',wnt)
          `(prog1
               (warn
                "`%s' block run after startup without dependency checks." ',name)
             (require ',name nil t)
             ,@pst)))))

(defmacro bk-block (name &rest args)
  (declare (indent 1))
  `(bk-block0 ,name
     :wanted-by gui-target
     ,@args))

(defmacro bk-block* (name &rest args)
  (declare (indent 1))
  `(bk-block0 ,name
     :wanted-by init-target
     ,@args))

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

(defconst bk-font-lock-keywords
  '(("(\\(bk-block.?\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
     (1 font-lock-keyword-face)
     (2 font-lock-constant-face nil t))))
(font-lock-add-keywords 'emacs-lisp-mode bk-font-lock-keywords)

(provide 'bk-block)

;;; bk-block.el ends here
