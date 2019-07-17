;;; bk-block-core.el --- flexible api for defining configuration blocks

;;; Commentary:

;; TODO: support multiple same keywords in block by appending
;; DONE: allow reading multiple vars in deftransition
;; DONE: remove deftransition argument and put it in order instead
;; TODO: allow escaping , comma in blocks

(require 'cl)
(require 'seq)
(require 'pcase)

;;; Code:

;;;; Variables

(defconst bk-default-order '()
  "List used by to determine the order of operations in a block.")

(defconst bk--operator-alist '()
  "Internal list used for symbol lookup.
Generally only altered by `bk-defop'.")

;;;; UI

(defmacro bk-block (name &rest args)
  "Expand ARGS and NAME bound to the arg `:name' according to
`bk-default-order' and execute the resulting code."
  (declare (indent 1))
  (let* ((alist (append `((:name ,name))
                        ;; (bk--construct-alist args)
                        ))
         (order (bk--normalize-order bk-default-order)
                ;; '((set :name :package nil) (produce (:straight :package) straight) (produce (:after :package) after) (produce :custom custom) (produce :hook hook) (produce :bind bind) (produce :bind* bind*) (produce :init code) (rproduce (:defer :package) defer ((produce (:require :package) require) (produce :load load) (produce :config code) (produce :start start))))
                )
         ;; (difference (seq-difference (seq-map (lambda (pair) (car pair)) alist)
         ;;                             (cons :name (bk--get-active-args order))))
         )
    ;; (unless (null difference)
    ;;   (error "Superfluous arguments in block `%s': %s" name difference))
    `(progn ,@(bk--expand-order order alist))))

(defmacro bk-defop (name arglist body)
  "Define an operator named NAME with ARGLIST as its arguments.
Nonbranching operators must return a progn, branching operators may
return any expression."
  (declare (indent defun))
  `(prog1 ',name
     (setf (alist-get ',name bk--operator-alist)
           '(,arglist ,body))))

;;;; Expansion

(defun bk--normalize-order (order)
  (seq-map
   (lambda (entry)
     (pcase entry
       (`(>? ,n1 ,n2 . ,rest) (list 'set n1 n2 rest))
       (`(,fun ,n1) (list 'produce n1 fun))
       (`(,fun ,n1 . ,rest) (list 'rproduce n1 fun (bk--normalize-order rest)))))
   order))

(defun bk--get-active-args (order)
  (seq-mapcat
   (lambda (entry)
     (case (car entry)
       ('set (bk--raise (cadr entry)))
       ('produce (bk--raise (cadr entry)))
       ('rproduce (append (bk--raise (cadr entry)) (bk--get-active-args (cadddr entry))))))
   order))

(defun bk--expand-order (order alist)
  (let ((result nil))
    (seq-do
     (lambda (entry)
       (case (car entry)
         ('set (setq alist (bk--handle-set (cadr entry) (caddr entry) alist)))
         ('produce (push (bk--handle-produce (caddr entry) (cadr entry) alist) result))
         ('rproduce (push (bk--handle-rproduce (caddr entry) (cadr entry) (cadddr entry) alist) result))))
     order)
    (nreverse result)))

(defun bk--handle-set (from to alist)
  (let ((val (alist-get from alist)))
    (when val
      (setf (alist-get to alist) val))
    alist))

(defun bk--handle-produce (op from alist)
  (let ((expansion (bk--handle-produce-internal op from alist)))
    (unless (and (listp expansion) (eq (car expansion) 'progn))
      (error "All operations must return a quoted `progn' sexp: %s" op))
    `(progn ,@(cdr expansion))))

(defun bk--handle-rproduce (op from rest alist)
  (progn
    (let ((expansion
           (bk--handle-produce-internal
            op from alist
            (bk--expand-order rest (copy-alist alist)))))
      `(progn ,@expansion))))

(defun bk--handle-produce-internal (op from alist &optional next)
  (let* ((operator
          (or (alist-get op bk--operator-alist)
              (error "Invalid operator: %s" op)))
         (arglist (car operator))
         (body (cadr operator))
         (args (if (listp from)
                   (seq-map (lambda (it) (alist-get it alist)) from)
                 (alist-get from alist))))
    (eval
     `(progn
        ;; (condition-case nil
        ;;     (cl-destructuring-bind ,arglist ',args)
        ;;   (error (error "Incorrect argument structure for operation `%s'. Wanted: %s Got: %s" ',op ',arglist ',args)))
        (cl-destructuring-bind ,(cons 'next arglist) ',(cons next args)
          ,body)
        ;; (condition-case err
        ;;     (cl-destructuring-bind ,(cons 'next arglist) ',(cons next args)
        ;;       ,body)
        ;;   (error (error "Error while expanding `%s' with `%s': %s" ',op ',args err)))
        ))))

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

(defun bk--raise (item)
  (if (listp item)
      item
    (list item)))

;;;; Runtime

(defmacro bk-blk (name &rest body)
  (declare (indent 1))
  `(prog1 ',name
     ,@body)
  ;; `(condition-case-unless-debug err
  ;;      (prog1 ',name ,@body)
  ;;    (error (display-warning
  ;;            ',(intern (format "bk-block/%s" name))
  ;;            (format "%s" (cadr err))
  ;;            :error)))
  )

(defmacro bk-op (name args &rest body)
  (declare (indent 2))
  `(progn ,@body)
  ;; `(condition-case err
  ;;      (progn ,@body)
  ;;    (error (error "In operation `%s' with args `%s': %s" ',name ',args err)))
  )

;;;; End:

(provide 'bk-block-core)

;;; bk-block-core.el ends here
