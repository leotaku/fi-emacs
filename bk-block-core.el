;;; bk-block-core.el --- flexible api for defining configuration blocks

;;; Commentary:

;; TODO: support multiple same keywords in block by appending
;; TODO: allow reading multiple vars in deftransition
;; TODO: remove deftransition argument and put it in order instead

(require 'cl)
(require 'seq)

;;; Code:

;;;; Variables

(defconst bk--transition-alist '()
  "Internal alist used by bk for symbol lookup.")

(defconst bk-transition-order '()
  "List used by bk to determine the order of transitions.")

(defconst bk-current-block nil
  "Variable used by bk to track the currently active block.")

(defconst bk-current-transition nil
  "Variable used by bk to track the currently active transition.")

(defconst bk--quoted-next ',@next)

;;;; User interface and error handling:

(defmacro bk-block (name &rest args)
  (declare (indent 1))
  (let* ((alist (append (bk--construct-alist args) `((:name ,name))))
         (unevaluated
          (bk--run-transitions name bk-transition-order alist)))
    `(bk-internal-block ,name ,@unevaluated)))

(defmacro bk-internal-block (name &rest body)
  (declare (indent 1))
  `(lexical-let ((bk-current-block ',name)
                 (bk-current-transition 'entry))
     (condition-case-unless-debug err
         (prog1 ',name ,@body)
       (error (display-warning
               (intern (format "bk-block/%s" bk-current-block))
               (format "In transition %s: %s" bk-current-transition err)
               :error)))))

(defmacro bk-branch (name &rest body)
  (declare (indent 1))
  `(progn
     (setq bk-current-transition ',name)
     ,@body))

(defalias 'bk-leaf 'bk-branch)

(defmacro deftransition (name &optional arg arglist &rest body)
  `(add-to-list
    'bk--transition-alist
    (cons ',name (cons ',arg (transition ,arglist (bk-leaf ,name ,@body)
                                         ,bk--quoted-next)))))
(defmacro deftransitionb (name &optional arg arglist &rest body)
  `(add-to-list
    'bk--transition-alist
    (cons ',name (cons ',arg (transition ,arglist (bk-branch ,name ,@body))))))

;;;; Transition system:

(defmacro transition (arglist &rest body)
  (declare (indent 1))
  `(lambda (args)
     ;; (condition-case-unless-debug nil
     ;;     (cl-destructuring-bind ,arglist args)
     ;;   (error (error "Wrong number of arguments in transition. Arglist: %s" ',arglist)))
     (lexical-let ((args args))
       (lambda (next)
         (bk--expand-macro-body ,arglist args
           ,body)))))

(defun bk--run-transitions (block t-names value-alist &optional transition-alist)
  (let* ((transition-alist (or transition-alist bk--transition-alist))
         (transitions (mapcar
                       (lambda (t-name)
                         (let* ((t-pair (or (alist-get t-name transition-alist)
                                            (error "Fuck %s" t-name)))
                                (trans (cdr t-pair))
                                (args (if (car t-pair)
                                          (alist-get (car t-pair) value-alist)
                                        nil)))
                           (condition-case-unless-debug err
                               (cons t-name (funcall trans args))
                             (error (error "This shouldn't be possible: %s" err)))))
                       t-names)))
    (seq-reduce
     (lambda (acc it)
       (condition-case-unless-debug err
           (funcall (cdr it) acc)
         (error (error "Error during expansion of transition `%s' in block `%s': %s" (car it) block err))))
     (nreverse transitions)
     nil)))

;;;; Implementation:

(defmacro bk--expand-macro-body (arglist args body)
  (declare (indent 2))
  `(cl-destructuring-bind ,arglist ,args
     ,(cdr (backquote-process body))))

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

;;;; End:

(provide 'bk-block-core)

;;; bk-block-core.el ends here
