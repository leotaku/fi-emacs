;;; bk-block.el --- flexible blocks for your init file


;;; Commentary:
;; 

(require 'bk-block-core)
(require 'fi-config)

;;; Code:

(setq bk-transition-order
      '(auto-package
        straight
        after
        custom
        hook
        bind
        bind*
        init
        defer
        require
        load
        start
        config))

(deftransitionb auto-package :name (name)
  (lexical-let ((package ',name))
    ,@next))

(deftransitionb defer :defer (&optional (defer nil))
  ,@(if defer
	`((with-eval-after-load package
            ,@next))
      next))

(deftransition straight :straight (&rest packages)
  ,@(mapcar (lambda (it)
              (cond ((eq it t)
                     `(straight-use-package package))
                    (it
                     `(straight-use-package ',it))))
            packages))

(deftransition bind :bind (&optional keys)
  (leaf-keys ,keys))

(deftransition bind* :bind* (&optional keys)
  (leaf-keys* ,keys))

(deftransition after :after (&rest files)
  ,@(mapcar (lambda (it)
              `(with-eval-after-load ',it
                 (require package)))
            files))

(deftransition config :config (&rest body)
  ,@body)

(deftransition init :init (&rest body)
  ,@body)

(deftransition start :start (&rest functions)
  ,@(mapcar
     (lambda (it)
       `(,it))
     functions))

(deftransition require :require (&rest packages)
  ,@(mapcar
     (lambda (it)
       `(require ',it))
     packages))

(deftransition load :load (&rest files)
  ,@(mapcar
     (lambda (it)
       `(load ,(expand-file-name it user-emacs-directory)))
     files))

(deftransition custom :custom (&rest pairs) ;; (&whole pairs &rest (symbol . value))
  ,@(mapcar
     (lambda (it)
       `(fi-csetq ,(car it) ,(cdr it)))
     pairs))

(deftransition hook :hook (&rest hooks) ;; (&whole hooks &rest (hook . function))
  ,@(mapcar
     (lambda (pair)
       (cl-destructuring-bind (hook . function)
           pair
         (if (listp hook)
             (cons 'progn
                   (mapcar
                    (lambda (hook)
                      `(add-hook ',hook ',function))
                    hook))
           `(add-hook ',hook ',function))))
     hooks))

;; (pp-macroexpand-expression
;;  (quote
;;   (bk-block company
;;     :straight nil
;;     :after emacs
;;     :hook
;;     ((baz foo) . bar)
;;     (boo . baz)
;;     :custom
;;     (foo . "foo")
;;     (foo . bar)
;;     (bar . "baz")
;;     :init
;;     (error "foo")
;;     :defer t)))

(provide 'bk-block)

;;; bk-block.el ends here
