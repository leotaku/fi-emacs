;;; bk-block.el --- flexible blocks for your init file


;;; Commentary:
;; 

(require 'bk-block-core)
(require 'fi-config)

;;; Code:

(setq bk-default-order
      '((>? :name :package)
        (straight (:straight :package))
        (after (:after :package))
        (custom :custom)
        (hook :hook)
        (bind :bind)
        (bind* :bind*)
        (code :init)
        (defer (:defer :package)
          (require (:require :package))
          (load :load)
          (code :config)
          (start :start))))

(bk-defop straight ((&rest packages) (&optional default))
  `(progn ,@(seq-map
             (lambda (package)
               '(straight-use-package
                 ',(if (eq t package)
                       default
                     package)))
             packages)))

(bk-defop bind (&optional keys)
  `(progn
     (leaf-keys ,keys)))

(deftransition bind* (&optional keys)
  `(progn
     (leaf-keys* ,keys)))

(deftransition after ((&rest files) (&optional package))
  `(progn
     ,@(seq-map
        (lambda (it)
          `(with-eval-after-load ',it
             (require ,package)))
        files)))

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
