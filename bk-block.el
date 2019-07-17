;;; bk-block.el --- flexible blocks for your init file


;;; Commentary:
;; 

(require 'bk-block-core)
(require 'fi-config)
(require 'leaf)

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

(bk-defop defer ((&optional pred) (&optional package))
  (if (not pred)
      next
    `((with-eval-after-load ',package
        ,@next))))

(bk-defop straight ((&rest packages) (&optional default))
  `(progn ,@(seq-map
             (lambda (package)
               `(straight-use-package
                 ',(if (eq t package)
                       default
                     package)))
             packages)))

(bk-defop bind (&optional keys)
  `(progn
     (leaf-keys ,keys)))

(bk-defop bind* (&optional keys)
  `(progn
     (leaf-keys* ,keys)))

(bk-defop after ((&rest files) (&optional package))
  `(progn
     ,@(seq-map
        (lambda (it)
          `(with-eval-after-load ',it
             (require ',package)))
        files)))

(bk-defop code (&rest body)
  `(progn ,@body))

(bk-defop start (&rest functions)
  `(progn
     ,@(seq-map
        (lambda (it)
          `(,it))
        functions)))

(bk-defop require ((&rest packages) (&optional default))
  `(progn
     ,@(seq-map
        (lambda (it)
          `(require ',(if (eq it t)
                          default
                        it)))
        packages)))

(bk-defop load (&rest files)
  `(progn
     ,@(mapcar
        (lambda (it)
          `(load ,(expand-file-name it user-emacs-directory)))
        files)))

(bk-defop custom (&rest pairs)
  `(progn
     ,@(mapcar
        (lambda (it)
          `(fi-csetq ,(car it) ,(cdr it)))
        pairs)))

(bk-defop hook (&rest hooks) ;; (&whole hooks &rest (hook . function))
  `(progn
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
        hooks)))

(provide 'bk-block)

;;; bk-block.el ends here
