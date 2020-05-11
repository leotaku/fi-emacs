;;; bk-test.el --- manual tests and benchmarking for bk-block.el

;;; Commentary:

;;; Code:

(pp-macroexpand-expression
 '(bk-block foo
    :config (doo)
    :init (doo)
    :init (do)
    :requires 10 10
    :requires 10 1
    :wanted-by 10))

(cl-defmacro foo (&key foo bar baz bat cool))

(benchmark-run-compiled 1000
  (macroexpand
   '(foo
     :foo nil
     :bar nil
     :baz nil
     :bat t
     :cool t)))

(benchmark-run-compiled 1000
  (macroexpand
   '(leaf foo
      :config nil)))

(benchmark-run-compiled 1000
  (macroexpand
   '(leaf foo
	  :config nil
	  :init nil)))

(benchmark-run-compiled 1000
  (macroexpand
   '(use-package foo
	  :config nil
	  :init nil)))

(benchmark-run-compiled 1000
  (macroexpand
   '(bk-block foo
      :config (doo)
      :at-load (doo)
      :requires 10
      :wanted-by 10)))

(/
 (car (benchmark-run-compiled 1000
        (macroexpand
         '(use-package foo
            :config nil
            :init nil))))
 (car (benchmark-run-compiled 1000
        (macroexpand
         '(bk-block foo
            :config nil
            :at-load nil
            :requires 10
            :wanted-by 10)))))
(/
 (car (benchmark-run-compiled 1000
        (macroexpand
         '(bk-block foo
            :config nil
            :at-load nil
            :requires 10
            :wanted-by 10))))
 (car (benchmark-run-compiled 1000
        (macroexpand
         '(foo
           :foo nil
           :bar nil
           :baz nil
           :bat t
           :cool t)))))

;;; bk-test.el ends here
