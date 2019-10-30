;;; bk-test.el --- manual tests and benchmarking for bk-block.el

;;; Commentary:
;; 

;;; Code:

(pp-macroexpand-expression
 '(bk-block foo
    :config (doo)
    :init (doo)
    :init (do)
    :requires 10 10
    :requires 10 1
    :wanted-by 10))

(benchmark-run 1000
  (macroexpand-1
   '(bk-block foo
      :config (doo)
      :init (doo)
      :requires 10
      :wanted-by 10)))

(benchmark-run 1000
  (macroexpand-1
   '(leaf foo
      :config nil)))

(benchmark-run 2000
  (macroexpand-1
   '(leaf foo
	  :config 10
	  :init 10)))

;;; bk-test.el ends here
