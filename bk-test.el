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

(benchmark-run 1000
  (macroexpand-1
   '(leaf foo
	  :config nil
	  :init nil)))

(/
 (car (benchmark-run-compiled 1000
        (leaf foo
          :config nil
          :init nil)))
 (car (benchmark-run-compiled 1000
        (bk-block foo
          :config nil
          :init nil
          :requires 10
          :wanted-by 10))))

;;; bk-test.el ends here
