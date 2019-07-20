;;; sd-test.el --- tests for sd.el

;;; Commentary:
;; 

(require 'ert)

;;; Code:

(defmacro sd-ert-deftest (name &rest body)
  "Macro for creating a clean and namespaced `ert-deftest' for sd.el."
  (declare (indent 1))
  `(ert-deftest ,(intern (concat "sd-" (symbol-name name)))
       ()
     (setq sd-unit-list nil)
     (setq sd-in-unit-setup-phase t)
     ,@body
     (setq sd-unit-list nil)
     (setq sd-in-unit-setup-phase t)))

(sd-ert-deftest simple-success
  (sd-register-unit 'foo)
  (should (equal (sd-reach-unit 'foo) nil)))

(sd-ert-deftest simple-fail
  (should (equal (sd-reach-unit 'foo) '(foo noexist))))

(sd-ert-deftest existing-package
  (should (equal (sd-reach-unit 'emacs) nil)))

(sd-ert-deftest existing-package-twice
  (should (equal (sd-reach-unit 'emacs) nil))
  (should (equal (sd-reach-unit 'emacs) nil)))

(sd-ert-deftest dep-fail
  (sd-register-unit 'foo nil '(bar))
  (should (equal (sd-reach-unit 'foo) '(foo dependencies (bar noexist)))))

(sd-ert-deftest dep-fail-twice
  (sd-register-unit 'foo nil '(bar))
  (should (equal (sd-reach-unit 'foo) '(foo dependencies (bar noexist))))
  (should (equal (sd-reach-unit 'foo) '(foo dependencies (bar noexist)))))

(sd-ert-deftest run-only-once
  (setq tracker nil)
  (sd-register-unit 'foo '(setq tracker t))
  (should (eq tracker nil))
  (sd-reach-unit 'foo)
  (should (eq tracker t))
  (setq tracker nil)
  (sd-reach-unit 'foo)
  (should (eq tracker nil)))

(sd-ert-deftest recursion
  (sd-register-unit 'foo nil '(foo))
  (should (equal
           (sd-reach-unit 'foo)
           '(foo dependencies (foo recursive)))))

(sd-ert-deftest mutual-recursion
  (sd-register-unit 'foo nil '(bar))
  (sd-register-unit 'bar nil '(foo))
  (sd-reach-unit 'foo))

(setq foo '(1 3 3))
(setq ref (gv-ref (cdr foo)))
(setf (gv-deref 2) 1)
(identity (gv-deref ref))

(provide 'sd-test)

;;; sd-test.el ends here
