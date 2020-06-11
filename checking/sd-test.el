;;; sd-test.el --- automatic tests for sd.el

;;; Commentary:

(require 'sd)
(require 'ert)

;;; Code:

(defmacro sd-ert-deftest (name &rest body)
  "Macro for creating a clean and namespaced `ert-deftest' for sd.el."
  (declare (indent 1))
  `(ert-deftest ,(intern (concat "sd-" (symbol-name name)))
       ()
     (let ((sd--in-unit-setup-phase t)
           (sd-startup-list nil))
       ,@body)))

(sd-ert-deftest simple-success
  (sd-register-unit 'foo)
  (should (equal (sd-reach-target 'foo) 'success)))

(sd-ert-deftest simple-success-again
  (sd-register-unit 'foo)
  (should (equal (sd-reach-target 'foo) 'success))
  (should (equal (sd-reach-target 'foo) 'success)))

(sd-ert-deftest simple-fail
  (should (equal (sd-reach-target 'foo) nil)))

(sd-ert-deftest dep-fail
  (sd-register-unit 'foo nil '(bar))
  (should (equal (sd-reach-target 'foo) '(dependencies bar))))

(sd-ert-deftest dep-fail-twice
  (sd-register-unit 'foo nil '(bar))
  (should (equal (sd-reach-target 'foo) '(dependencies bar)))
  (should (equal (sd-reach-target 'foo) '(dependencies bar))))

(sd-ert-deftest run-only-once
  (setq tracker nil)
  (sd-register-unit 'foo '(setq tracker t))
  (should (eq tracker nil))
  (sd-reach-target 'foo)
  (should (eq tracker t))
  (setq tracker nil)
  (sd-reach-target 'foo)
  (should (eq tracker nil)))

(sd-ert-deftest list-recursion
  (sd-register-unit 'foo nil '(foo))
  (sd--generate-unit-sequence 'foo nil))

(sd-ert-deftest recursion
  (sd-register-unit 'foo nil '(foo))
  (should (equal
           (sd-reach-target 'foo)
           '(recursive))))

(sd-ert-deftest mutual-recursion
  (sd-register-unit 'foo nil '(bar))
  (sd-register-unit 'bar nil '(foo))
  (should (equal
           (sd-reach-target 'foo)
           '(dependencies bar)))
  (should (equal
           (sd-reach-target 'bar)
           '(recursive))))

;;; sd-test.el ends here
