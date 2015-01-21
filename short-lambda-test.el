;;; short-lambda-test.el --- Test short-lambda

;;; Commentary:
;; 

(require 'ert)

;;; Code:

(ert-deftest sl--arguments ()
  (should (equal (sl--arguments '(1+ %))
                 '(%)))
  (should (equal (sl--arguments '(vector %& %&))
                 '(&rest %&)))
  (should (equal (sl--arguments '(list % %&))
                 '(% &rest %&)))
  (should (equal (sl--arguments '(list %& %))
                 '(% &rest %&)))
  (should (equal (sl--arguments '(list %5))
                 '(%1 %2 %3 %4 %5)))
  (should (equal (sl--arguments '(cons %5 (cons %5 (cons %1 %&))))
                 '(%1 %2 %3 %4 %5 &rest %&))))

(provide 'short-lambda-test)

;;; short-lambda-test.el ends here
