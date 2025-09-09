;;; blake-tests.el -- tests for blake

;;; Code:

(require 'ert)
(require 'blake)

(ert-deftest blake-two-bits-resolve ()
  (should (equal 64 (alist-get blake-two-big blake-two-bits-in-word)))
  (should (equal 32 (alist-get blake-two-small blake-two-bits-in-word))))

(ert-deftest blake-two-rounds-resolve ()
  (should (equal 12 (alist-get blake-two-big blake-two-rounds)))
  (should (equal 10 (alist-get blake-two-small blake-two-rounds))))

(ert-deftest blake-two-block-size-resolve ()
  (should (equal 128 (alist-get blake-two-big blake-two-block-size)))
  (should (equal 64 (alist-get blake-two-small blake-two-block-size))))

(ert-deftest blake-two-hash-size-limits-resolve ()
  (let ((big (alist-get blake-two-big blake-two-hash-size-limits))
        (small (alist-get blake-two-small blake-two-hash-size-limits)))
    (should (equal 64 (alist-get 'upper big)))
    (should (equal 1 (alist-get 'lower big)))
    (should (equal 32 (alist-get 'upper small)))
    (should (equal 1 (alist-get 'lower small)))))

(ert-deftest blake-two-key-size-limits-resolve ()
  (let ((big (alist-get blake-two-big blake-two-key-size-limits))
        (small (alist-get blake-two-small blake-two-key-size-limits)))
    (should (equal 64 (alist-get 'upper big)))
    (should (equal 1 (alist-get 'lower big)))
    (should (equal 32 (alist-get 'upper small)))
    (should (equal 1 (alist-get 'lower small)))))

(ert-deftest blake-two-input-size-limits-resolve ()
  (let ((big (alist-get blake-two-big blake-two-input-size-limits))
        (small (alist-get blake-two-small blake-two-input-size-limits)))
    (should (equal (expt 2 128) (alist-get 'upper big)))
    (should (equal 0 (alist-get 'lower big)))
    (should (equal (expt 2 64) (alist-get 'upper small)))
    (should (equal 0 (alist-get 'lower small)))))

(ert-deftest blake-two-rotconst-resolve ()
  (should (equal '(32 24 16 63)
                 (alist-get blake-two-big blake-two-rotconst)))
  (should (equal '(16 12 8 7)
                 (alist-get blake-two-small blake-two-rotconst))))

(provide 'blake-tests)
;;; blake-tests.el ends here
