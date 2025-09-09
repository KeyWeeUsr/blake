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
(ert-deftest blake-two-schedules ()
  (should (equal [[  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 ]
                  [ 14 10  4  8  9 15 13  6  1 12  0  2 11  7  5  3 ]
                  [ 11  8 12  0  5  2 15 13 10 14  3  6  7  1  9  4 ]
                  [  7  9  3  1 13 12 11 14  2  6  5 10  4  0 15  8 ]
                  [  9  0  5  7  2  4 10 15 14  1 11 12  6  8  3 13 ]
                  [  2 12  6 10  0 11  8  3  4 13  7  5 15 14  1  9 ]
                  [ 12  5  1 15 14 13  4 10  0  7  6  3  9  2  8 11 ]
                  [ 13 11  7 14 12  1  3  9  5  0 15  4  8  6  2 10 ]
                  [  6 15 14  9 11  3  0  8 12  2 13  7  1  4 10  5 ]
                  [ 10  2  8  4  7  6  1  5 15 11  9 14  3 12 13  0 ]]
                 blake-two-schedule-small))
  (should (equal [[  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 ]
                  [ 14 10  4  8  9 15 13  6  1 12  0  2 11  7  5  3 ]
                  [ 11  8 12  0  5  2 15 13 10 14  3  6  7  1  9  4 ]
                  [  7  9  3  1 13 12 11 14  2  6  5 10  4  0 15  8 ]
                  [  9  0  5  7  2  4 10 15 14  1 11 12  6  8  3 13 ]
                  [  2 12  6 10  0 11  8  3  4 13  7  5 15 14  1  9 ]
                  [ 12  5  1 15 14 13  4 10  0  7  6  3  9  2  8 11 ]
                  [ 13 11  7 14 12  1  3  9  5  0 15  4  8  6  2 10 ]
                  [  6 15 14  9 11  3  0  8 12  2 13  7  1  4 10  5 ]
                  [ 10  2  8  4  7  6  1  5 15 11  9 14  3 12 13  0 ]
                  [  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 ]
                  [ 14 10  4  8  9 15 13  6  1 12  0  2 11  7  5  3 ]]
                 blake-two-schedule-big)))


(provide 'blake-tests)
;;; blake-tests.el ends here
