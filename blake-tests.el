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

(ert-deftest blake-two-chunk-data ()
  (let ((matrix `((:name "nil"
                   :data nil
                   :error "Empty data"
                   :result nil)
                  (:name "empty []"
                   :data []
                   :error "Empty data"
                   :result nil)
                  (:name "short [0]"
                   :data [0]
                   :error nil
                   :result [[0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])
                  (:name "short [1]"
                   :data [1]
                   :error nil
                   :result [[1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])
                  (:name "n-1"
                   :data ,(vconcat (number-sequence 1 15))
                   :error nil
                   :result [[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 0]])
                  (:name "n"
                   :data ,(vconcat (number-sequence 1 16))
                   :error nil
                   :result [[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16]])
                  (:name "n+1"
                   :data ,(vconcat (number-sequence 1 17))
                   :error nil
                   :result [[ 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16]
                            [17  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0]])
                  (:name "2n-1"
                   :data ,(vconcat (number-sequence 1 31))
                   :error nil
                   :result [[ 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16]
                            [17 18 19 20 21 22 23 24 25 26 27 28 29 30 31  0]])
                  (:name "2n"
                   :data ,(vconcat (number-sequence 1 32))
                   :error nil
                   :result [[ 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16]
                            [17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32]])
                  (:name "2n+1"
                   :data ,(vconcat (number-sequence 1 33))
                   :error nil
                   :result [[ 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16]
                            [17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32]
                            [33  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0]])
                  )))
    (dolist (item matrix)
      (let (tmp)
        (condition-case error
            (should (equal (plist-get item :result)
                           (blake-two-chunk-data (plist-get item :data))))
          (t (let ((err (plist-get item :error)))
               (if err
                   (should (equal (plist-get item :error)
                                  (error-message-string error)))
                 (should-not error)))))))))

(ert-deftest blake-two-init-state-zero-sample ()
  (let* ((kind blake-two-big)
         (state (vconcat (alist-get kind blake-two-iv))))
    (should (= #x6A09E667F3BCC908
               (aref state 0)
               (aref (alist-get kind blake-two-iv) 0)))
    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=0, v[16][0]
    (should (= #x6A09E667F2BDC948
               (blake-two-init-state-zero
                kind state 0 (alist-get kind blake-two-bits-in-word))))))

(ert-deftest blake-two-first-mix ()
  (let* ((kind blake-two-big)
         (state (vconcat (alist-get kind blake-two-iv)))
         (schedule (aref (vconcat (alist-get kind blake-two-schedule)) 0))
         (msg [#x0000000000636261 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))
    (aset state 0 (blake-two-init-state-zero
                   kind state 0 (alist-get kind blake-two-bits-in-word)))

    (setq state (vconcat state (alist-get kind blake-two-iv)))

    ;; set the counter for the first compression round manually
    ;; note: this should be set by the compress func
    (aset state 12 (1+ (aref state 12)))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=0, v[16][0]
    (should (= #x6A09E667F2BDC948 (aref state 0)))
    (should (= 0 (aref msg (aref schedule 1))))
    (should (= (aref msg 0) (aref msg (aref schedule 0))))
    (should (= #xF0C9AA0F86491DEA
               (aref (blake-two-mix kind state 0 4 8 12
                                    (aref msg (aref schedule 0))
                                    (aref msg (aref schedule 1)))
                     0)))))

(ert-deftest blake-two-one-round-mix-manual ()
  (let* ((kind blake-two-big)
         (state (vconcat (alist-get kind blake-two-iv)))
         (schedule (aref (vconcat (alist-get kind blake-two-schedule)) 0))
         (msg [#x0000000000636261 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]))
    ;; initialize state
    ;; note: this should be set by the blake2 func
    (aset state 0 (blake-two-init-state-zero
                   kind state 0 (alist-get kind blake-two-bits-in-word)))

    ;; state is a 16-element array of [,@init state, ,@IV]
    (setq state (vconcat state (alist-get kind blake-two-iv)))

    ;; set the counter for the first compression round manually
    ;; note: this should be set by the compress func
    (aset state 12 (1+ (aref state 12)))

    ;; invert the v[14] manually
    ;; note: this should be set by the compress func
    ;; note: this happens only when the final compression takes the place
    ;;       for which a small-enough message falls only to the final stage
    ;;       without intermediate compression calls
    (aset state 14
          (logand (lognot (aref state 14))
                  (1- (expt 2 (alist-get kind blake-two-bits-in-word)))))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=0, v[16][0]
    (should (= #x6A09E667F2BDC948 (aref state 0)))
    (should (= 0 (aref msg (aref schedule 1))))
    (should (= (aref msg 0) (aref msg (aref schedule 0))))

    (setq state (blake-two-mix kind state 0 4 8 12
                               (aref msg (aref schedule 0))
                               (aref msg (aref schedule 1))))
    (should (equal [#xF0C9AA0F86491DEA #xBB67AE8584CAA73B #x3C6EF372FE94F82B
                    #xA54FF53A5F1D36F1 #xB7DEFC3D0277E11F #x9B05688C2B3E6C1F
                    #x1F83D9ABFB41BD6B #x5BE0CD19137E2179 #xEE5E0F39647A9FFF
                    #xBB67AE8584CAA73B #x3C6EF372FE94F82B #xA54FF53A5F1D36F1
                    #x7772FC2886A76C5F #x9B05688C2B3E6C1F #xE07C265404BE4294
                    #x5BE0CD19137E2179]
                   state))

    (setq state (blake-two-mix kind state 1 5 9 13
                               (aref msg (aref schedule 2))
                               (aref msg (aref schedule 3))))
    (should (equal [#xF0C9AA0F86491DEA #x63B7DEDF4A4E5AD3 #x3C6EF372FE94F82B
                    #xA54FF53A5F1D36F1 #xB7DEFC3D0277E11F #xED49C30CD311D50E
                    #x1F83D9ABFB41BD6B #x5BE0CD19137E2179 #xEE5E0F39647A9FFF
                    #x7BEE264BF3CDADFE #x3C6EF372FE94F82B #xA54FF53A5F1D36F1
                    #x7772FC2886A76C5F #x254EF880A19A8726 #xE07C265404BE4294
                    #x5BE0CD19137E2179]
                   state))

    (setq state (blake-two-mix kind state 2 6 10 14
                               (aref msg (aref schedule 4))
                               (aref msg (aref schedule 5))))
    (should (equal [#xF0C9AA0F86491DEA #x63B7DEDF4A4E5AD3 #xBE50EB454E0A93D7
                    #xA54FF53A5F1D36F1 #xB7DEFC3D0277E11F #xED49C30CD311D50E
                    #xA056671704B00D71 #x5BE0CD19137E2179 #xEE5E0F39647A9FFF
                    #x7BEE264BF3CDADFE #xB2752DADD66BD8F9 #xA54FF53A5F1D36F1
                    #x7772FC2886A76C5F #x254EF880A19A8726 #x789D43381C47F584
                    #x5BE0CD19137E2179]
                   state))

    (setq state (blake-two-mix kind state 3 7 11 15
                               (aref msg (aref schedule 6))
                               (aref msg (aref schedule 7))))
    (should (equal [#xF0C9AA0F86491DEA #x63B7DEDF4A4E5AD3 #xBE50EB454E0A93D7
                    #x949804B0483EAD14 #xB7DEFC3D0277E11F #xED49C30CD311D50E
                    #xA056671704B00D71 #x75E6432FC4661B06 #xEE5E0F39647A9FFF
                    #x7BEE264BF3CDADFE #xB2752DADD66BD8F9 #xA99463CB37905929
                    #x7772FC2886A76C5F #x254EF880A19A8726 #x789D43381C47F584
                    #xA25EF57D7DA312EE]
                   state))

    (setq state (blake-two-mix kind state 0 5 10 15
                               (aref msg (aref schedule 8))
                               (aref msg (aref schedule 9))))
    (should (equal [#x86B7C1568029BB79 #x63B7DEDF4A4E5AD3 #xBE50EB454E0A93D7
                    #x949804B0483EAD14 #xB7DEFC3D0277E11F #xA447C850AA694A7E
                    #xA056671704B00D71 #x75E6432FC4661B06 #xEE5E0F39647A9FFF
                    #x7BEE264BF3CDADFE #xFA87B01273FA6DBE #xA99463CB37905929
                    #x7772FC2886A76C5F #x254EF880A19A8726 #x789D43381C47F584
                    #x2318A24E2140FC64]
                   state))

    (setq state (blake-two-mix kind state 1 6 11 12
                               (aref msg (aref schedule 10))
                               (aref msg (aref schedule 11))))
    (should (equal [#x86B7C1568029BB79 #xC12CBCC809FF59F3 #xBE50EB454E0A93D7
                    #x949804B0483EAD14 #xB7DEFC3D0277E11F #xA447C850AA694A7E
                    #xDE080F1BB1C0F84B #x75E6432FC4661B06 #xEE5E0F39647A9FFF
                    #x7BEE264BF3CDADFE #xFA87B01273FA6DBE #x521A715C63E08D8A
                    #xE02D0975B8D37A83 #x254EF880A19A8726 #x789D43381C47F584
                    #x2318A24E2140FC64]
                   state))

    (setq state (blake-two-mix kind state 2 7 8 13
                               (aref msg (aref schedule 12))
                               (aref msg (aref schedule 13))))
    (should (equal [#x86B7C1568029BB79 #xC12CBCC809FF59F3 #xC6A5214CC0EACA8E
                    #x949804B0483EAD14 #xB7DEFC3D0277E11F #xA447C850AA694A7E
                    #xDE080F1BB1C0F84B #x595CB8A9A1ACA66C #xBEC3AE837EAC4887
                    #x7BEE264BF3CDADFE #xFA87B01273FA6DBE #x521A715C63E08D8A
                    #xE02D0975B8D37A83 #x1C7B754F08B7D193 #x789D43381C47F584
                    #x2318A24E2140FC64]
                   state))

    (setq state (blake-two-mix kind state 3 4 9 14
                               (aref msg (aref schedule 14))
                               (aref msg (aref schedule 15))))
    (should (equal [#x86B7C1568029BB79 #xC12CBCC809FF59F3 #xC6A5214CC0EACA8E
                    #x0C87CD524C14CC5D #x44EE6039BD86A9F7 #xA447C850AA694A7E
                    #xDE080F1BB1C0F84B #x595CB8A9A1ACA66C #xBEC3AE837EAC4887
                    #x6267FC79DF9D6AD1 #xFA87B01273FA6DBE #x521A715C63E08D8A
                    #xE02D0975B8D37A83 #x1C7B754F08B7D193 #x8F885A76B6E578FE
                    #x2318A24E2140FC64]
                   state))))

(provide 'blake-tests)
;;; blake-tests.el ends here
