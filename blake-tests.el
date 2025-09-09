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
                   :data ,(vconcat (number-sequence 1 (1- blake-two-msg-size)))
                   :error nil
                   :result [[#x0807060504030201 #x000F0E0D0C0B0A09
                             0 0 0 0 0 0 0 0 0 0 0 0 0 0]])
                  (:name "n"
                   :data ,(vconcat (number-sequence 1 blake-two-msg-size))
                   :error nil
                   :result [[#x0807060504030201 #x100F0E0D0C0B0A09
                             0 0 0 0 0 0 0 0 0 0 0 0 0 0]])
                  (:name "n+1"
                   :data ,(vconcat (number-sequence 1 (1+ blake-two-msg-size)))
                   :error nil
                   :result [[#x0807060504030201 #x100F0E0D0C0B0A09
                             #x0000000000000011 0 0 0 0 0 0 0 0 0 0 0 0 0]])
                  (:name "2n-1"
                   :data ,(vconcat (number-sequence
                                    1 (1- (* 2 blake-two-msg-size))))
                   :error nil
                   :result [[#x0807060504030201 #x100F0E0D0C0B0A09
                             #x1817161514131211 #x001F1E1D1C1B1A19
                             0 0 0 0 0 0 0 0 0 0 0 0]])
                  (:name "2n"
                   :data ,(vconcat (number-sequence
                                    1 (* 2 blake-two-msg-size)))
                   :error nil
                   :result [[#x0807060504030201 #x100F0E0D0C0B0A09
                             #x1817161514131211 #x201F1E1D1C1B1A19
                             0 0 0 0 0 0 0 0 0 0 0 0]])
                  (:name "2n+1"
                   :data ,(vconcat (number-sequence
                                    1 (1+ (* 2 blake-two-msg-size))))
                   :error nil
                   :result [[#x0807060504030201 #x100F0E0D0C0B0A09
                             #x1817161514131211 #x201F1E1D1C1B1A19
                             #x0000000000000021 0 0 0 0 0 0 0 0 0 0 0]])
                  (:name "8n-1"
                   :data ,(vconcat (number-sequence
                                    1 (1- (* 8 blake-two-msg-size))))
                   :error nil
                   :result [[#x0807060504030201 #x100F0E0D0C0B0A09
                             #x1817161514131211 #x201F1E1D1C1B1A19
                             #x2827262524232221 #x302F2E2D2C2B2A29
                             #x3837363534333231 #x403F3E3D3C3B3A39
                             #x4847464544434241 #x504F4E4D4C4B4A49
                             #x5857565554535251 #x605F5E5D5C5B5A59
                             #x6867666564636261 #x706F6E6D6C6B6A69
                             #x7877767574737271 #x007F7E7D7C7B7A79]])
                  (:name "8n"
                   :data ,(vconcat (number-sequence
                                    1 (* 8 blake-two-msg-size)))
                   :error nil
                   :result [[#x0807060504030201 #x100F0E0D0C0B0A09
                             #x1817161514131211 #x201F1E1D1C1B1A19
                             #x2827262524232221 #x302F2E2D2C2B2A29
                             #x3837363534333231 #x403F3E3D3C3B3A39
                             #x4847464544434241 #x504F4E4D4C4B4A49
                             #x5857565554535251 #x605F5E5D5C5B5A59
                             #x6867666564636261 #x706F6E6D6C6B6A69
                             #x7877767574737271 #x807F7E7D7C7B7A79]])
                  (:name "8n+1"
                   :data ,(vconcat (number-sequence
                                    1 (1+ (* 8 blake-two-msg-size))))
                   :error nil
                   :result [[#x0807060504030201 #x100F0E0D0C0B0A09
                             #x1817161514131211 #x201F1E1D1C1B1A19
                             #x2827262524232221 #x302F2E2D2C2B2A29
                             #x3837363534333231 #x403F3E3D3C3B3A39
                             #x4847464544434241 #x504F4E4D4C4B4A49
                             #x5857565554535251 #x605F5E5D5C5B5A59
                             #x6867666564636261 #x706F6E6D6C6B6A69
                             #x7877767574737271 #x807F7E7D7C7B7A79]
                            [#x0000000000000081
                             0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]])
                  (:name "16n-1"
                   :data ,(vconcat (number-sequence
                                    1 (1- (* 16 blake-two-msg-size))))
                   :error nil
                   :result [[#x0807060504030201 #x100F0E0D0C0B0A09
                             #x1817161514131211 #x201F1E1D1C1B1A19
                             #x2827262524232221 #x302F2E2D2C2B2A29
                             #x3837363534333231 #x403F3E3D3C3B3A39
                             #x4847464544434241 #x504F4E4D4C4B4A49
                             #x5857565554535251 #x605F5E5D5C5B5A59
                             #x6867666564636261 #x706F6E6D6C6B6A69
                             #x7877767574737271 #x807F7E7D7C7B7A79]
                            [#x8887868584838281 #x908F8E8D8C8B8A89
                             #x9897969594939291 #xA09F9E9D9C9B9A99
                             #xA8A7A6A5A4A3A2A1 #xB0AFAEADACABAAA9
                             #xB8B7B6B5B4B3B2B1 #xC0BFBEBDBCBBBAB9
                             #xC8C7C6C5C4C3C2C1 #xD0CFCECDCCCBCAC9
                             #xD8D7D6D5D4D3D2D1 #xE0DFDEDDDCDBDAD9
                             #xE8E7E6E5E4E3E2E1 #xF0EFEEEDECEBEAE9
                             #xF8F7F6F5F4F3F2F1 #x00FFFEFDFCFBFAF9]])
                  (:name "16n+1"
                   :data ,(vconcat (number-sequence
                                    1 (1+ (* 16 blake-two-msg-size))))
                   :error nil
                   :result [[#x0807060504030201 #x100F0E0D0C0B0A09
                             #x1817161514131211 #x201F1E1D1C1B1A19
                             #x2827262524232221 #x302F2E2D2C2B2A29
                             #x3837363534333231 #x403F3E3D3C3B3A39
                             #x4847464544434241 #x504F4E4D4C4B4A49
                             #x5857565554535251 #x605F5E5D5C5B5A59
                             #x6867666564636261 #x706F6E6D6C6B6A69
                             #x7877767574737271 #x807F7E7D7C7B7A79]
                            [#x8887868584838281 #x908F8E8D8C8B8A89
                             #x9897969594939291 #xA09F9E9D9C9B9A99
                             #xA8A7A6A5A4A3A2A1 #xB0AFAEADACABAAA9
                             #xB8B7B6B5B4B3B2B1 #xC0BFBEBDBCBBBAB9
                             #xC8C7C6C5C4C3C2C1 #xD0CFCECDCCCBCAC9
                             #xD8D7D6D5D4D3D2D1 #xE0DFDEDDDCDBDAD9
                             #xE8E7E6E5E4E3E2E1 #xF0EFEEEDECEBEAE9
                             #xF8F7F6F5F4F3F2F1 #x0100fffefdfcfbfaf9]
                            [#x0000000000000101
                             0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]))))
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

(ert-deftest blake-two-twelve-round-mix ()
  (let* ((kind blake-two-big)
         (state (vconcat (alist-get kind blake-two-iv)))
         (schedules (vconcat (alist-get kind blake-two-schedule)))
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
    (should (= 0 (aref msg (aref (aref schedules 0) 1))))
    (should (= (aref msg 0) (aref msg (aref (aref schedules 0) 0))))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=0, v[16]
    (should (equal [#x6A09E667F2BDC948 #xBB67AE8584CAA73B #x3C6EF372FE94F82B
                    #xA54FF53A5F1D36F1 #x510E527FADE682D1 #x9B05688C2B3E6C1F
                    #x1F83D9ABFB41BD6B #x5BE0CD19137E2179 #x6A09E667F3BCC908
                    #xBB67AE8584CAA73B #x3C6EF372FE94F82B #xA54FF53A5F1D36F1
                    #x510E527FADE682D2 #x9B05688C2B3E6C1F #xE07C265404BE4294
                    #x5BE0CD19137E2179]
                   state))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=1, v[16]
    (setq state (blake-two-round kind state msg (aref schedules 0)))
    (should (equal [#x86B7C1568029BB79 #xC12CBCC809FF59F3 #xC6A5214CC0EACA8E
                    #x0C87CD524C14CC5D #x44EE6039BD86A9F7 #xA447C850AA694A7E
                    #xDE080F1BB1C0F84B #x595CB8A9A1ACA66C #xBEC3AE837EAC4887
                    #x6267FC79DF9D6AD1 #xFA87B01273FA6DBE #x521A715C63E08D8A
                    #xE02D0975B8D37A83 #x1C7B754F08B7D193 #x8F885A76B6E578FE
                    #x2318A24E2140FC64]
                   state))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=2, v[16]
    (setq state (blake-two-round kind state msg (aref schedules 1)))
    (should (equal [#x53281E83806010F2 #x3594B403F81B4393 #x8CD63C7462DE0DFF
                    #x85F693F3DA53F974 #xBAABDBB2F386D9AE #xCA5425AEC65A10A8
                    #xC6A22E2FF0F7AA48 #xC6A56A51CB89C595 #x224E6A3369224F96
                    #x500E125E58A92923 #xE9E4AD0D0E1A0D48 #x85DF9DC143C59A74
                    #x92A3AAAA6D952B7F #xC5FDF71090FAE853 #x2A8A40F15A462DD0
                    #x572D17EFFDD37358]
                   state))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=3, v[16]
    (setq state (blake-two-round kind state msg (aref schedules 2)))
    (should (equal [#x60ED96AA7AD41725 #xE46A743C71800B9D #x1A04B543A01F156B
                    #xA2F8716E775C4877 #xDA0A61BCDE4267EA #xB1DD230754D7BDEE
                    #x25A1422779E06D14 #xE6823AE4C3FF58A5 #xA1677E19F37FD5DA
                    #x22BDCE6976B08C51 #xF1DE8696BEC11BF1 #xA0EBD586A4A1D2C8
                    #xC804EBAB11C99FA9 #x8E0CEC959C715793 #x7C45557FAE0D4D89
                    #x716343F52FDD265E]
                   state))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=4, v[16]
    (setq state (blake-two-round kind state msg (aref schedules 3)))
    (should (equal [#xBB2A77D3A8382351 #x45EB47971F23B103 #x98BE297F6E45C684
                    #xA36077DEE3370B89 #x8A03C4CB7E97590A #x24192E49EBF54EA0
                    #x4F82C9401CB32D7A #x8CCD013726420DC4 #xA9C9A8F17B1FC614
                    #x55908187977514A0 #x5B44273E66B19D27 #xB6D5C9FCA2579327
                    #x086092CFB858437E #x5C4BE2156DBEECF9 #x2EFEDE99ED4EFF16
                    #x3E7B5F234CD1F804]
                   state))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=5, v[16]
    (setq state (blake-two-round kind state msg (aref schedules 4)))
    (should (equal [#xC79C15B3D423B099 #x2DA2224E8DA97556 #x77D2B26DF1C45C55
                    #x8934EB09A3456052 #x0F6D9EEED157DA2A #x6FE66467AF88C0A9
                    #x4EB0B76284C7AAFB #x299C8E725D954697 #xB2240B59E6D567D3
                    #x2643C2370E49EBFD #x79E02EEF20CDB1AE #x64B3EED7BB602F39
                    #xB97D2D439E4DF63D #xC718E755294C9111 #x1F0893F2772BB373
                    #x1205EA4A7859807D]
                   state))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=6, v[16]
    (setq state (blake-two-round kind state msg (aref schedules 5)))
    (should (equal [#xE58F97D6385BAEE4 #x7640AA9764DA137A #xDEB4C7C23EFE287E
                    #x70F6F41C8783C9F6 #x7127CD48C76A7708 #x9E472AF0BE3DB3F6
                    #x0F244C62DDF71788 #x219828AA83880842 #x41CCA9073C8C4D0D
                    #x5C7912BC10DF3B4B #xA2C3ABBD37510EE2 #xCB5668CC2A9F7859
                    #x8733794F07AC1500 #xC67A6BE42335AA6F #xACB22B28681E4C82
                    #xDB2161604CBC9828]
                   state))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=7, v[16]
    (setq state (blake-two-round kind state msg (aref schedules 6)))
    (should (equal [#x6E2D286EEADEDC81 #xBCF02C0787E86358 #x57D56A56DD015EDF
                    #x55D899D40A5D0D0A #x819415B56220C459 #xB63C479A6A769F02
                    #x258E55E0EC1F362A #x3A3B4EC60E19DFDC #x04D769B3FCB048DB
                    #xB78A9A33E9BFF4DD #x5777272AE1E930C0 #x5A387849E578DBF6
                    #x92AAC307CF2C0AFC #x30AACCC4F06DAFAA #x483893CC094F8863
                    #xE03C6CC89C26BF92]
                   state))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=8, v[16]
    (setq state (blake-two-round kind state msg (aref schedules 7)))
    (should (equal [#xFFC83ECE76024D01 #x1BE7BFFB8C5CC5F9 #xA35A18CBAC4C65B7
                    #xB7C2C7E6D88C285F #x81937DA314A50838 #xE1179523A2541963
                    #x3A1FAD7106232B8F #x1C7EDE92AB8B9C46 #xA3C2D35E4F685C10
                    #xA53D3F73AA619624 #x30BBCC0285A22F65 #xBCEFBB6A81539E5D
                    #x3841DEF6F4C9848A #x98662C85FBA726D4 #x7762439BD5A851BD
                    #xB0B9F0D443D1A889]
                   state))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=9, v[16]
    (setq state (blake-two-round kind state msg (aref schedules 8)))
    (should (equal [#x753A70A1E8FAEADD #x6B0D43CA2C25D629 #xF8343BA8B94F8C0B
                    #xBC7D062B0DB5CF35 #x58540EE1B1AEBC47 #x63C5B9B80D294CB9
                    #x490870ECAD27DEBD #xB2A90DDF667287FE #x316CC9EBEEFAD8FC
                    #x4A466BCD021526A4 #x5DA7F7638CEC5669 #xD9C8826727D306FC
                    #x88ED6C4F3BD7A537 #x19AE688DDF67F026 #x4D8707AAB40F7E6D
                    #xFD3F572687FEA4F1]
                   state))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=10, v[16]
    (setq state (blake-two-round kind state msg (aref schedules 9)))
    (should (equal [#xE630C747CCD59C4F #xBC713D41127571CA #x46DB183025025078
                    #x6727E81260610140 #x2D04185EAC2A8CBA #x5F311B88904056EC
                    #x40BD313009201AAB #x0099D4F82A2A1EAB #x6DD4FBC1DE60165D
                    #xB3B0B51DE3C86270 #x900AEE2F233B08E5 #xA07199D87AD058D8
                    #x2C6B25593D717852 #x37E8CA471BEAA5F8 #x2CFC1BAC10EF4457
                    #x01369EC18746E775]
                   state))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=11, v[16]
    (setq state (blake-two-round kind state msg (aref schedules 10)))
    (should (equal [#xE801F73B9768C760 #x35C6D22320BE511D #x306F27584F65495E
                    #xB51776ADF569A77B #xF4F1BE86690B3C34 #x3CC88735D1475E4B
                    #x5DAC67921FF76949 #x1CDB9D31AD70CC4E #x35BA354A9C7DF448
                    #x4929CBE45679D73E #x733D1A17248F39DB #x92D57B736F5F170A
                    #x61B5C0A41D491399 #xB5C333457E12844A #xBD696BE010D0D889
                    #x02231E1A917FE0BD]
                   state))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=12, v[16]
    (setq state (blake-two-round kind state msg (aref schedules 11)))
    (should (equal [#x12EF8A641EC4F6D6 #xBCED5DE977C9FAF5 #x733CA476C5148639
                    #x97DF596B0610F6FC #xF42C16519AD5AFA7 #xAA5AC1888E10467E
                    #x217D930AA51787F3 #x906A6FF19E573942 #x75AB709BD3DCBF24
                    #xEE7CE1F345947AA4 #xF8960D6C2FAF5F5E #xE332538A36B6D246
                    #x885BEF040EF6AA0B #xA4939A417BFB78A3 #x646CBB7AF6DCE980
                    #xE813A23C60AF3B82]
                   state))))

(ert-deftest blake-two-chunk-raw-data ()
  (should (equal [[#x0000000000636261 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]
                 (blake-two-chunk-data "abc"))))

(ert-deftest blake-two-compress ()
  (let* ((kind blake-two-big)
         (state (vconcat (alist-get kind blake-two-iv)))
         (msg (aref (blake-two-chunk-data "abc") 0))
         (counter 0)
         (final t))
    ;; initialize state
    ;; note: this should be set by the blake2 func
    (aset state 0 (blake-two-init-state-zero
                   kind state 0 (alist-get kind blake-two-bits-in-word)))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, i=0, v[16][0]
    (should (= #x6A09E667F2BDC948 (aref state 0)))

    ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, h[8]
    (setq state (blake-two-compress kind state msg (length "abc") t))
    (should (equal [#x0D4D1C983FA580BA #xE9F6129FB697276A #xB7C45A68142F214C
                    #xD1A2FFDB6FBB124B #x2D79AB2A39C5877D #x95CC3345DED552C2
                    #x5A92F1DBA88AD318 #x239900D4ED8623B9]
                   state))))

(ert-deftest blake-two-digest-sample ()
  ;; note: https://www.rfc-editor.org/rfc/rfc7693#appendix-A, BLAKE2b-512
  (should (equal [#xBA #x80 #xA5 #x3F #x98 #x1C #x4D #x0D
                  #x6A #x27 #x97 #xB6 #x9F #x12 #xF6 #xE9
                  #x4C #x21 #x2F #x14 #x68 #x5A #xC4 #xB7
                  #x4B #x12 #xBB #x6F #xDB #xFF #xA2 #xD1
                  #x7D #x87 #xC5 #x39 #x2A #xAB #x79 #x2D
                  #xC2 #x52 #xD5 #xDE #x45 #x33 #xCC #x95
                  #x18 #xD3 #x8A #xA8 #xDB #xF1 #x92 #x5A
                  #xB9 #x23 #x86 #xED #xD4 #x00 #x99 #x23]
                 (blake-two
                  blake-two-big "abc"
                  (alist-get blake-two-big blake-two-bits-in-word)))))

(provide 'blake-tests)
;;; blake-tests.el ends here
