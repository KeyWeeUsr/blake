;;; blake.el --- Blake implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: tools
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/KeyWeeUsr/blake

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TBD

;;; Code:

(defconst blake-two-big 'blake-two-big)
(defconst blake-two-small 'blake-two-small)
(defconst blake-two-kinds '(blake-two-big blake-two-small))

(defconst blake-two-byte 8)
(defconst blake-two-bits-in-word
  '((blake-two-big . 64)
    (blake-two-small . 32)))
(defconst blake-two-rounds
  '((blake-two-big . 12)
    (blake-two-small . 10)))
(defconst blake-two-block-size
  `((blake-two-big
     . ,(* 2 (alist-get blake-two-big blake-two-bits-in-word)))
    (blake-two-small
     . ,(* 2 (alist-get blake-two-small blake-two-bits-in-word)))))
(defconst blake-two-hash-size-limits
  `((blake-two-big
     . ((upper . ,(alist-get blake-two-big blake-two-bits-in-word))
        (lower . 1)))
    (blake-two-small
     . ((upper . ,(alist-get blake-two-small blake-two-bits-in-word))
        (lower . 1)))))
(defconst blake-two-key-size-limits
  `((blake-two-big
     . ((upper . ,(alist-get blake-two-big blake-two-bits-in-word))
        (lower . 1)))
    (blake-two-small
     . ((upper . ,(alist-get blake-two-small blake-two-bits-in-word))
        (lower . 1)))))
(defconst blake-two-input-size-limits
  `((blake-two-big
     . ((upper
         . ,(expt 2 (alist-get blake-two-big blake-two-block-size)))
        (lower . 0)))
    (blake-two-small
     . ((upper
         . ,(expt 2 (alist-get blake-two-small blake-two-block-size)))
        (lower . 0)))))
(defconst blake-two-rotconst
  '((blake-two-big . (32 24 16 63))
    (blake-two-small . (16 12 8 7))))
(defconst blake-two-iv
  '((blake-two-big . [#x6A09E667F3BCC908 #xBB67AE8584CAA73B
                      #x3C6EF372FE94F82B #xA54FF53A5F1D36F1
                      #x510E527FADE682D1 #x9B05688C2B3E6C1F
                      #x1F83D9ABFB41BD6B #x5BE0CD19137E2179])
    (blake-two-small . [#x6A09E667 #xBB67AE85 #x3C6EF372 #xA54FF53A
                        #x510E527F #x9B05688C #x1F83D9AB #x5BE0CD19])))
(defconst blake-two-schedule-small
  [[  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 ]
   [ 14 10  4  8  9 15 13  6  1 12  0  2 11  7  5  3 ]
   [ 11  8 12  0  5  2 15 13 10 14  3  6  7  1  9  4 ]
   [  7  9  3  1 13 12 11 14  2  6  5 10  4  0 15  8 ]
   [  9  0  5  7  2  4 10 15 14  1 11 12  6  8  3 13 ]
   [  2 12  6 10  0 11  8  3  4 13  7  5 15 14  1  9 ]
   [ 12  5  1 15 14 13  4 10  0  7  6  3  9  2  8 11 ]
   [ 13 11  7 14 12  1  3  9  5  0 15  4  8  6  2 10 ]
   [  6 15 14  9 11  3  0  8 12  2 13  7  1  4 10  5 ]
   [ 10  2  8  4  7  6  1  5 15 11  9 14  3 12 13  0 ]]
  "Message word schedule permutations for each round are defined by SIGMA.")

(defconst blake-two-schedule-big
  `[,@blake-two-schedule-small
    ,(aref blake-two-schedule-small 0)
    ,(aref blake-two-schedule-small 1)]
  "Message word schedule permutations for each round are defined by SIGMA.
For BLAKE2b, the two extra permutations forrounds 10 and 11 are
SIGMA[10..11] = SIGMA[0..1].")

(defconst blake-two-schedule
  `((blake-two-big . ,blake-two-schedule-big)
    (blake-two-small . ,blake-two-schedule-small)))

(defconst blake-two-local-size 16
  "Vector size of 32-bit or 64-bit word local vector for calculating.")

(defconst blake-two-state-size 8
  "Vector size of initial state for compressing.")

(defconst blake-two-msg-size 16
  "Vector size of message block for compressing.")

(defconst blake-two-counter-size
  `((blake-two-big
     . ,(alist-get blake-two-big blake-two-block-size))
    (blake-two-small
     . ,(alist-get blake-two-small blake-two-block-size))))

(defun blake-rotate (v n &optional size)
  "Left (N>0) or right (N<0) rotate the *unsigned* SIZE-bit value V N-times.
From Elchacha (`elchacha-rotate')."
  (unless size (setq size 32))
  (let ((n (mod (+ n size) size)))
    (if (= n 0) v
      (logand (logior (ash v n) (ash v (* -1 (- size n))))
              (1- (expt 2 size))))))

(defun blake-two-mix (kind v a b c d x y)
  "Mixing function G from RFC7693.
Ref: https://www.rfc-editor.org/rfc/rfc7693#section-3.1
Argument KIND One of `blake-two-kinds'.
Argument V Working 16-element vector.
Arguments A, B, C, D are used as V index when xoring.
Arguments X, Y are input words for xoring and rotating."
  (unless (member kind blake-two-kinds)
    (error "Invalid kind %S" kind))

  (unless (= blake-two-local-size (length v))
    (error "Invalid V length"))

  (let* ((rotconst (alist-get kind blake-two-rotconst))
         (word (alist-get kind blake-two-bits-in-word))
         (word-max (1- (expt 2 (alist-get kind blake-two-bits-in-word)))))

    (aset v a (logand (+ (aref v a) (aref v b) x)
                      word-max))

    ;; note: watch out for v[12] and counter addition when using manually
    (aset v d (blake-rotate (logxor (aref v d) (aref v a))
                            (* -1 (nth 0 rotconst))
                            word))
    (aset v c (logand (+ (aref v c) (aref v d))
                      word-max))
    (aset v b (blake-rotate (logxor (aref v b) (aref v c))
                            (* -1 (nth 1 rotconst))
                            word))
    (aset v a (logand (+ (aref v a) (aref v b) y)
                      word-max))
    (aset v d (blake-rotate (logxor (aref v d) (aref v a))
                            (* -1 (nth 2 rotconst))
                            word))
    (aset v c (logand (+ (aref v c) (aref v d))
                      word-max))
    (aset v b (blake-rotate (logxor (aref v b) (aref v c))
                            (* -1 (nth 3 rotconst))
                            word))

    v))

(defun blake-two-compress (kind state msg counter &optional final)
  "Compressing function F.
Argument KIND One of `blake-two-kinds'.
Argument STATE 8-element state vector.
Argument MSG 16-element message vector.
Argument COUNTER is double-word-bit counter split into word-parts for xoring.
Optional argument FINAL is a flag marking the final round."
  (unless (member kind blake-two-kinds)
    (error "Invalid kind %S" kind))

  (unless (= blake-two-state-size (length state))
    (error "Invalid state lenght"))

  (unless (= blake-two-msg-size (length msg))
    (error "Invalid msg lenght"))

  (unless (booleanp final)
    (error "Invalid flag type"))

  (let ((ctr-size (alist-get kind blake-two-counter-size))
        (local (make-vector blake-two-local-size 0))
        ;; copies because of re-use in defconst
        (schedule (vconcat (alist-get kind blake-two-schedule)))
        (iv (vconcat (alist-get kind blake-two-iv))))
    (unless (= ctr-size (length counter))
      (error "Invalid counter lenght"))

    (dotimes (idx (length state))
      (aset local idx (aref state idx)))
    (dotimes (idx (length iv))
      (aset local (+ 8 idx) (aref iv idx)))

    (aset local 12 (logxor (aref local 12) (mod counter (expt 2 ctr-size))))
    (aset local 13
          (logxor (aref local 13)
                  ;; right-shift
                  (lsh counter
                       (* -1 (alist-get kind blake-two-bits-in-word)))))

    (when final
      (aset local 14
            (logxor (aref local 14)
                    ;; invert all bits
                    (1- (expt 2 (alist-get kind blake-two-bits-in-word))))))

    (dotimes (idx (alist-get kind blake-two-rounds))
      (let ((s (aref schedule (mod idx 10))))
        (setq local (blake-two-mix kind local  0 4  8 12
                                   (aref msg (aref schedule 0))
                                   (aref msg (aref schedule 1))))
        (setq local (blake-two-mix kind local  1 5  9 13
                                   (aref msg (aref schedule 2))
                                   (aref msg (aref schedule 3))))
        (setq local (blake-two-mix kind local  2 6 10 14
                                   (aref msg (aref schedule 4))
                                   (aref msg (aref schedule 5))))
        (setq local (blake-two-mix kind local  3 7 11 15
                                   (aref msg (aref schedule 6))
                                   (aref msg (aref schedule 7))))
        (setq local (blake-two-mix kind local  0 5 10 15
                                   (aref msg (aref schedule 8))
                                   (aref msg (aref schedule 9))))
        (setq local (blake-two-mix kind local  1 6 11 12
                                   (aref msg (aref schedule 10))
                                   (aref msg (aref schedule 11))))
        (setq local (blake-two-mix kind local  2 7  8 13
                                   (aref msg (aref schedule 12))
                                   (aref msg (aref schedule 13))))
        (setq local (blake-two-mix kind local  3 4  9 14
                                   (aref msg (aref schedule 14))
                                   (aref msg (aref schedule 15))))))

    (dotimes (idx blake-two-state-size)
      (aset state idx (logxor (aref state idx)
                              (aref local idx)
                              (aref local (+ 8 idx)))))

    state))

(defun blake-two-chunk-data (raw-data)
  "Split RAW-DATA flat array of bytes into BLAKE workable blocks."
  (let* ((data-len (length raw-data))
         (data (make-vector (ceiling (/ (float data-len) blake-two-msg-size))
                            nil))
         (idx 0))
    (unless (> data-len 0) (error "Empty data"))

    (while (< idx data-len)
      (let ((chunk (/ idx blake-two-msg-size))
            (chunk-pos (mod idx blake-two-msg-size)))
        (when (= 0 chunk-pos)
          (aset data chunk (make-vector blake-two-msg-size 0)))

        (aset (aref data chunk) chunk-pos (aref raw-data idx))
        (setq idx (1+ idx))))
    data))

(defun blake-two-init-state-zero (kind state key first-bytes)
  "Initialize zero-th place in the state.
Argument KIND One of `blake-two-kinds'.
Argument STATE 8-element state vector.
Argument KEY is a secret key making the func output a keyed hash.
Argument FIRST-BYTES cuts the output to the first N bytes."
  (logxor (aref state 0)
          #x01010000
          (logand (lsh key (/ (alist-get kind blake-two-bits-in-word)
                              blake-two-byte))
                  (1- (expt 2 (alist-get kind blake-two-bits-in-word))))
          first-bytes))

(defun blake-two (kind raw-data first-bytes &optional key)
  "BLAKE2 hashing func.
Argument KIND One of `blake-two-kinds'.
Argument RAW-DATA is string to be hashed.
Argument FIRST-BYTES cuts the output to the first N bytes.
Optional argument KEY is a secret key making the func output a keyed hash."
  (unless (member kind blake-two-kinds)
    (error "Invalid kind %S" kind))

  (let* ((data (blake-two-chunk-data raw-data))
         (state (alist-get kind blake-two-iv))
         (data-len (length data)))
    ;; parameter block p[0]
    (aset state 0
          (logxor (aref state 0)
                  #x01010000
                  (logand (lsh key 8)
                          (1- (expt 2 (alist-get kind blake-two-bits-in-word))))
                  first-bytes))

    ;; Process padded key and data blocks
    (when (> data-len 1)
      ;; todo: possibly 2->1 because inclusive for in pseudo
      (dotimes (idx (- data-len 2))
        (setq state (blake-two-compress
                     kind state
                     (aref data idx) (* (1+ idx) blake-two-msg-size)))))

    ;; Final block
    (if (= 0 (length key))
        (setq state (blake-two-compress kind state
                                        (aref data (1- data-len))
                                        (length raw-data)
                                        t))
      (setq state (blake-two-compress kind state
                                      (aref data (1- data-len))
                                      (+ (length raw-data)
                                         blake-two-msg-size)
                                      t)))
    ;; todo: RETURN first "nn" bytes from little-endian word array h[].
    state))

(provide 'blake)
;;; blake.el ends here
