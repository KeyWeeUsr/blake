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

(defconst blake-two-bits-in-word '((blake-two-big . 64) (blake-two-small . 32)))
(defconst blake-two-rounds '((blake-two-big . 12) (blake-two-small . 10)))
(defconst blake-two-block-size '((blake-two-big . 128) (blake-two-small . 64)))
(defconst blake-two-hash-size-limits
  '((blake-two-big . ((upper . 64) (lower . 1)))
    (blake-two-small . ((upper . 32) (lower . 1)))))
(defconst blake-two-key-size-limits
  '((blake-two-big . ((upper . 64) (lower . 1)))
    (blake-two-small . ((upper . 32) (lower . 1)))))
(defconst blake-two-input-size-limits
  `((blake-two-big . ((upper . ,(expt 2 128)) (lower . 0)))
    (blake-two-small . ((upper . ,(expt 2 64)) (lower . 0)))))
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


(provide 'blake)
;;; blake.el ends here
