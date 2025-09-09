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

(provide 'blake)
;;; blake.el ends here
