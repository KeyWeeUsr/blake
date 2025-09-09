# blake
[![CI][ci-badge]][ci-workflow]
[![Coverage Status][cover-badge]][cover-link]
[![Buy me a coffee][bmc-badge]][bmc-link]
[![Liberapay][lp-badge]][lp-link]
[![PayPal][ppl-badge]][ppl-link]

Blake implementation in ELisp.

Currently only `blake2s` (256-bit) and `blake2b` (512-bit).

## How to

Clone and install manually, then:

1. `(require 'blake)`
2. `(blake-two blake-two-big "abc")`

To get the same output as with `b2sum`, serialize the output:

```emacs-lisp
;; echo -n abc | b2sum
(string-join (mapcar (lambda (x) (format "%02x" x))
                     (blake-two blake-two-big "abc"))
             "")
```

Note that the implementation is obviously slower than with compiled languages
and, for example, running an `elisp-manual-21-2.8.tar.gz` (2455995 bytes) with
`blake-two-big` took astonishing 75s! (40s once byte-compiled) while with
coreutils' `b2sum` it took under one second.

```emacs-lisp
(with-temp-buffer
  (set-buffer-multibyte nil)
  (insert-file-contents-literally "elisp-manual-21-2.8.tar.gz")
  (message "hash: %s"
           (string-join (mapcar (lambda (x) (format "%02x" x))
                                (blake-two blake-two-big (buffer-string)))
                        "")))
```

While there might be performance bottlenecks in the current implementation, if
you are looking for speed, there are better and safer implementations.

[bmc-badge]: https://img.shields.io/badge/-buy_me_a%C2%A0coffee-gray?logo=buy-me-a-coffee
[bmc-link]: https://www.buymeacoffee.com/peterbadida
[ppl-badge]: https://img.shields.io/badge/-paypal-grey?logo=paypal
[ppl-link]: https://paypal.me/peterbadida
[lp-badge]: https://img.shields.io/badge/-liberapay-grey?logo=liberapay
[lp-link]: https://liberapay.com/keyweeusr
[ci-badge]: https://github.com/KeyWeeUsr/blake/actions/workflows/test.yml/badge.svg
[ci-workflow]: https://github.com/KeyWeeUsr/blake/actions/workflows/test.yml
[cover-badge]: https://coveralls.io/repos/github/KeyWeeUsr/blake/badge.svg?branch=master
[cover-link]: https://coveralls.io/github/KeyWeeUsr/blake?branch=master
