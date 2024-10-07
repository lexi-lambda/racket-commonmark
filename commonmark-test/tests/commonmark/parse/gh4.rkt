#lang racket/base

;; Regression test for <https://github.com/lexi-lambda/racket-commonmark/issues/4>.

(require commonmark/struct
         rackunit
         "../test-util.rkt")

(check-equal? (md "[a link](http://example.com).</span>")
              (document
               (list
                (paragraph
                 (list (link "a link" "http://example.com" #f) "." (html "</span>"))))
               '()))

(check-equal? (md "[a link][1].</span>"
                  ""
                  "[1]: http://example.com")
              (document
               (list
                (paragraph
                 (list (link "a link" "http://example.com" #f) "." (html "</span>"))))
               '()))

(check-equal? (md "This <span>is _emphasis_ and [a link](http://example.com).</span>")
              (document
               (list
                (paragraph
                 (list "This " (html "<span>") "is " (italic "emphasis") " and " (link "a link" "http://example.com" #f) "." (html "</span>"))))
               '()))

(check-equal? (md "This <span>is _emphasis_ and [a link][1].</span>"
                  ""
                  "[1]: http://example.com")
              (document
               (list
                (paragraph
                 (list "This " (html "<span>") "is " (italic "emphasis") " and " (link "a link" "http://example.com" #f) "." (html "</span>"))))
               '()))

(check-equal? (md "This <span>is _emphasis_ and [a link](http://example.com).")
              (document
               (list
                (paragraph
                 (list "This " (html "<span>") "is " (italic "emphasis") " and " (link "a link" "http://example.com" #f) ".")))
               '()))

(check-equal? (md "This <span>is _emphasis_ and [a link](http://example.com).</div>")
              (document
               (list
                (paragraph
                 (list "This " (html "<span>") "is " (italic "emphasis") " and " (link "a link" "http://example.com" #f) "." (html "</div>"))))
               '()))
