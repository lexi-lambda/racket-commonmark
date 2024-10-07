#lang racket/base

;; Regression test for <https://github.com/lexi-lambda/racket-commonmark/issues/5>

(require commonmark/struct
         rackunit
         "../test-util.rkt")

(check-equal? (md "* outer"
                  "  * inner"
                  "")
              (document
               (list
                (itemization
                 (list
                  (list
                   (paragraph "outer")
                   (itemization (list (list (paragraph "inner"))) 'tight #f)))
                 'tight
                 #f))
               null))

(check-equal? (md "* outer"
                  "  * inner"
                  ""
                  "new para")
              (document
               (list
                (itemization
                 (list
                  (list
                   (paragraph "outer")
                   (itemization (list (list (paragraph "inner"))) 'tight #f)))
                 'tight
                 #f)
                (paragraph "new para"))
               null))

(check-equal? (md "* outer"
                  "  * inner"
                  ""
                  "> new blockquote")
              (document
               (list
                (itemization
                 (list
                  (list
                   (paragraph "outer")
                   (itemization (list (list (paragraph "inner"))) 'tight #f)))
                 'tight
                 #f)
                (blockquote (list (paragraph "new blockquote"))))
               null))

(check-equal? (md "* outer 1"
                  "  * middle"
                  "    * inner"
                  ""
                  "* outer 2")
              (document
               (list
                (itemization
                 (list
                  (list
                   (paragraph "outer 1")
                   (itemization
                    (list
                     (list
                      (paragraph "middle")
                      (itemization (list (list (paragraph "inner"))) 'tight #f)))
                    'tight
                    #f))
                  (list (paragraph "outer 2")))
                 'loose
                 #f))
               '()))
