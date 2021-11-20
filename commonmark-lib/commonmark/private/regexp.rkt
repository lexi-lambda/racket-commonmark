#lang racket/base

;; This module provides an extremely simple interface for assembling regular
;; expressions from smaller parts. Importantly, the assembling is done at
;; compile-time, so the resulting regular expressions will be efficiently
;; compiled the same way ordinary regular expression literals are.

(require (for-syntax racket/base
                     racket/contract
                     racket/format
                     racket/string
                     racket/syntax)
         syntax/parse/define)

(provide px)

(define-syntax-parser px
  [(_ {~var str-e (expr/c #'string? #:phase (add1 (syntax-local-phase-level)))} ...)
   (datum->syntax #'here
                  (pregexp (string-append* (map syntax-local-eval (attribute str-e.c)))
                           (λ (message) (raise-syntax-error #f message this-syntax)))
                  this-syntax)])

(begin-for-syntax
  (provide (contract-out
            [:: (-> string? ... string?)]
            [:ci (-> string? ... string?)]
            [:group (-> string? ... string?)]
            [:or (-> string? ... string?)]

            [:ahead (-> string? ... string?)]
            [:not-ahead (-> string? ... string?)]
            [:behind (-> string? ... string?)]
            [:not-behind (-> string? ... string?)]

            [:? (->* [] [#:greedy? any/c] #:rest (listof string?) string?)]
            [:* (->* [] [#:min exact-nonnegative-integer?
                         #:max (or/c exact-nonnegative-integer? #f)
                         #:greedy? any/c]
                     #:rest (listof string?)
                     string?)]
            [:+ (->* [] [#:max (or/c exact-nonnegative-integer? #f)
                         #:greedy? any/c]
                     #:rest (listof string?)
                     string?)]

            [:?? (-> string? ... string?)]
            [:*? (->* [] [#:min exact-nonnegative-integer?
                          #:max (or/c exact-nonnegative-integer? #f)]
                      #:rest (listof string?)
                      string?)]
            [:+? (->* [] [#:max (or/c exact-nonnegative-integer? #f)]
                      #:rest (listof string?)
                      string?)]))

  (define (:: . strs) (~a "(?:" (string-append* strs) ")"))
  (define (:ci . strs) (~a "(?i:" (string-append* strs) ")"))
  (define (:group . strs) (~a "(" (string-append* strs) ")"))
  (define (:or . strs) (:: (string-join (map :: strs) "|")))

  (define (:ahead . strs) (~a "(?=" (string-append* strs) ")"))
  (define (:not-ahead . strs) (~a "(?!" (string-append* strs) ")"))
  (define (:behind . strs) (~a "(?<=" (string-append* strs) ")"))
  (define (:not-behind . strs) (~a "(?<!" (string-append* strs) ")"))

  (define (:* #:min [min-count 0]
              #:max [max-count #f]
              #:greedy? [greedy? #t]
              . strs)
    (~a (apply :: strs)
        (cond
          [(and (= min-count 0) (not max-count)) "*"]
          [(and (= min-count 1) (not max-count)) "+"]
          [(and (= min-count 0) (= max-count 1)) "?"]
          [else (~a "{" min-count "," (or max-count "") "}")])
        (if greedy? "" "?")))

  (define (:? #:greedy? [greedy? #t] . strs)
    (apply :* #:min 0 #:max 1 #:greedy? greedy? strs))
  (define (:+ #:max [max-count #f] #:greedy? [greedy? #t] . strs)
    (apply :* #:min 1 #:max max-count #:greedy? greedy? strs))

  (define (:?? . strs)
    (apply :? #:greedy? #f strs))
  (define (:*? #:min [min-count 0] #:max [max-count #f] . strs)
    (apply :* #:min min-count #:max max-count #:greedy? #f strs))
  (define (:+? #:max [max-count #f] . strs)
    (apply :+ #:max max-count #:greedy? #f strs)))
