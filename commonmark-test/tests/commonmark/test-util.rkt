#lang racket/base

(require commonmark
         racket/string)

(provide md md*)

(define (md . strs)
  (string->document (string-join strs "\n" #:after-last "\n")))

(define (md* . strs)
  (define doc (apply md strs))
  (list doc (document->xexprs doc)))
