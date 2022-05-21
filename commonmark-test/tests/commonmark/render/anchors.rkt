#lang racket/base

(require commonmark
         racket/match
         racket/string
         rackunit)

(define (md-fn-ids . strs)
  (match (document->xexprs (string->document (string-join strs "\n")))
    [(list _ (list 'section _ (list 'ol `(li ([id ,fn-ids]) ,_ ...) ...))) fn-ids]))

(parameterize ([current-parse-footnotes? #t])
  (check-equal? (md-fn-ids "[^1] [^x] [^boo]" "[^1]: 1st" "[^x]: 2nd" "[^boo]: 3rd")
                '("fn-1" "fn-x" "fn-boo")
                "Footnote anchors use fn-_ format by default")
  
  (parameterize ([current-anchor-proc (λ (s) (format "~a_000" s))])
    (check-equal? (md-fn-ids "[^1] [^x] [^boo]" "[^1]: 1st" "[^x]: 2nd" "[^boo]: 3rd")
                  '("fn-1_000" "fn-x_000" "fn-boo_000")
                  "Can change current-anchor-proc to control format of footnote anchors"))

  (parameterize ([current-anchor-proc (λ (s) 0)])
    (check-exn exn:fail:contract?
               (λ () (document->xexprs (string->document "[^1]\n[^1]: Not likely!")))
               "Exception at render time if current-anchor-proc is not (-> string? string?)")))
