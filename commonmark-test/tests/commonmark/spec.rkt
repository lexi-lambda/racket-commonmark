#lang racket/base

(require json
         net/uri-codec
         racket/format
         racket/list
         racket/match
         racket/runtime-path
         rackunit
         xml

         commonmark)

(define-runtime-path spec.json "spec-0.31.2.json")

(define color:reset #"\e(B\e[m")
(define color:bold #"\e[1m")
(define color:red #"\e[31m")
(define color:green #"\e[32m")
(define color:yellow #"\e[33m")
(define color:gray #"\e[90m")

; The spec examples use very particular HTML formatting (the precise details of
; which are not actually mandated by the spec), and normalizing the spec’s
; expected HTML would cause problems for examples involving raw “HTML” that is
; not actually valid HTML, so this function manually converts an xexpr to HTML
; using the spec’s formatting rules.
(define (document->spec-html doc)
  (define out (open-output-string))
  (define last-out-newline? #t)
  (define current-context (make-parameter 'block))

  (define (newline-out)
    (unless last-out-newline?
      (newline out)
      (set! last-out-newline? #t)))

  (define (write-out str)
    (write-string str out)
    (set! last-out-newline? #f))

  (define (oprintf . args)
    (apply fprintf out args)
    (set! last-out-newline? #f))

  (define (do-xexpr xexpr)
    (match xexpr
      [(? string?) (write-out (xml-attribute-encode xexpr))]
      [(? cdata?)
       (when (eq? (current-context) 'block)
         (newline-out))
       (write-out (cdata-string xexpr))
       (when (eq? (current-context) 'block)
         (newline-out))]
      [(list tag (list (list attr-names attr-vals) ...) xexprs ...)
       (do-element tag (map cons attr-names attr-vals) xexprs)]
      [(list tag xexprs ...)
       (do-element tag '() xexprs)]))

  (define (do-element tag attrs xexprs)
    (define add-newlines? (memq tag '(blockquote h1 h2 h3 h4 h5 h6 hr li ol p pre ul)))
    (when add-newlines? (newline-out))

    (oprintf "<~a" tag)
    (for ([attr (in-list attrs)])
      (define attr-val (if (eq? (car attr) 'href)
                           (encode-url (cdr attr))
                           (cdr attr)))
      (oprintf " ~a=\"~a\"" (car attr) (xml-attribute-encode attr-val)))
    (cond
      [(and (empty? xexprs) (memq tag html-empty-tags))
       (write-out " />")]
      [else
       (write-out ">")
       (when (eq? tag 'blockquote) (newline-out))
       (if (memq tag '(h1 h2 h3 h4 h5 h6 p))
           (parameterize ([current-context 'inline])
             (for-each do-xexpr xexprs))
           (for-each do-xexpr xexprs))
       (when (eq? tag 'blockquote) (newline-out))
       (oprintf "</~a>" tag)])

    (when (or add-newlines? (eq? tag 'br))
      (newline-out)))

  (for-each do-xexpr (document->xexprs doc))
  (get-output-string out))

(define (encode-url str)
  (regexp-replace* #px"[^a-zA-Z0-9;/?:@&=+$,\\-_.!~*'()#%]" str uri-encode))

(define (run-spec-tests #:print-summary? [print-summary? #f])
  (define all-specs (call-with-input-file* spec.json read-json #:mode 'text))

  (define spec-sections '())
  (define specs-per-section (make-hash))
  (define successes-per-section (make-hash))

  (for ([spec (in-list all-specs)])
    (define section (hash-ref spec 'section))
    (unless (hash-has-key? specs-per-section section)
      (set! spec-sections (cons section spec-sections))
      (hash-set! specs-per-section section 0)
      (hash-set! successes-per-section section 0))
    (hash-update! specs-per-section section add1)

    (with-check-info (['section section]
                      ['example (hash-ref spec 'example)]
                      ['markdown (hash-ref spec 'markdown)])
      (test-begin
       (check-equal? (document->spec-html (string->document (hash-ref spec 'markdown)))
                     (hash-ref spec 'html))
       (hash-update! successes-per-section section add1))))

  (when print-summary?
    (define section-title-width (apply max (map string-length spec-sections)))
    (define total-specs (apply + (hash-values specs-per-section)))
    (define totals-width (string-length (~a total-specs)))

    (define (write-chars c len)
      (for ([i (in-range len)])
        (write-char c)))

    (define (write-separator)
      (write-chars #\─ (+ section-title-width (* totals-width 2) 58))
      (newline))

    (define (write-bar-line label successes total)
      (write-string (~a label #:width section-title-width #:align 'right))
      (write-string " ")

      (define score (/ successes total))
      (define score-color (cond
                            [(= score 1) color:green]
                            [(>= score 7/10) color:yellow]
                            [else color:red]))

      (write-bytes score-color)
      (define filled-chars (round (* score 50)))
      (write-chars #\█ filled-chars)
      (write-chars #\░ (- 50 filled-chars))
      (write-string " ")
      (write-string (~r (* score 100) #:precision 0 #:min-width 3))
      (write-string "%")

      (write-bytes color:gray)
      (write-string (~a " " (~r successes #:min-width totals-width) "/" total))
      (write-bytes color:reset)
      (newline))

    (newline)
    (write-bytes color:bold)
    (write-string "CommonMark conformance summary")
    (write-bytes color:reset)
    (newline)
    (write-separator)
    
    (for ([section (in-list (reverse spec-sections))])
      (write-bar-line section
                      (hash-ref successes-per-section section)
                      (hash-ref specs-per-section section)))
    (write-separator)
    (write-bar-line "Total"
                    (apply + (hash-values successes-per-section))
                    total-specs)))

(module+ test (run-spec-tests))
(module+ main (run-spec-tests #:print-summary? #t))
