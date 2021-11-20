#lang racket/base

(require (for-syntax racket/base
                     racket/match
                     racket/runtime-path
                     json)
         racket/match
         syntax/parse/define
         "../regexp.rkt")

(provide decode-entities
         try-read-entity)

;; -----------------------------------------------------------------------------

(begin-for-syntax
  ;; The authoritative list of HTML entity names, retrieved from
  ;;   <https://html.spec.whatwg.org/entities.json>.
  (define-runtime-path entities.json "entities.json"))

(define-syntax-parser define-entity-names!
  [(_ entity-names:id entity-name-px:id)
   (define entities
     (for/fold ([entities (hash)])
               ([(k v) (in-hash (call-with-input-file* entities.json read-json #:mode 'text))])
       (match (regexp-match #px"^&(.+);$" (symbol->string k))
         [#f entities]
         [(list _ name)
          (hash-set entities name (hash-ref v 'characters))])))
   #`(begin
       (define entity-names '#,entities)
       (define-for-syntax entity-name-px #,(apply :or (map regexp-quote (hash-keys entities)))))])

(define-entity-names! entity-names :entity-name)
(define-for-syntax :entity (:: "&"
                               (:or (:group :entity-name)
                                    "#([0-9]{1,7})"
                                    "#[xX]([0-9a-fA-F]{1,6})")
                               ";"))

(define (entity-integer->string n)
  (if (or (<= 1 n #xD7FF)
          (<= #xE000 n #x10FFFF))
      (string (integer->char n))
      "\uFFFD"))

(define (decode-entities str)
  (regexp-replace*
   (px :entity) str
   (match-lambda**
     [(_ (? values name) _ _)
      (hash-ref entity-names name)]
     [(_ _ (? values decimal-number) _)
      (entity-integer->string (string->number decimal-number))]
     [(_ _ _ (? values hex-number))
      (entity-integer->string (string->number hex-number 16))])))

(define (try-read-entity in)
  (match (regexp-try-match (px "^" :entity) in)
    [#f #f]
    [(list _ (? values name) _ _)
     (hash-ref entity-names (bytes->string/utf-8 name))]
    [(list _ _ (? values decimal-number) _)
     (entity-integer->string (string->number (bytes->string/utf-8 decimal-number)))]
    [(list _ _ _ (? values hex-number))
     (entity-integer->string (string->number (bytes->string/utf-8 hex-number) 16))]))
