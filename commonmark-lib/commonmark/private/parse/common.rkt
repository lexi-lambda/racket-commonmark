#lang racket/base

(require (for-syntax racket/base)
         racket/match
         racket/string
         syntax/parse/define
         threading
         "entity.rkt"
         "../regexp.rkt")

(provide (for-syntax :newline :ascii-punctuation :ascii-control:
                     :space: :space* :space+
                     :html-open-close)

         unicode-whitespace?
         ascii-control?
         ascii-punctuation?
         unicode-punctuation?

         try-read-entity
         decode-backslash-escapes
         decode-entities+escapes

         normalize-link-label
         try-peek-link-label
         try-peek-link-destination
         try-peek-link-title

         match-and
         match-and*)

;; -----------------------------------------------------------------------------
;; § 2.1 Characters and lines

(begin-for-syntax
  ; <https://spec.commonmark.org/0.30/#line-ending>
  (define :newline (:: "\n|\r\n?"))
  ; <https://spec.commonmark.org/0.30/#ascii-punctuation-character>
  (define :ascii-punctuation "[!-/:-@[-`{-~]")
  ; <https://spec.commonmark.org/0.30/#ascii-control-character>
  (define :ascii-control: "\0-\x1F\x7F")

  ; Often referred to in the spec using the phrase “spaces, tabs, and up to one
  ; line ending”. The “one line ending” part is enforced by the structure of the
  ; block parser, so we don’t have to worry about it explicitly.
  (define :space: " \t\r\n")
  (define :space* (:* "[" :space: "]"))
  (define :space+ (:+ "[" :space: "]"))

  ; <https://spec.commonmark.org/0.30/#link-destination>
  (define :link-destination
    (:or "<([^\r\n]*?)(?<!\\\\)>"
         (:group "[^< " :ascii-control: "]")))

  ;; § 6.5 Raw HTML <https://spec.commonmark.org/0.30/#raw-html>
  (define (:html-open-close #:allow-newlines? allow-newlines?)
    (define-values [:sp: :sp* :sp+ :nl:]
      (if allow-newlines?
          (values :space: :space* :space+ "")
          (values " \t" "[ \t]*" "[ \t]+" "\r\n")))

    (define :tag-name (:: "[a-zA-Z][a-zA-Z0-9-]*"))
    (define :attribute-name (:: "[a-zA-Z_:][a-zA-Z0-9_.:-]*"))
    (define :attribute-value (:or (:: "[^" :sp: :nl: "\"'=<>`]+")
                                  (:: "'[^" :nl: "']*'")
                                  (:: "\"[^" :nl: "\"]*\"")))
    (define :attribute (:: :sp+ :attribute-name (:? :sp* "=" :sp* :attribute-value)))

    (define :open-tag (:: "<" :tag-name (:* :attribute) :sp* "/?" ">"))
    (define :close-tag (:: "</" :tag-name :sp* ">"))

    (:or :open-tag :close-tag)))

(define (unicode-whitespace? c)
  (or (eq? (char-general-category c) 'zs)
      (member c '(#\tab #\newline #\page #\return))))

(define (ascii-control? c)
  (or (char<? c #\space)
      (char=? c #\rubout)))

(define (ascii-punctuation? c)
  (or (char<=? #\! c #\/)
      (char<=? #\: c #\@)
      (char<=? #\[ c #\`)
      (char<=? #\{ c #\~)))

(define (unicode-punctuation? c)
  (or (ascii-punctuation? c)
      (memq (char-general-category c) '(pc pd pe pf pi po ps))))

;; -----------------------------------------------------------------------------

;; § 2.4 Backslash escapes
(define (decode-backslash-escapes str)
  (regexp-replace* (px "\\\\(" :ascii-punctuation ")") str "\\1"))

(define (decode-entities+escapes str)
  (decode-entities (decode-backslash-escapes str)))

(define (try-peek-link-label in [start-pos 0])
  (match (regexp-match-peek (px "^" "\\["
                                (:group (:+ (:or "\\\\[[\\]]"
                                                 "[^[\\]]")
                                            #:max 999))
                                "\\]")
                            in start-pos)
    [(list peeked-bytes (app bytes->string/utf-8 label-str))
     ; From <https://spec.commonmark.org/0.30/#link-label>:
     ;   “Between these brackets there must be at least one character that is
     ;    not a space, tab, or line ending.”
     #:when (regexp-match? (px "[^" :space: "]") label-str)
     (list (+ start-pos (bytes-length peeked-bytes))
           (normalize-link-label label-str))]
    [_ #f]))

;; <https://spec.commonmark.org/0.30/#matches>
(define (normalize-link-label str)
  (~> (string-foldcase str)
      (string-trim (px :space+))
      (string-replace (px :space+) " ")))

;; <https://spec.commonmark.org/0.30/#link-destination>
(define (try-peek-link-destination in [start-pos 0])
  (match (regexp-match-peek #px"<([^\r\n]*?)(?<!\\\\)>" in start-pos)
    ; First, the simple case: a destination enclosed in <angle brackets>.
    [(list peeked-bytes dest-bytes)
     (list (+ start-pos (bytes-length peeked-bytes))
           (decode-entities+escapes (bytes->string/utf-8 dest-bytes)))]
    [_
     ; The complicated case: a destination not enclosed in <angle brackets>.
     (match (peek-char in start-pos)
       ; Start by checking for illegal characters, since if the first character
       ; is illegal, then we’ve necessarily failed.
       [(or (? eof-object?) #\< #\) (? ascii-control?) #\space) #f]
       [_
        ; Now we have to iterate, keeping track of our peeking position and
        ; how many unbalanced open parentheses we’ve seen.
        (let loop ([pos start-pos]
                   [paren-depth 0])
          ; First, we skip over any definitely-allowed characters.
          (match-define (list (cons _ pos*))
            (regexp-match-peek-positions
             (px "^" (:* (:or "\\\\[()]" ; escaped parens are always allowed
                              "[^[:cntrl:]\x7f ()]")))
             in pos))
          ; Now we look at what character we stopped skipping on.
          (match (peek-char in pos*)
            ; An open paren is always allowed, we just increment the paren depth.
            [#\( (loop (add1 pos*) (add1 paren-depth))]
            ; If we hit a close paren with a non-zero paren depth, we decrement
            ; the paren depth and keep going.
            [#\) #:when (> paren-depth 0)
                 (loop (add1 pos*) (sub1 paren-depth))]
            [_
             (cond
               ; If we hit anything else with a non-zero paren depth, we’ve failed.
               [(> paren-depth 0) #f]
               [else
                ; Otherwise, we’ve succeeded, so peek the actual bytes out and return.
                (define peeked-bytes (peek-bytes (- pos* start-pos) start-pos in))
                (list pos*
                      (decode-entities+escapes (bytes->string/utf-8 peeked-bytes)))])]))])]))

(define (try-peek-link-title in [start-pos 0])
  ; This is very similar to `try-peek-link-destination` above.
  (match (regexp-match-peek
          (px "^" "([\"'])" ; open delimiter
              "(.*?)" ; title content
              "(?<!\\\\)\\1") ; close delimiter
          in start-pos)
    ; We start with the (relatively) simple case that can be handled by regexp.
    [(list peeked-bytes _ title-bytes)
     (list (+ start-pos (bytes-length peeked-bytes))
           (decode-entities+escapes (bytes->string/utf-8 title-bytes)))]
    [_
     (match (peek-char in start-pos)
       ; Start by checking for a required open paren.
       [#\(
        (let loop ([pos (add1 start-pos)]
                   [paren-depth 0])
          ; First, we skip over any definitely-allowed characters.
          (match-define (list (cons _ pos*))
            (regexp-match-peek-positions
             (px "^" (:* (:or "\\\\[()]" ; escaped parens are always allowed
                              "[^()]")))
             in pos))
          ; Now we look at what character we stopped skipping on.
          (match (peek-char in pos*)
            ; An open paren is always allowed, we just increment the paren depth.
            [#\( (loop (add1 pos*) (add1 paren-depth))]
            [#\) (cond
                   ; If we hit a close paren with a non-zero paren depth, we
                   ; decrement the paren depth and keep going.
                   [(> paren-depth 0)
                    (loop (add1 pos*) (sub1 paren-depth))]
                   [else
                    ; Otherwise, we’ve succeeded, so peek the actual bytes out and return.
                    (define peeked-bytes (peek-bytes (- pos* start-pos 1) (add1 start-pos) in))
                    (list (add1 pos*)
                          (decode-entities+escapes (bytes->string/utf-8 peeked-bytes)))])]
            ; If we hit anything else, we’ve failed.
            [_ #f]))]
       [_ #f])]))

;; -----------------------------------------------------------------------------
;; match-and[*]

(define-syntax-parser match-and
  [(_ scrutinee:expr clause ...)
   #`(match/derived scrutinee #,this-syntax [#f #f] clause ...)])

;; A little helper macro to make it easier to write simple backtracking parsers.
;; Uses should have the following syntax:
;;
;;     (match-and*
;;       [scrut-expr => pat body ...] ...
;;       [scrut-expr => pat body ...+])
;;
;; The clauses are evaluated top-down. Each `scrut-expr` is evaluated, and if it
;; is #f, the entire `match-and*` form evaluates to #f. Otherwise, the result is
;; matched against `pat`, and the resulting bindings scope over both the
;; clause’s body and all subsequent clauses. If all `scrut-expr` clauses evaluate
;; to non-#f values, the result of the `match-and*` form is the result of
;; evaluating the final clause’s body.
;;
;; In practice, each `scrut-expr` is an expression that peeks into an input port
;; and returns #f upon failure. A backtracking point can be expressed using `or`:
;;
;;     (match-and*
;;       ....
;;       [scrut-expr => pat
;;        (or (match-and* ....)
;;            (match-and* ....))])
;;
;; A bit crude, but good enough for our minimal needs.
(define-syntax-parser match-and*
  #:literals [=>]
  #:track-literals
  [(_ [scrutinee:expr => pat body ...+])
   (syntax/loc this-syntax
     (match-and scrutinee [pat body ...]))]
  [(_ [scrutinee:expr => pat body ...] more ...+)
   (quasisyntax/loc this-syntax
     (match-and scrutinee
       [pat body ... #,(syntax/loc this-syntax
                         (match-and* more ...))]))])
