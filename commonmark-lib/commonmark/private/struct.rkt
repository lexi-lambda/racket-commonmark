#lang racket/base

(provide (struct-out document)
         (struct-out footnote-definition)

         block?
         thematic-break thematic-break?
         (struct-out heading)
         (struct-out code-block)
         (struct-out html-block)
         (struct-out paragraph)
         (struct-out blockquote)
         (struct-out itemization)

         inline?
         line-break line-break?
         (struct-out italic)
         (struct-out bold)
         (struct-out code)
         (struct-out link)
         (struct-out image)
         (struct-out html)
         (struct-out footnote-reference))

;; -----------------------------------------------------------------------------

(struct document (blocks footnotes) #:transparent)
(struct footnote-definition (blocks label) #:transparent)

(define (block? v)
  (or (thematic-break? v)
      (heading? v)
      (code-block? v)
      (html-block? v)
      (paragraph? v)
      (blockquote? v)
      (itemization? v)))

(define-values [thematic-break thematic-break?]
  (let ()
    (struct thematic-break () #:authentic)
    (values (thematic-break) thematic-break?)))
(struct heading (content depth) #:transparent)
(struct code-block (content info-string) #:transparent)
(struct html-block (content) #:transparent)
(struct paragraph (content) #:transparent)
(struct blockquote (blocks) #:transparent)
(struct itemization (blockss style start-num) #:transparent)

(define (inline? v)
  (or (string? v)
      (and (list? v) (andmap inline? v))
      (line-break? v)
      (italic? v)
      (bold? v)
      (code? v)
      (link? v)
      (image? v)
      (html? v)
      (footnote-reference? v)))

(define-values [line-break line-break?]
  (let ()
    (struct line-break () #:authentic)
    (values (line-break) line-break?)))
(struct italic (content) #:transparent)
(struct bold (content) #:transparent)
(struct code (content) #:transparent)
(struct link (content dest title) #:transparent)
(struct image (description source title) #:transparent)
(struct html (content) #:transparent)
(struct footnote-reference (label) #:transparent)
