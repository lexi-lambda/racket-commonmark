#lang racket/base

;; This module implements a simple Markdown-to-Scribble renderer for the
;; purposes of rendering examples in the commonmark library documentation.
;;
;; It is *not* a general-purpose solution for converting Markdown to Scribble,
;; primarily because it explicitly avoids converting Markdown headers to
;; Scribble `part-start` declarations, which would pollute the structure of the
;; manual itself and would make it impossible to display rendered headings
;; inside a nested flow.

(require commonmark/parse
         commonmark/private/render
         racket/class
         racket/contract
         racket/format
         racket/list
         racket/match
         racket/path
         racket/runtime-path
         setup/main-collects
         scribble/core
         scribble/html-properties
         scribble/manual
         threading
         (only-in xml cdata))

(provide (contract-out
          [markdown-block (-> string? ... block?)]
          [markdown-example (-> string? ... block?)]))

;; -----------------------------------------------------------------------------

(define-runtime-path commonmark.css "commonmark.css")
(define commonmark-css-addition (~> (simple-form-path commonmark.css)
                                    path->main-collects-relative
                                    css-addition))

(define horizontal-rule-style (style #f (list (alt-tag "hr"))))
(define (horizontal-rule)
  (paragraph horizontal-rule-style '()))

(define inset-style (style 'inset '()))
(define ordered-style (style 'ordered '()))
(define div-flow-style (style #f (list (alt-tag "div"))))
(define footnotes-style (style "CmMdNotes" (list commonmark-css-addition (alt-tag "section"))))
(define markdown-doc-style (style "CmMdDoc" (list commonmark-css-addition (alt-tag "div"))))
(define example-style (style "CmExample" (list commonmark-css-addition (alt-tag "div"))))
(define example-content-style (style "CmExampleContent" (list commonmark-css-addition (alt-tag "div"))))
(define example-title-left-style (style "CmExampleTitleLeft" (list commonmark-css-addition)))
(define example-title-right-style (style "CmExampleTitleRight" (list commonmark-css-addition)))
(define example-title-text-style (style "CmExampleTitleText" (list commonmark-css-addition)))

(define (markdown-block . strs)
  (nested #:style 'code-inset (document->scribble (string->document (apply string-append strs)))))

(define (markdown-example . strs)
  (define str (apply string-append strs))
  (nested-flow
   example-style
   (list (nested-flow
          div-flow-style
          (list (paragraph example-title-left-style (list (element example-title-text-style (tt "markdown"))))
                (nested-flow example-content-style (list (verbatim str)))))
         (nested-flow
          div-flow-style
          (list (paragraph example-title-right-style (list (element example-title-text-style (tt "rendered"))))
                (nested-flow example-content-style (list (document->scribble (string->document str)))))))))

(define (document->scribble doc #:who [who 'document->scribble])
  (send (new scribble-render% [doc doc] [who who]) render-document))

(define scribble-render%
  (class abstract-render%
    (define/override (render-document)
      (define-values [body footnotes] (super render-document))
      (nested-flow
       markdown-doc-style
       (if (empty? footnotes)
           body
           (append body
                   (list (nested-flow footnotes-style (list (itemization ordered-style footnotes))))))))

    (define/override (render-thematic-break)
      (horizontal-rule))
    (define/override (render-heading content depth)
      (define tag (vector-ref #("h1" "h2" "h3" "h4" "h5" "h6") (sub1 depth)))
      (paragraph (style #f (list (alt-tag tag))) content))
    (define/override (render-code-block content language)
      (match language
        ["racket" (typeset-code #:keep-lang-line? #f "#lang racket\n" content)]
        [_        (nested #:style 'code-inset (verbatim content))]))
    (define/override (render-html-block content)
      (paragraph (style #f '(div omitable)) (render-html content)))
    (define/override (render-paragraph content)
      (paragraph plain content))
    (define/override (render-blockquote blocks)
      (nested inset-style blocks))
    (define/override (render-itemization blockss list-style start-num)
      (itemization
       (style (cond
                [start-num 'ordered]
                [(eq? list-style 'tight) 'compact]
                [else #f])
              (match start-num
                [(or #f 1) '()]
                [_ (list (attributes (list (cons 'start (number->string start-num)))))]))
       blockss))

    (define/override (render-line-break)
      (linebreak))
    (define/override (render-bold content)
      (element 'bold content))
    (define/override (render-italic content)
      (element 'italic content))
    (define/override (render-code content)
      (element 'tt content))
    (define/override (render-link content dest title)
      (element (style #f (list (make-target-url dest))) content))
    (define/override (render-image description source title)
      (image-element #f description source '() 1))
    (define/override (render-html content)
      (element (style #f (list (xexpr-property (cdata #f #f content) ""))) '()))
    (define/override (render-footnote-reference label defn-num ref-num)
      (~>> (~a defn-num)
           (link-element #f _ (footnote-definition-tag label))
           (target-element #f _ (footnote-reference-tag label ref-num))
           (element 'superscript)))
    (define/override (render-wikilink content dest)
      (element (style #f (list (make-target-url dest))) content))

    (define/override (render-footnote-definition blocks label ref-count)
      (define multiple-refs? (> ref-count 1))
      (define backrefs (~> (for/list ([i (in-range ref-count)])
                             (define ref-num (add1 i))
                             (link-element
                              #f
                              (if multiple-refs?
                                  (list "↩" (element 'superscript (~a ref-num)))
                                  "↩")
                              (footnote-reference-tag label ref-num)))
                           (add-between " ")
                           (cons " " _)))

      (define target (target-element #f '() (footnote-definition-tag label)))
      (define targeted-blocks
        (match blocks
          [(cons (paragraph para-style para-content) blocks)
           (cons (paragraph para-style (list target para-content)) blocks)]
          [_
           (cons (paragraph plain target) blocks)]))

      (match targeted-blocks
        [(list blocks ... (paragraph para-style para-content))
         (append blocks (list (paragraph para-style (cons para-content backrefs))))]
        [_
         (append targeted-blocks (list (paragraph plain backrefs)))]))

    (define local-tags (make-hash))
    (define/public (footnote-definition-tag label)
      (list 'footnote (hash-ref! local-tags label generated-tag)))
    (define/public (footnote-reference-tag label ref-num)
      (list 'footnote-ref (hash-ref! local-tags (cons label ref-num) generated-tag)))

    (super-new)))
