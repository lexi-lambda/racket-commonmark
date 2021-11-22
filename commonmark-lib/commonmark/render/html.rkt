#lang racket/base

(require net/uri-codec
         racket/class
         racket/contract
         racket/format
         racket/list
         racket/match
         threading
         (except-in xml document document? struct:document)

         "../private/render.rkt"
         "../private/struct.rkt")

(provide (contract-out
          [document->xexprs (-> document? (listof xexpr/c))]
          [document->html (-> document? string?)]
          [write-document-html (->* [document?] [output-port?] void?)]

          [current-bold-tag (parameter/c symbol?)]
          [current-italic-tag (parameter/c symbol?)]))

;; -----------------------------------------------------------------------------

(define current-bold-tag (make-parameter 'strong))
(define current-italic-tag (make-parameter 'em))

(define (document->html doc)
  (define out (open-output-string))
  (write-document-html doc out #:who 'document->html)
  (get-output-string out))

(define (write-document-html doc [out (current-output-port)] #:who [who 'write-document-html])
  (parameterize ([empty-tag-shorthand html-empty-tags])
    (for ([xexpr (in-list (document->xexprs doc #:who who))])
      (write-xexpr xexpr out))))

(define (document->xexprs doc #:who [who 'document->xexprs])
  (send (new html-render% [doc doc] [who who]) render-document))

(define html-render%
  (class abstract-render%
    (define/override (render-document)
      (define-values [body footnotes] (super render-document))
      (if (empty? footnotes)
          body
          `[,@body
            (section ([class "footnotes"])
                     (ol ,@footnotes))]))

    (define/override (render-thematic-break)
      '(hr))
    (define/override (render-heading content depth)
      `(,(vector-ref #(h1 h2 h3 h4 h5 h6) (sub1 depth)) ,@content))
    (define/override (render-code-block content language)
      `(pre (code ,@(if language
                        `(([class ,(string-append "language-" language)]))
                        '())
                  ,content)))
    (define/override (render-html-block content)
      (cdata #f #f content))
    (define/override (render-paragraph content)
      `(p ,@content))
    (define/override (render-blockquote blocks)
      `(blockquote ,@blocks))
    (define/override (render-itemization blockss style start-num)
      `(,(if start-num 'ol 'ul)
        ,@(match start-num
            [(or #f 1) '()]
            [_ `(([start ,(~a start-num)]))])
        ,@(for/list ([blocks (in-list blockss)])
            `(li ,@blocks))))

    (define/override (render-line-break)
      '(br))
    (define/override (render-bold content)
      `(,(current-bold-tag) ,@content))
    (define/override (render-italic content)
      `(,(current-italic-tag) ,@content))
    (define/override (render-code content)
      `(code ,content))
    (define/override (render-link content dest title)
      `(a ([href ,dest]
           ,@(if title
                 `([title ,title])
                 '()))
          ,@content))
    (define/override (render-image description source title)
      `(img ([src ,source]
             [alt ,description]
             ,@(if title
                   `([title ,title])
                   '()))))
    (define/override (render-html content)
      (cdata #f #f content))
    (define/override (render-footnote-reference label defn-num ref-num)
      `(sup ([class "footnote-ref"])
            (a ([id ,(footnote-reference-anchor label ref-num)]
                [href ,(~a "#" (footnote-definition-anchor (uri-path-segment-encode label)))])
               ,(~a defn-num))))

    (define/override (render-footnote-definition blocks label ref-count)
      (define encoded-label (uri-path-segment-encode label))
      (define multiple-refs? (> ref-count 1))
      (define backrefs (~> (for/list ([i (in-range ref-count)])
                             (define ref-num (add1 i))
                             `(a ([class "footnote-backref"]
                                  [href ,(~a "#" (footnote-reference-anchor encoded-label ref-num))]
                                  [aria-label ,(~a "Jump to reference"
                                                   (if multiple-refs? (~a " (" ref-num ")") ""))])
                                 "↩" ,@(if multiple-refs? `((sup ,(~a ref-num))) '())))
                           (add-between " ")
                           (cons " " _)))

      `(li ([id ,(footnote-definition-anchor label)])
           ,@(match blocks
               [(list block ... (list 'p inline ...))
                `(,@block (p ,@inline ,@backrefs))]
               [_
                (append blocks backrefs)])))

    (define/public (footnote-definition-anchor label)
      (~a "fn-" label))
    (define/public (footnote-reference-anchor label ref-num)
      (~a "fnref-" label (if (= ref-num 1) "" (~a "-" ref-num))))

    (super-new)))
