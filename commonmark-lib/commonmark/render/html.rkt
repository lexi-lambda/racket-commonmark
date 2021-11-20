#lang racket/base

(require racket/contract
         racket/list
         racket/match
         (except-in xml document document? struct:document)

         "../struct.rkt")

(provide (contract-out
          [document->xexprs (-> document? (listof xexpr/c))]
          [document->html (-> document? string?)]
          [write-document-html (->* [document?] [output-port?] void?)]

          [current-bold-tag (parameter/c symbol?)]
          [current-italic-tag (parameter/c symbol?)]))

;; -----------------------------------------------------------------------------

(define current-bold-tag (make-parameter 'strong))
(define current-italic-tag (make-parameter 'em))

(define (document->xexprs doc)
  (map block->xexpr (document-blocks doc)))

(define (document->html doc)
  (define out (open-output-string))
  (write-document-html doc out)
  (get-output-string out))

(define (write-document-html doc [out (current-output-port)])
  (parameterize ([empty-tag-shorthand html-empty-tags])
    (for ([xexpr (in-list (document->xexprs doc))])
      (write-xexpr xexpr out))))

(define (block->xexpr block)
  (match block
    [(? thematic-break?) '(hr)]
    [(heading content depth)
     `(,(vector-ref #(h1 h2 h3 h4 h5 h6) (sub1 depth))
       ,@(inline->xexprs content))]
    [(code-block content info-string)
     `(pre (code ,@(match info-string
                     [(regexp #px"^[^ \t\r\n]+" (list lang))
                      `(([class ,(string-append "language-" lang)]))]
                     [_ '()])
                 ,content))]
    [(html-block content) (cdata #f #f content)]
    [(paragraph content) `(p ,@(inline->xexprs content))]
    [(blockquote blocks) `(blockquote ,@(map block->xexpr blocks))]
    [(itemization blockss style start-num)
     `(,(if start-num 'ol 'ul)
       ,@(match start-num
           [(or #f 1) '()]
           [_ `(([start ,(number->string start-num)]))])
       ,@(for/list ([blocks (in-list blockss)])
           `(li ,@(match style
                    ['loose (map block->xexpr blocks)]
                    ['tight
                     (for*/list ([block (in-list blocks)]
                                 [xexpr (in-list (match block
                                                   [(paragraph content) (inline->xexprs content)]
                                                   [_ (list (block->xexpr block))]))])
                       xexpr)]))))]))

(define (inline->xexprs content)
  (match content
    [(? list?) (append-map inline->xexprs content)]
    [_ (list (match content
               [(? string?) content]
               [(? line-break?) '(br)]
               [(bold content) `(,(current-bold-tag) ,@(inline->xexprs content))]
               [(italic content) `(,(current-italic-tag) ,@(inline->xexprs content))]
               [(code content) `(code ,@(inline->xexprs content))]
               [(link content dest title)
                `(a ([href ,dest]
                     ,@(if title
                           `([title ,title])
                           '()))
                    ,@(inline->xexprs content))]
               [(image description source title)
                `(img ([src ,source]
                       [alt ,(inline->string description)]
                       ,@(if title
                             `([title ,title])
                             '())))]
               [(html content) (cdata #f #f content)]))]))

(define (inline->string content)
  (cond
    ; fast path for common case of plain string content
    [(string? content) content]
    [else
     (define out (open-output-string))
     (let loop ([content content])
       (match content
         [(? string?) (write-string content out)]
         [(? list?) (for-each loop content)]
         [(? line-break?) (void)]
         [(bold content) (loop content)]
         [(italic content) (loop content)]
         [(code content) (write-string content out)]
         [(link content _ _) (loop content)]
         [(image description _ _) (loop description)]
         [(html content) (write-string content out)]))
     (get-output-string out)]))
