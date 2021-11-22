#lang racket/base

(require data/gvector
         racket/class
         racket/list
         racket/match
         threading
         "struct.rkt")

(provide abstract-render%)

(struct footnote-info (num ref-count) #:transparent)

;; This is a private helper class for implementing a document renderer. It is
;; not currently intended for public use, as some invariants are currently
;; unchecked, but it is useful for sharing functionality between the
;; public-facing HTML renderer and the internal Scribble renderer used in the
;; documentation.
;;
;; The main bit of logic handled by `abstract-render%` is collecting and
;; cross-referencing footnotes. It also provides the implementation of the
;; overall tree traversal, as well as some other minor details shared between
;; renderers. It is implemented as a class (rather than as, say, a higher-order
;; function) to make it easier to express the mutual recursion between the
;; shared logic and the implementation-specific logic.
;;
;; The name of this class is `abstract-render%` rather than `abstract-renderer%`
;; because instances of the class are stateful and should only be used to render
;; a single document. Uses of subclasses should essentially always take the
;; following form:
;;
;;     (send (new <format>-render% [doc <doc>] [who <who>]) render-document)
;;
;; Once the document has been rendered, the instance should be thrown away.
(define abstract-render%
  (class object%
    (init-field who doc)

    ;; -------------------------------------------------------------------------

    (define footnote-defns
      (for/fold ([footnote-defns (hash)])
                ([footnote-defn (in-list (document-footnotes doc))])
        (match-define (footnote-definition blocks label) footnote-defn)
        (if (hash-has-key? footnote-defns label)
            footnote-defns
            (hash-set footnote-defns label blocks))))

    (define footnote-infos (make-hash))
    (define rendered-footnotes (make-gvector))

    (define/public (resolve-footnote-reference label)
      (match (hash-ref footnote-infos label #f)
        [#f
         (define new-info (footnote-info (add1 (hash-count footnote-infos)) 1))
         (hash-set! footnote-infos label new-info)
         (define blocks (hash-ref footnote-defns label
                                  (λ ()
                                    (raise-arguments-error who "reference to undefined footnote"
                                                           "footnote label" label
                                                           "document..." doc))))
         (define index (gvector-count rendered-footnotes))
         (gvector-set! rendered-footnotes index #f) ; reserve this slot
         (gvector-set! rendered-footnotes index (footnote-definition (render-flow blocks) label))
         new-info]
        [(footnote-info num ref-count)
         (define new-info (footnote-info num (add1 ref-count)))
         (hash-set! footnote-infos label new-info)
         new-info]))

    ;; -------------------------------------------------------------------------

    (abstract render-thematic-break
              render-heading
              render-code-block
              render-html-block
              render-paragraph
              render-blockquote
              render-itemization

              render-line-break
              render-bold
              render-italic
              render-code
              render-link
              render-image
              render-html
              render-footnote-reference

              render-footnote-definition)

    (define/public (render-document)
      (values (render-document-flow)
              (render-footnote-definitions)))

    (define/public (render-document-flow)
      (render-flow (document-blocks doc)))

    (define/public (render-footnote-definitions)
      (for/list ([defn (in-gvector rendered-footnotes)])
        (match-define (footnote-definition rendered-blocks label) defn)
        (match-define (footnote-info _ ref-count) (hash-ref footnote-infos label))
        (render-footnote-definition rendered-blocks label ref-count)))

    (define/public (render-block block)
      (match block
        [(? thematic-break?)
         (render-thematic-break)]
        [(heading content depth)
         (render-heading (render-inline content) depth)]
        [(code-block content info-string)
         (define language (and~>> info-string (regexp-match #px"^[^ \t\r\n]+") first))
         (render-code-block content language)]
        [(html-block content)
         (render-html-block content)]
        [(paragraph content)
         (render-paragraph (render-inline content))]
        [(blockquote blocks)
         (render-blockquote (render-flow blocks))]
        [(itemization blockss style start-num)
         (define rendered-blocks
           (for/list ([blocks (in-list blockss)])
             (match style
               ['loose (render-flow blocks)]
               ['tight
                (for*/list ([block (in-list blocks)]
                            [xexpr (in-list (match block
                                              [(paragraph content) (render-tight-paragraph content)]
                                              [_ (list (render-block block))]))])
                  xexpr)])))
         (render-itemization rendered-blocks style start-num)]))

    (define/public (render-flow blocks)
      (for/list ([block (in-list blocks)])
        (render-block block)))

    (define/public (render-inline content)
      (if (list? content)
          (render-inlines content)
          (list (match content
                  [(? string?)
                   (render-inline-string content)]
                  [(? line-break?)
                   (render-line-break)]
                  [(bold content)
                   (render-bold (render-inline content))]
                  [(italic content)
                   (render-italic (render-inline content))]
                  [(code content)
                   (render-code content)]
                  [(link content dest title)
                   (render-link (render-inline content) dest title)]
                  [(image description source title)
                   (render-image (render-image-description description) source title)]
                  [(html content)
                   (render-html content)]
                  [(footnote-reference label)
                   (match-define (footnote-info defn-num ref-num) (resolve-footnote-reference label))
                   (render-footnote-reference label defn-num ref-num)]))))

    (define/public (render-inlines contents)
      (for*/list ([content (in-list contents)]
                  [result (in-list (render-inline content))])
        result))

    (define/public (render-inline-string content)
      content)

    (define/public (render-tight-paragraph content)
      (render-inline content))

    (define/public (render-image-description content)
      (inline->string content))

    (define/public (inline->string content)
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
             [(html content) (write-string content out)]
             [(? footnote-reference?) (void)]))
         (get-output-string out)]))

    ;; -------------------------------------------------------------------------

    (super-new)))
