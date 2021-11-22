#lang racket/base

(require commonmark
         commonmark/struct
         racket/match
         racket/string
         rackunit)

(define (md . strs)
  (string->document (string-join strs "\n")))
(define (md* . strs)
  (define doc (apply md strs))
  (list doc (document->xexprs doc)))

(check-equal? (md "A paragraph.[^1]"
                  ""
                  "[^1]: A footnote.")
              (document (list (paragraph "A paragraph.[^1]")
                              (paragraph "[^1]: A footnote."))
                        '())
              "Footnotes are ignored when `current-parse-footnotes?` is #f")

(parameterize ([current-parse-footnotes? #t])
  (check-equal? (md "This is some text![^1]. Other text.[^footnote]."
                    ""
                    "[^1]: Some *bolded* footnote definition."
                    ""
                    "[^footnote]:"
                    "    > Blockquotes can be in a footnote."
                    ""
                    "        as well as code blocks"
                    ""
                    "    or, naturally, simple paragraphs.")
                (document (list (paragraph
                                 (list "This is some text!"
                                       (footnote-reference "1")
                                       ". Other text."
                                       (footnote-reference "footnote")
                                       ".")))
                          (list (footnote-definition
                                 (list (paragraph (list "Some " (italic "bolded") " footnote definition.")))
                                 "1")
                                (footnote-definition
                                 (list
                                  (blockquote (list (paragraph "Blockquotes can be in a footnote.")))
                                  (code-block "as well as code blocks\n" #f)
                                  (paragraph "or, naturally, simple paragraphs."))
                                 "footnote")))
                "Footnotes are parsed when `current-parse-footnotes?` is #t")

  (check-equal? (md "This doesn't have a referent[^nope].")
                (document (list (paragraph "This doesn't have a referent[^nope].")) '())
                "References to nonexistent footnotes are ignored")

  (check-equal? (md* "[^unused]: This is unused.")
                (list (document '() (list (footnote-definition (list (paragraph "This is unused."))
                                                               "unused")))
                      '())
                "Unreferenced footnote definitions are not rendered")

  (check-equal? (md "A paragraph.[^1]"
                    "[^1]: A footnote.")
                (document (list (paragraph (list "A paragraph." (footnote-reference "1"))))
                          (list (footnote-definition (list (paragraph "A footnote.")) "1")))
                "Footnote definitions can interrupt paragraphs")

  (check-equal? (md "A paragraph.[^1]"
                    ""
                    "[^1]:     Some text.")
                (document (list (paragraph (list "A paragraph." (footnote-reference "1"))))
                          (list (footnote-definition (list (paragraph "Some text.")) "1")))
                "The first line of a footnote definition cannot start an indented code block")

  (test-case
   "Footnotes are rendered in the order they are referenced"
   (match-define (list doc xexprs) (md* "[^3][^1][^2]"
                                        "[^1]: [^4]"
                                        "[^2]: "
                                        "[^3]: "
                                        "[^4]: "))
   (check-equal? doc (document (list (paragraph (list (footnote-reference "3")
                                                      (footnote-reference "1")
                                                      (footnote-reference "2"))))
                               (list (footnote-definition (list (paragraph (footnote-reference "4"))) "1")
                                     (footnote-definition '() "2")
                                     (footnote-definition '() "3")
                                     (footnote-definition '() "4"))))
   (match xexprs
     [(list _ (list 'section _ (list 'ol `(li ([id ,fn-ids]) ,_ ...) ...)))
      (check-equal? fn-ids '("fn-3" "fn-1" "fn-4" "fn-2"))])))
