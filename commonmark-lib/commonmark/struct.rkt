#lang racket/base

(require racket/contract
         "private/struct.rkt")

(provide block? inline?
         thematic-break thematic-break?
         line-break line-break?
         (contract-out
          (struct document ([blocks (listof block?)]
                            [footnotes (listof footnote-definition?)]))
          (struct footnote-definition ([blocks (listof block?)] [label string?]))

          (struct heading ([content inline?] [depth (integer-in 1 6)]))
          (struct code-block ([content string?] [info-string (or/c string? #f)]))
          (struct html-block ([content string?]))
          (struct paragraph ([content inline?]))
          (struct blockquote ([blocks (listof block?)]))
          (struct itemization ([blockss (listof (listof block?))]
                               [style (or/c 'loose 'tight)]
                               [start-num (or/c exact-nonnegative-integer? #f)]))

          (struct italic ([content inline?]))
          (struct bold ([content inline?]))
          (struct code ([content string?]))
          (struct link ([content inline?] [dest string?] [title (or/c string? #f)]))
          (struct image ([description inline?] [source string?] [title (or/c string? #f)]))
          (struct html ([content string?]))
          (struct footnote-reference ([label string?]))))
