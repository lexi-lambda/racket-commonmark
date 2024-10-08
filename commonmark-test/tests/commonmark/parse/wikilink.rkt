#lang racket/base

(require commonmark
         commonmark/struct
         rackunit)

(parameterize ([current-parse-wikilinks? #t])
  (check-equal? (string->document "[[example]]")
                (document (list (paragraph (wikilink "example" "example"))) '()))
  (check-equal? (string->document "[[destination|label]]")
                (document (list (paragraph (wikilink "label" "destination"))) '()))
  (check-equal? (string->document "[[destination|label with **bold** markup]]")
                (document (list (paragraph (wikilink (list "label with " (bold "bold") " markup") "destination"))) '()))
  (check-equal? (string->document "[[lorem [[link]] ipsum]]")
                (document (list (paragraph (list "[[lorem " (wikilink "link" "link") " ipsum]]"))) '()))
  (check-equal? (string->document "[[unclosed")
                (document (list (paragraph "[[unclosed")) '()))
  (check-equal? (string->document "[[]]")
                (document (list (paragraph "[[]]")) '())))