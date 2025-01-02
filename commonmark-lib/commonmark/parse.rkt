#lang racket/base

(require racket/contract
         "private/struct.rkt"
         "private/parse/block.rkt"
         "private/parse/inline.rkt")

(provide (contract-out
          [read-document (-> input-port? document?)]
          [string->document (-> string? document?)]

          [current-parse-footnotes? (parameter/c any/c boolean?)]
          [current-parse-wikilinks? (parameter/c any/c boolean?)]))
