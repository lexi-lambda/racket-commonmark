#lang racket/base

(require racket/contract
         "private/struct.rkt"
         "private/parse/block.rkt")

(provide (contract-out
          [read-document (-> input-port? document?)]
          [string->document (-> string? document?)]

          [current-parse-footnotes? (parameter/c any/c boolean?)]))
