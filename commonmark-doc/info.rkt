#lang info

(define version "1.1.1")

(define collection 'multi)

(define deps
  '("base"))
(define build-deps
  '(["commonmark-lib" #:version "1.1.1"]
    "racket-doc"
    "scribble-lib"
    "threading-lib"))
