#lang info

(define version "1.2")

(define collection 'multi)

(define deps
  '("base"
    ["commonmark-doc" #:version "1.2"]
    ["commonmark-lib" #:version "1.2"]))
(define build-deps '())

(define implies
  '("commonmark-doc"
    "commonmark-lib"))
