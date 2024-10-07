#lang info

(define version "1.1.1")

(define collection 'multi)

(define deps
  '("base"
    ["commonmark-doc" #:version "1.1.1"]
    ["commonmark-lib" #:version "1.1.1"]))
(define build-deps '())

(define implies
  '("commonmark-doc"
    "commonmark-lib"))
