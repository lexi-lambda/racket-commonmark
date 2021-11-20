#lang info

(define version "1.0")

(define collection 'multi)

(define deps
  '("base"
    "commonmark-doc"
    "commonmark-lib"))
(define build-deps '())

(define implies
  '("commonmark-doc"
    "commonmark-lib"))
