#lang info

(define version "1.0")
(define license 'ISC)

(define collection 'multi)

(define deps
  '("base"
    ["mvar-doc" #:version "1.0"]
    ["mvar-lib" #:version "1.0"]))
(define build-deps '())

(define implies
  '("mvar-doc"
    "mvar-lib"))
