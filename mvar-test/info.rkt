#lang info

(define version "1.0")

(define collection 'multi)

(define deps
  '("base"))
(define build-deps
  '(["mvar-lib" #:version "1.0"]
    "rackunit-lib"))
