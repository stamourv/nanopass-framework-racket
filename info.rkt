#lang info
(define version "0.1")
(define collection "nanopass")

(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib"
                     "racket-doc"))
(define scribblings '(("doc/user-guide.scrbl")))
