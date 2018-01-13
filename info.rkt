#lang info
(define collection "dogstatsd")
(define deps '("base"
               "threading"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/dogstatsd.scrbl" ())))
(define pkg-desc
  "DogStatsD Client - https://docs.datadoghq.com/guides/dogstatsd/")
(define version "0.1")
(define pkg-authors '(Darren_N))
