#lang racket/base

(require "./private/buffer.rkt")
(require "./private/events.rkt")
(require "./private/servicechecks.rkt")
(require "./private/socket.rkt")
(require "./private/statsd.rkt")

(provide (all-from-out "./private/buffer.rkt")
         (all-from-out "./private/events.rkt")
         (all-from-out "./private/servicechecks.rkt")
         (all-from-out "./private/socket.rkt")
         (all-from-out "./private/statsd.rkt"))

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>

;; Code here

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
