#lang racket/base

(provide get-sock
         update-sock)

;; UDP Socket
(define sock #f)

(define (get-sock)
  sock)

(define (update-sock s)
  (set! sock s))
