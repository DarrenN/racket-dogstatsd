#lang racket/base

(require racket/string
         racket/udp)

(provide sock-create
         sock-send
         sock-close)

;; UDP Socket
(define sock #f)

(define (get-sock)
  sock)

(define (update-sock s)
  (set! sock s))

(define (handle-sock-exn e)
  (raise-arguments-error 'sock-send
                         "UDP socket not open. Call (sock-create) first."))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

;; Create a UDP socket and make available to metric functions
(define (sock-create #:host-name [host-name "127.0.0.1"]
                     #:host-port [host-port 8125])
  (update-sock (udp-open-socket))
  (let ([sock (get-sock)])
    (udp-connect! sock host-name host-port)
    sock))

;; Send a metric string
;; (-> string? bool?)
(define (sock-send metric)
  (with-handlers ([exn? handle-sock-exn])
    (udp-send* (get-sock) (string->bytes/utf-8 metric))))

;; Cleanup and close the socket
(define (sock-close)
  (let ([sock (get-sock)])
    (when (udp-connected? sock)
      (udp-close sock)
      (update-sock sock))))

;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit
           racket/udp)

  (test-case "sock-create returns udp?"
    (check-pred udp? (sock-create))
    (define-values (a1 a2) (udp-addresses (sock-create)))
    (check-equal? a1 "127.0.0.1")
    (check-equal? a2 "127.0.0.1"))

  (sock-close))
