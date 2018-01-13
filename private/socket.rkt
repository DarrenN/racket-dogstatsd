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

  (define port 8120)
  (define s (udp-open-socket #f #f))
  (udp-bind! s "127.0.0.1" port #t)

  (define (get-datagram)
    (define buffer (make-bytes 1024))
    (define-values (length host port)
      (udp-receive! s buffer 0))
    (subbytes buffer 0 length))

  (test-case "sock-create returns udp?"
    (check-pred udp? (sock-create #:host-port port))
    (sock-close)
    (define-values (a1 a2) (udp-addresses (sock-create #:host-port port)))
    (check-equal? a1 "127.0.0.1")
    (check-equal? a2 "127.0.0.1")
    (sock-close))

  (test-case "sock-send sends bytes to port via UDP"
    (sock-create #:host-port port)
    (sock-send "probot")
    (define res (bytes->string/utf-8 (get-datagram)))
    (check-equal? res "probot")
    (sock-close))

  (test-case "sock-close shuts down the port and a send raises"
    (sock-create #:host-port port)
    (sock-close)
    (check-exn exn:fail? (λ () (sock-send "probot")))))
