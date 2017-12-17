#lang racket/base

(require racket/string
         racket/udp
         "socket.rkt"
         "utils.rkt")

(provide buffer-send
         buffer-create
         buffer-flush
         (struct-out metric-buffer))

;; Store metrics in a buffer
(struct metric-buffer (max-size items) #:transparent)

;; Metric buffer
(define BUFFER #f)

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

;; Create a buffer with the number of metrics it will hold
;; (-> number? metric-buffer?)
(define (buffer-create [limit 25])
  (metric-buffer limit '()))

;; If over max-size then combined metrics into a single string and send
;; (-> metric-buffer? metric-buffer?)
(define (buffer-flush buffer)
  (sock-send (string-join (metric-buffer-items buffer) "\n"))
  (buffer-create (metric-buffer-max-size buffer)))

;; Add a metric to the buffer or flush to socket
;; (-> metric-buffer? string? metric-buffer?)
(define (buffer-send buffer metric)
  (define limit (metric-buffer-max-size buffer))
  (define items (metric-buffer-items buffer))
  (if (< (length items) limit)
      (struct-copy metric-buffer buffer
                   [items (append items (list metric))])
      (buffer-flush (struct-copy metric-buffer buffer
                                 [items (append items (list metric))]))))

;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit
           racket/list
           racket/udp
           "socket.rkt"
           "utils.rkt")

  ;; Create out own testing socket and listener
  (define port 8129)
  (define tsock (sock-create #:host-port port))
  (define s (udp-open-socket #f #f))
  (udp-bind! s "127.0.0.1" port #t)

  (define (get-datagram)
    (define buffer (make-bytes 1024))
    (define-values (length host port)
      (udp-receive! s buffer 0))
    (subbytes buffer 0 length))

  (test-case "buffer-create"
    (check-pred metric-buffer? (buffer-create))
    (check-equal? (metric-buffer-max-size (buffer-create)) 25)
    (check-equal? (metric-buffer-max-size (buffer-create 50)) 50))

  (test-case "buffer-send adds to buffer when under max-size"
    (define buffer
      (for/fold ([b (buffer-create)])
                ([i (range 10)])
        (buffer-send b i)))
    (check-equal? (length (metric-buffer-items buffer)) 10))

  (test-case "buffer-flush flushes to socket"
    (define b1
      (for/fold ([b (buffer-create)])
                ([i '("foo" "bar" "baz" "quux")])
        (buffer-send b i)))
    (define b2 (buffer-flush b1))
    (check-equal? (length (metric-buffer-items b1)) 4) ; original still full
    (check-equal? (length (metric-buffer-items b2)) 0) ; reset to zero
    (check-equal? (metric-buffer-max-size b2) 25) ; set to original
    (check-pred null? (metric-buffer-items b2)) ; empty list
    (check-equal? (bytes->string/utf-8 (get-datagram))
                  "foo\nbar\nbaz\nquux"))

  (test-case "buffer-send flushes when past max-size and resets"
    (define buffer
      (for/fold ([b (buffer-create 3)])
                ([i '("foo" "bar" "baz" "quux")])
        (buffer-send b i)))
    (check-equal? (length (metric-buffer-items buffer)) 0) ; reset to zero
    (check-equal? (metric-buffer-max-size buffer) 3) ; set to original
    (check-pred null? (metric-buffer-items buffer)) ; empty list
    (check-equal? (bytes->string/utf-8 (get-datagram))
                  "foo\nbar\nbaz\nquux"))

  (udp-close s)
  (sock-close))
