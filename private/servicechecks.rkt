#lang racket/base

(require racket/string
         racket/udp
         threading
         "buffer.rkt"
         "socket.rkt"
         "utils.rkt")

(provide service-check
         OK
         WARNING
         CRITICAL
         UNKNOWN)

;; Is a number and within 0 - 3
(define (valid-status? status)
  (and (number? status) (>= status OK) (<= status UNKNOWN)))

;; Invalid status is cast to UNKNOWN
;; (-> string? number? string?)
(define (create-service-check name status)
  (if (valid-status? status)
      (format "_sc|~a|~a" name status)
      (format "_sc|~a|~a" name UNKNOWN)))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

(define OK 0)
(define WARNING 1)
(define CRITICAL 2)
(define UNKNOWN 3)

(define (service-check name [status UNKNOWN] #:timestamp [timestamp #f]
                       #:hostname [hostname #f] #:tags [tags #f]
                       #:message [message #f] #:buffer [buffer #f])

  (define metric
    (~>> (create-service-check name status)
         (append-event-metric "d" timestamp)
         (append-event-metric "h" hostname)
         (append-tags tags)
         (append-event-metric "m" message)))

  (if (metric-buffer? buffer)
      (buffer-send buffer metric)
      (sock-send metric)))

;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit
           racket/list
           "socket.rkt"
           "statsd.rkt")

  ;; Create out own testing socket and listener
  (define port 8128)
  (define tsock (sock-create #:host-port port))
  (define s (udp-open-socket #f #f))
  (udp-bind! s "127.0.0.1" port #t)

  (define (get-datagram)
    (define buffer (make-bytes 1024))
    (define-values (length host port)
      (udp-receive! s buffer 0))
    (subbytes buffer 0 length))

  (test-case "valid-status?"
    (define xs (list OK WARNING CRITICAL UNKNOWN))
    (check-equal? (map valid-status? xs) (list #t #t #t #t))
    (check-equal? (valid-status? "ohai") #f))

  (test-case "create-service-check"
    (check-equal? (create-service-check "aphex" OK) "_sc|aphex|0")
    (check-equal? (create-service-check "aphex" "twin") "_sc|aphex|3"))

  (test-case "service-check"
    (define timestamp (current-seconds))
    (define hostname "cthulhu")
    (service-check "aphex" OK)
    (check-equal? (get-datagram) #"_sc|aphex|0")

    (service-check "aphex" WARNING #:timestamp timestamp)
    (check-equal? (get-datagram)
                  (string->bytes/utf-8
                   (format "_sc|aphex|~a|d:~a" WARNING timestamp)))

    (service-check "aphex" CRITICAL #:hostname hostname)
    (check-equal? (get-datagram)
                  (string->bytes/utf-8
                   (format "_sc|aphex|~a|h:~a" CRITICAL hostname)))

    (service-check "aphex" UNKNOWN #:tags '("name:azathoth" "aeon:12"))
    (check-equal? (get-datagram)
                  (string->bytes/utf-8
                   (format "_sc|aphex|~a|#~a" UNKNOWN "name:azathoth,aeon:12")))

    (service-check "aphex" 999 #:message "Welcome the Old\nOnes!")
    (check-equal? (get-datagram)
                  (string->bytes/utf-8
                   (format "_sc|aphex|~a|m:~a" UNKNOWN "Welcome the Old\\nOnes!")))

    ;; Message is in the specified order
    (service-check "aphex" 999 #:message "Welcome the Old\nOnes!"
                   #:tags '("aeon:12") #:hostname hostname #:timestamp timestamp)
    (check-equal? (get-datagram)
                  (string->bytes/utf-8
                   (format "_sc|aphex|~a|d:~a|h:~a|#~a|m:~a"
                           UNKNOWN timestamp hostname "aeon:12"
                           "Welcome the Old\\nOnes!"))))
  (udp-close s)
  (sock-close))
