#lang racket/base

(require racket/string
         racket/udp
         threading
         (for-syntax racket/base
                     racket/syntax))

(provide create-socket
         counter
         guage
         histogram
         timer)

;; UDP Socket
(define sock #f)

;; Metric constants

(define COUNTER "c")
(define GAUGE "g")
(define TIMER "ms")
(define HISTOGRAM "h")
(define SET "s")

;; Ensure a sample-rate is valid for a metric type
;; (-> number? string? (U number? bool?))
(define (check-sample-rate sample-rate type)
  (cond [(equal? type COUNTER) sample-rate]
        [(equal? type HISTOGRAM) sample-rate]
        [(equal? type TIMER) sample-rate]
        [else #f]))

;; Ensure a tag values list doesn't contain spaces
;; (-> list? list?)
(define (escape-tags tags)
  (map (λ (s) (string-replace (string-trim s) " " "_")) tags))

;; Build a basic name:value|type string
;; (-> string? (U number? string?) string? string? string?)
(define (create-name-value-type name value type)
  (string-trim (format "~a:~a|~a" name value type)))

;; Only append sample-rate if a number between 0 and 1
;; (-> (U bool? number?) string? string?)
(define (append-sample-rate sample-rate str)
  (if (and sample-rate (number? sample-rate)
           (and (< sample-rate 1) (> sample-rate 0)))
      (format "~a|@~a" str sample-rate)
      str))

;; Append tags to metric string
;; (-> (U bool? list?) string? string?)
(define (append-tags tags str)
  (if (and (not (null? tags)) (list? tags))
      (format "~a|#~a" str (string-join (escape-tags tags) ","))
      str))

;; (-> string? (U string? number?) string? number? list? bytes?)
(define (create-metric name value type #:sample-rate [sample-rate #f]
                       #:tags [tags #f])
  (let* ([sample (check-sample-rate sample-rate type)])
    (string->bytes/utf-8
     (~>> (create-name-value-type name value type)
          (append-sample-rate sample)
          (append-tags tags)))))

;; [Macro] Bind a proc to the metric type
;; We fire and forget udp datagrams (udp-send*)
(define-syntax (define/metric stx)
  (syntax-case stx ()
    [(_ procname type)
     #'(define (procname name value #:sample-rate [sample-rate #f] #:tags
                         [tags #f])
         (udp-send*
          sock (create-metric name value type
                              #:sample-rate sample-rate
                              #:tags tags)))]))


;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

(define (create-socket #:host-name [host-name "127.0.0.1"]
                       #:host-port [host-port 8125])
  (set! sock (udp-open-socket))
  (udp-connect! sock host-name host-port)
  sock)

;; Basic metric procs
;; Ex: (counter name value #:sample-rate 0.25 #:tags '("city:london" "tz:gmt"))

(define/metric guage GAUGE)
(define/metric set SET)
(define/metric counter COUNTER)
(define/metric histogram HISTOGRAM)
(define/metric timer TIMER)

;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit)

  (define s (create-socket))

  (test-case "check-sample-rate returns sample-rate or #f"
    (check-false (check-sample-rate 0.12 GAUGE))
    (check-false (check-sample-rate 0.12 SET))
    (check-equal? (check-sample-rate 0.12 COUNTER) 0.12)
    (check-equal? (check-sample-rate 0.12 HISTOGRAM) 0.12)
    (check-equal? (check-sample-rate 0.12 TIMER) 0.12))

  (test-case "all metric procs return #t"
    (check-true (guage "rkt.guage" "eco"))
    (check-true (set "rkt.set" "eco"))
    (check-true (counter "rkt.counter" 1))
    (check-true (histogram "rkt.histogram" 12))
    (check-true (timer "rkt.timer" (current-inexact-milliseconds))))

  (test-case "create-name-value-type returns formatted string"
    (check-equal? (create-name-value-type "spc" "eco" "c") "spc:eco|c")
    (check-equal? (create-name-value-type "spc" 0.5 "c") "spc:0.5|c"))

  (test-case "append-sample-rate"
    (define base "spc:eco|c")
    (check-equal? (append-sample-rate 12 base) base)
    (check-equal? (append-sample-rate 0 base) base)
    (check-equal? (append-sample-rate 1 base) base)
    (check-equal? (append-sample-rate .12 base) "spc:eco|c|@0.12"))

  (test-case "append-tags"
    (define base1 "spc:eco|c")
    (check-equal? (append-tags '() base1) base1)
    (check-equal? (append-tags '("song:fallapartforlove") base1)
                  (format "~a|#song:fallapartforlove" base1))
    (check-equal? (append-tags '("song:die" "song:alone") base1)
                  (format "~a|#song:die,song:alone" base1))
    (check-equal? (append-tags '("song:die alone" "song:full flow  ") base1)
                  (format "~a|#song:die_alone,song:full_flow" base1))))
