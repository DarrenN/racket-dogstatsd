#lang racket/base

(require racket/list
         racket/string
         threading
         (for-syntax racket/base
                     racket/syntax)
         "buffer.rkt"
         "socket.rkt"
         "utils.rkt")

(provide counter
         guage
         histogram
         make-buffered
         set
         timer
         with-timer)

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

;; Random number between 0 and 1
;; (-> number?)
(define (get-rand)
  (exact->inexact (/ (random 100) 100)))

;; Create a metric string
;; (-> string? (U string? number?) string? (U number? false?) list?
;;     (U string? false?))
(define (create-metric-string name value type sample tags)
  (~>> (create-name-value-type name value type)
       (append-sample-rate sample)
       (append-tags tags)))

;; Only create a metric string is get-rand is less than sample-rate
;; (-> string? (U string? number?) string? number? list? (U string? false?))
(define (create-metric-if-within-range name value type sample tags)
  (if (<= (get-rand) sample)
      (create-metric-string name value type sample tags)
      #f))

;; Create a metric string - if sample-rate is given and this is a valid type for
;; sampling then we check the sample-rate against rnd and return #f if rnd
;; larger than sample-rate
;; (-> string? (U string? number?) string? number? list? (U string? false?))
(define (create-metric name value type #:sample-rate [sample-rate #f]
                       #:tags [tags #f])
  (let* ([sample (check-sample-rate sample-rate type)])
    (if (sample-within-bounds? sample)
        (create-metric-if-within-range name value type sample tags)
        (create-metric-string name value type sample tags))))

;; [Macro]
;; Bind a proc to the metric type
;; We fire and forget udp datagrams (udp-send*)
(define-syntax (define/metric stx)
  (syntax-case stx ()
    [(_ procname type)
     #'(define (procname name value #:sample-rate [sample-rate #f] #:tags
                         [tags #f] #:buffer [buffer #f])
         (define metric (create-metric name value type
                                       #:sample-rate sample-rate
                                       #:tags tags))

         ;; if metric false then do not send (dropped due to sample-rate)
         (when metric
           (if (metric-buffer? buffer)
               (buffer-send buffer metric)
               (sock-send metric))))]))

; convert '(keyword? any? ...) into a hash(keyword? any)
; used to keep values straight when sorting the keywords
(define (make-kw-hash lst)
  (let* ([grouped (group-by keyword? lst)]
         [kws (car grouped)]
         [kw-args (cadr grouped)])
    (for/hash ([key kws]
               [val kw-args])
      (values key val))))


;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

;; Basic metric procs
;; Ex: (counter name value #:sample-rate 0.25 #:tags '("city:london" "tz:gmt"))
(define/metric guage GAUGE)
(define/metric set SET)
(define/metric counter COUNTER)
(define/metric histogram HISTOGRAM)
(define/metric timer TIMER)

;; [Macro]
;; Wrap code block with time-apply and send real value (in milliseconds)
;; via a timer call. Takes all the same keyword args as timer.
;; ex: (with-timer "rkt.cool-timer" #:tags '("chill") ...)
(define-syntax (with-timer stx)
  (syntax-case stx (with-timer)
    [(_ name keywords ... (body ...))
     #'(let ([proc (λ () (body ...))])
         (define-values (res cpu real gc) (time-apply proc '()))
         (timer name real keywords ...)
         (car res))]))

;; Create a buffered version of a metric proc
;; ex: (define bcounter (make-buffered counter 10))
;; (-> metric? number? metric?)
(define (make-buffered base-proc [limit 25])
  (let ([buffer (buffer-create limit)])
    (make-keyword-procedure
     (λ (kws kw-args . args)
       (define nkws (cons (string->keyword "buffer") kws))
       (define nkw-args (cons buffer kw-args))
       (set! buffer (keyword-apply base-proc nkws nkw-args args))))))


;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit
           racket/list
           racket/udp
           "buffer.rkt"
           "socket.rkt")

  ;; Create our own testing socket and listener
  (define port 8127)
  (define tsock (sock-create #:host-port port))
  (define s (udp-open-socket #f #f))
  (udp-bind! s "127.0.0.1" port #t)

  (define (get-datagram)
    (define buffer (make-bytes 1024))
    (define-values (length host port)
      (udp-receive! s buffer 0))
    (subbytes buffer 0 length))

  (test-case "with-timer"
    (with-timer "rkt.timer" #:tags '("foo" "bar")
      (for ([i (range 200)])
        (map (λ (s) (string->bytes/utf-8 s)) (make-list i "foo"))))
    (define res (bytes->string/utf-8 (get-datagram)))
    (check-regexp-match #rx"rkt.timer:[\\d]|ms|#foo" res))

  (test-case "with-timer returns a value"
    (define timed (with-timer "rkt.timer" #:tags '("foo")
                    (+ 1 1 1)))
    (define res (bytes->string/utf-8 (get-datagram)))
    (check-regexp-match #rx"rkt.timer:[\\d]|ms|#foo" res)
    (check-equal? timed 3))

  (test-case "timer"
    (timer "rkt.timer" 23)
    (check-equal? (bytes->string/utf-8 (get-datagram))
                  "rkt.timer:23|ms"))

  (test-case "gauge"
    (guage "rkt.gauge" 12)
    (check-equal? (bytes->string/utf-8 (get-datagram))
                  "rkt.gauge:12|g"))

  (test-case "set"
    (set "rkt.set" 1203)
    (check-equal? (bytes->string/utf-8 (get-datagram))
                  "rkt.set:1203|s"))

  (test-case "counter"
    (counter "rkt.counter" 1203344)
    (check-equal? (bytes->string/utf-8 (get-datagram))
                  "rkt.counter:1203344|c"))

  (test-case "histogram"
    (histogram "rkt.histogram" 12)
    (check-equal? (bytes->string/utf-8 (get-datagram))
                  "rkt.histogram:12|h"))

  (test-case "check-sample-rate returns sample-rate or #f"
    (check-false (check-sample-rate 0.12 GAUGE))
    (check-false (check-sample-rate 0.12 SET))
    (check-equal? (check-sample-rate 0.12 COUNTER) 0.12)
    (check-equal? (check-sample-rate 0.12 HISTOGRAM) 0.12)
    (check-equal? (check-sample-rate 0.12 TIMER) 0.12))

  (test-case "metrics can take a buffer and will return it"
    (define buffer (buffer-create 3))
    (define b1 (counter "rkt" 1 #:buffer buffer))
    (check-pred metric-buffer? b1)
    (define b2
      (for/fold ([b b1])
                ([i '("is" "super" "cool")])
        (counter i 1 #:buffer b)))
    (check-equal? (bytes->string/utf-8 (get-datagram))
                  "rkt:1|c\nis:1|c\nsuper:1|c\ncool:1|c"))

  (test-case "make-buffered creates a buffered form of a metric proc"
    (define bcounter (make-buffered counter 3))
    (for ([i '(1 2 3 4 5 6 7 8)])
      (bcounter "rkt" i #:tags '("proc")))
    (check-equal?
     (bytes->string/utf-8 (get-datagram))
     "rkt:1|c|#proc\nrkt:2|c|#proc\nrkt:3|c|#proc\nrkt:4|c|#proc")
    (check-equal?
     (bytes->string/utf-8 (get-datagram))
     "rkt:5|c|#proc\nrkt:6|c|#proc\nrkt:7|c|#proc\nrkt:8|c|#proc"))

  (udp-close s)
  (sock-close))
