#lang racket/base

(require racket/string
         threading
         (for-syntax racket/base
                     racket/syntax)
         "buffer.rkt"
         "socket.rkt"
         "utils.rkt")

(provide counter
         guage
         histogram
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

;; Create a metric string
;; (-> string? (U string? number?) string? number? list? bytes?)
(define (create-metric name value type #:sample-rate [sample-rate #f]
                       #:tags [tags #f])
  (let* ([sample (check-sample-rate sample-rate type)])
    (~>> (create-name-value-type name value type)
         (append-sample-rate sample)
         (append-tags tags))))


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
         (if (metric-buffer? buffer)
             (buffer-send buffer metric)
             (sock-send metric)))]))

;; [Macro]
;; Used in (with-timer) - calculates the execution time of executing the body
;; and sends in a timer metric as ms - returns whatever the executed body
;; returns
(define-for-syntax (time-body name sample-rate tags bodies)
  (with-syntax ([timer-name name]
                [timer-sample sample-rate]
                [timer-tags tags]
                [(body ...) bodies])
    (syntax/loc bodies
      (let ([start-time (current-inexact-milliseconds)]
            [result ((λ () body ...))]
            [end-time (current-inexact-milliseconds)])
        (timer timer-name (round (- end-time start-time)) #:sample-rate
               timer-sample #:tags timer-tags)
        result))))


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
;; Wrap a body with a timer call:
;; (with-timer #:name "timer.name" #:sample-rate 0.5 #:tags '()
;;  ... )
(define-syntax (with-timer stx)
  (syntax-case stx ()
    [(_ #:name name #:sample-rate sample-rate #:tags tags body ...)
     (time-body (syntax/loc stx name) (syntax/loc stx sample-rate)
                (syntax/loc stx tags) (syntax/loc stx (body ...)))]
    [(_ #:name name #:tags tags body ...)
     (time-body (syntax/loc stx name) #f (syntax/loc stx tags)
                (syntax/loc stx (body ...)))]
    [(_ #:name name #:sample-rate sample-rate body ...)
     (time-body (syntax/loc stx name) (syntax/loc stx sample-rate)
                #f (syntax/loc stx (body ...)))]
    [(_ #:name name body ...)
     (time-body (syntax/loc stx name) #f #f (syntax/loc stx (body ...)))]))


;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit
           racket/list
           racket/udp
           "buffer.rkt"
           "socket.rkt")

  ;; Create out own testing socket and listener
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
    (with-timer #:name "rkt.timer" #:tags '("lambda")
      (for ([i (range 200)]) (add1 i)))
    (check-regexp-match #rx"rkt.timer:(\\d\\.)|ms|#lambda"
                        (bytes->string/utf-8 (get-datagram))))

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

  (udp-close s)
  (sock-close))
