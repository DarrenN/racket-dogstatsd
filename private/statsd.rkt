#lang racket/base

(require racket/string
         racket/udp
         threading
         (for-syntax racket/base
                     racket/syntax)
         "parameters.rkt"
         "utils.rkt")

(provide create-socket
         counter
         guage
         histogram
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

;; (-> string? (U string? number?) string? number? list? bytes?)
(define (create-metric name value type #:sample-rate [sample-rate #f]
                       #:tags [tags #f])
  (let* ([sample (check-sample-rate sample-rate type)])
    (string->bytes/utf-8
     (~>> (create-name-value-type name value type)
          (append-sample-rate sample)
          (append-tags tags)))))

;; [Macro]
;; Bind a proc to the metric type
;; We fire and forget udp datagrams (udp-send*)
(define-syntax (define/metric stx)
  (syntax-case stx ()
    [(_ procname type)
     #'(define (procname name value #:sample-rate [sample-rate #f] #:tags
                         [tags #f])
         (udp-send*
          (get-sock) (create-metric name value type
                                    #:sample-rate sample-rate
                                    #:tags tags)))]))

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

;; Create a UDP socket and make available to metric functions
(define (create-socket #:host-name [host-name "127.0.0.1"]
                       #:host-port [host-port 8125])
  (update-sock (udp-open-socket))
  (let ([sock (get-sock)])
    (udp-connect! sock host-name host-port)
    sock))

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
           racket/list)

  (define s (create-socket))
  (define (slow) (for ([i (range (random 4))]) (sleep 1)))

  (with-timer #:name "rkt.timer" #:tags '("lambda")
    (slow))

  (test-case "timer works"
    (timer "rkt.timer" 23))


  (test-case "check-sample-rate returns sample-rate or #f"
    (check-false (check-sample-rate 0.12 GAUGE))
    (check-false (check-sample-rate 0.12 SET))
    (check-equal? (check-sample-rate 0.12 COUNTER) 0.12)
    (check-equal? (check-sample-rate 0.12 HISTOGRAM) 0.12)
    (check-equal? (check-sample-rate 0.12 TIMER) 0.12))

  (test-case "all metric procs return #t"
    (check-true (guage "rkt.guage" (random 100)))
    (check-true (set "rkt.set" (random 100)))
    (check-true (counter "rkt.counter" (random 20)))
    (check-true (histogram "rkt.histogram" (random 100)))
    (check-true (timer "rkt.timer" (current-inexact-milliseconds)))))
