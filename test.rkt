#lang racket/base

(require racket/list
         "./private/statsd.rkt")

(module+ main
  (create-socket)
  (define (send-times)
    (with-timer #:name "rkt.timer" #:tags '("proc:send-times" "proc:with-timer")
      (time
       (let ([xs (range (+ 10000 (random 100000)))])
         (histogram "rkt.histogram" (length xs) #:tags '("proc:send-times"
                                                         "aeon:12"))
         (for ([i xs])
           (* i i 3.141)))))
    (sleep 0.5)
    (send-times))

  (send-times))
