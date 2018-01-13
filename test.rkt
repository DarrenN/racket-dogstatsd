#lang racket/base

(require racket/list
         dogstatsd)

(module+ main
  (sock-create)
  (define (send-times)
    (with-timer "rkt.timer" #:tags '("proc:send-times" "proc:with-timer")
      (let ([xs (range 10)])
        (for ([i xs])
          (counter "rkt.counter" i #:tags '("proc:send-times"
                                            "aeon:12")
                     #:sample-rate 0.25))))
    (println (current-seconds))
    (sleep 2)
    (send-times))

  (send-times))
