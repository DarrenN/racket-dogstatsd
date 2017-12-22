DogStatsD
=========

## [DogStatsD](https://docs.datadoghq.com/guides/dogstatsd/) client for [Racket](https://racket-lang.org/)

```racket
(with-timer #:name "rkt.timer" #:tags '("proc:send-times" "proc:with-timer")
  (let ([xs (range (+ 10000 (random 100000)))])
    (histogram "rkt.histogram" (length xs) #:tags '("proc:send-times"
               "aeon:12"))
      (for ([i xs])
        (* i i 3.141)))
  (println (current-seconds)))
```


You can see a simple example of `with-timer` in `test.rkt` and all the of statsd functions are available.
