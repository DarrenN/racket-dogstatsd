DogStatsD
=========

[![Build Status](https://travis-ci.org/DarrenN/racket-dogstatsd.svg?branch=master)](https://travis-ci.org/DarrenN/racket-dogstatsd)
[![Coverage Status](https://coveralls.io/repos/github/DarrenN/racket-dogstatsd/badge.svg?branch=master)](https://coveralls.io/github/DarrenN/racket-dogstatsd?branch=master)

## [DogStatsD](https://docs.datadoghq.com/guides/dogstatsd/) client for [Racket](https://racket-lang.org/)

<img src="https://datadog-prod.imgix.net/img/presskit/DDlogo.jpg?dpr=2" width="200" height="200" alt="DataDog" />

```racket
#lang racket/base

(require racket/list
         dogstatsd)

(with-timer #:name "rkt.timer" #:tags '("proc:send-times" "proc:with-timer")
  (let ([xs (range 50)])
    (for ([i xs])
      (counter "rkt.counter" i #:tags '("proc:send-times"
                                        "aeon:12")
               #:sample-rate 0.25))))
```


You can see a simple example of `with-timer` in `test.rkt` and all the of statsd functions are available.

**Note**: Requires Racket 6.3 and above.
