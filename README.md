racket-dogstatsd
================

## [DogStatsD](https://docs.datadoghq.com/guides/dogstatsd/) client for [Racket](https://racket-lang.org/)

**Very hack. Use at your peril.**

```clojure
(with-timer #:name "rkt.timer" #:tags '("proc:send-times" "proc:with-timer")
    ;; code you wanna time here:
    (let ([xs (range (+ 10000 (random 100000)))])
         (histogram "rkt.histogram" (length xs) #:tags '("proc:send-times"
                                                         "aeon:12"))
         (for ([i xs])
           (* i i 3.141)))
       (println (current-seconds)))
```

You can see a simple example of `with-timer` in `test.rkt` and all the of statsd functions are available.

![good dog](https://media1.popsugar-assets.com/files/thumbor/eH6B03ksVYYPLFJAgmC4u2_ihFI/fit-in/1024x1024/filters:format_auto-!!-:strip_icc-!!-/2015/10/19/826/n/1922243/a00d1cad_edit_img_image_16686166_1444860712_12142682_526874914145470_1831035987_n/i/DIY-Halloween-Costumes-Dogs.jpg)
