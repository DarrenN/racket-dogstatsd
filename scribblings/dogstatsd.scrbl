#lang scribble/manual

@require[racket/base
         scribble/eval
         @for-label[racket/base
                    racket/contract
                    racket/udp
                    dogstatsd]]

@title{dogstatsd}
@author[(author+email "Darren Newton" "info@v25media.com")]

@defmodule[dogstatsd]

@section{Introduction}

Provides a Racket @hyperlink["https://docs.datadoghq.com/developers/dogstatsd/"]{DogStatsD} client for sending metrics to @hyperlink["https://www.datadoghq.com/"]{Datadog}. DogStatsD is an extension of the @hyperlink["https://github.com/b/statsd_spec"]{StatsD} protocol developed by @hyperlink["https://github.com/etsy/statsd"]{Etsy} for metrics aggregation. DogStatsD requires that you have the @hyperlink["https://docs.datadoghq.com/agent/"]{Datadog Agent} running on the host, or another service listening for @hyperlink["https://en.wikipedia.org/wiki/User_Datagram_Protocol"]{udp} traffic on port 8125.

@bold{Example:}

@#reader scribble/comment-reader
(racketblock
(require racket/list
         dogstatsd)

(module+ main
  ;; Create a socket on 127.0.0.1:8125 which the Agent listens on
  (sock-create)
  
  (define (send-times)
    ;; Wrap some code with a timer metric - the elapsed time (ms) for the execution
    ;; of the body will be sent to the agent
    (with-timer "rkt.timer" #:tags '("proc:send-times" "proc:with-timer")
      (let ([xs (range 10)])
        (for ([i xs])
          ;; Send a counter metric
          (counter "rkt.counter" i #:tags '("proc:send-times"
                                            "aeon:12")
                     #:sample-rate 0.25))))
    (println (current-seconds))
    (sleep 2)
    (send-times))
  (send-times)))

@section{Setup a socket}

@defproc[(sock-create
          [#:host-name? host-name string? "127.0.0.1"]
          [#:host-port? host-port number? 8125])
          udp?]{
  Create and open a @racket[udp?] socket for host-name on host-port. You should only do this once in your program.
}

@defproc[(sock-send
          [metric string?])
          boolean?]{
  Manually send a metric string on the socket created with @racket[sock-create]. This is a low-level building block used by the metric procedures.
}

@defproc[(sock-close)
         void?]{
  Close the currently open socket.
}

@section{Metrics}

@defproc[(counter
          [name string?]
          [value number?]
          [#:sample-rate sample-rate number? #f]
          [#:tags tags (listof string?) #f])
          void?]{
  Send a DogStatsD @hyperlink["https://docs.datadoghq.com/developers/dogstatsd/#counters"]{Counter} metric. Counters track how many times something happens per second, like page views or downloads.

  @bold{Examples:}

  @#reader scribble/comment-reader
  (racketblock
        ;; Simple counter
        (counter "dogeapp.downloads" 5)

        ;; Counter with tags
        (counter "dogeapp.dowloads" 5 #:tags '("size:small" "type:shibainu"))

        ;; Counter with sample-rate
        (counter "dogeapp.dowloads" 5 #:sample-rate 0.25) 
  )
}

@defproc[(gauge
          [name string?]
          [value number?]
          [#:sample-rate sample-rate number? #f]
          [#:tags tags (listof string?) #f])
          void?]{
  Send a DogStatsD @hyperlink["https://docs.datadoghq.com/developers/dogstatsd/#gauges"]{Gauge} metric. Gauges track the ebb and flow of a particular metric value over time, like the number of active users on a website.

  @bold{Examples:}

  @#reader scribble/comment-reader
  (racketblock
        ;; Simple gauge
        (gauge "dogeapp.users.active" (get-active-users))

        ;; Gauge with tags
        (gauge "dogeapp.users.active" (get-active-users) #:tags '("type:shibainu"))

        ;; Gauge with sample-rate
        (gauge "dogeapp.users.active" (get-active-users) #:sample-rate 0.25) 
  )
}

@defproc[(histogram
          [name string?]
          [value number?]
          [#:sample-rate sample-rate number? #f]
          [#:tags tags (listof string?) #f])
          void?]{
  Send a DogStatsD @hyperlink["https://docs.datadoghq.com/developers/dogstatsd/#histograms"]{Histogram} metric. Histograms calculate the statistical distribution of any kind of value. For instance y can track distributions for anything, like the size of files users upload to your site.

  @bold{Examples:}

  @#reader scribble/comment-reader
  (racketblock
        ;; Simple histogram
        (histogram "dogeapp.uploads.filesize" file-size)

        ;; Histogram with tags
        (histogram "dogeapp.uploads.filesize" file-size #:tags '("type:shibainu"))

        ;; Histogram with sample-rate
        (histogram "dogeapp.uploads.filesize" file-size #:sample-rate 0.45)
  )
}

@defproc[(set
          [name string?]
          [value (or/c string? number?)]
          [#:sample-rate sample-rate number? #f]
          [#:tags tags (listof string?) #f])
          void?]{
  Send a DogStatsD @hyperlink["https://docs.datadoghq.com/developers/dogstatsd/#sets"]{Set} metric. Sets count the number of unique elements in a group. To track the number of unique visitors to your site, use a set.

  @bold{Examples:}

  @#reader scribble/comment-reader
  (racketblock
        ;; Simple set
        (set "dogeapp.users.uniques" (current-user-id))

        ;; Set with tags
        (set "dogeapp.users.uniques" (current-user-id) #:tags '("type:shibainu")) 

        ;; Set with sample-rate
        (set "dogeapp.users.uniques" (current-user-id) #:sample-rate 0.75)         
  )
}

@section{Timers}

@hyperlink["https://docs.datadoghq.com/developers/dogstatsd/#timers"]{Timers} measure the amount of time a section of code takes to execute, like the time it takes to render a web page. This module provides a manual @racket[timer] procedure and a @racket[with-timer] macro you can wrap code with.

@defproc[(timer
          [name string?]
          [value number?]
          [#:sample-rate sample-rate number? #f]
          [#:tags tags (listof string?) #f])
          void?]{
  Manually send a DogStatsD @hyperlink["https://docs.datadoghq.com/developers/dogstatsd/#timers"]{Histogram} metric. The value should be the milliseconds you wish to record. Read the documentation on Timers, as they create Histogram metrics automatically.

  @bold{Examples:}

  @#reader scribble/comment-reader
  (racketblock
        (define timed (... some procedure that returns milliseconds you wish to record ...))

        ;; Simple timer
        (timer "dogeapp.page.render" timed)

        ;; Timer with tags
        (timer "dogeapp.page.render" timed #:tags '("type:shibainu"))     

        ;; Timer with sample-rate
        (timer "dogeapp.page.render" timed #:sample-rate 0.45)
  )
}


@defform[
  #:id with-timer
  (with-timer name tags-clause sample-clause
              body ...)
  #:grammar ([name metric-name ...]
             [tags-clause (code:line)
               (code:line #:tags (tags-id:id))
               (code:line #:tags (tags-id:id) ...)]
             [sample-clause (code:line)
               (code:line #:sample-rate sample-id:id)
               (code:line #:sample-rate sample-id:id ...)])
  #:contracts ([name string?]
               [tags-clause (listof string?)]
               [sample-clause number?])
]{
  @racket[with-timer] provides an easy to use macro for timing chunks of code. Wrapping your function or let-body will automatically time its execution and send the metric. It also returns the value of the executed body clause, so you can use it in a @racket[define] or @racket[let] if you like.

  @bold{Example:}

  @#reader scribble/comment-reader
  (racketblock
    ;; Time the execution of code in milliseconds and send metric to Agent
    (with-timer "dogeapp.page.render" #:tags '("type:threaded")
      ;; code you want to instrument here
      (...))

    ;; Used in a define, will set timed-value to the result of (call-databass)
    ;; and send the timing metric to the Agent
    (define timed-value (with-timer "dogeapp.databass.call" (call-databass ...)))
  )
}

@section{Buffers}

@section{Events}

@section{Service Checks}
