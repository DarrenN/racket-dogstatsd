#lang racket/base

(require racket/string
         racket/udp
         threading
         "parameters.rkt"
         "utils.rkt")

(provide event)

;; _e{title.length,text.length}:title|text|d:timestamp|h:hostname|p:priority|t:alert_type|#tag1,tag2

(define (create-event-title-text title text)
  (format "_e{~a,~a}:~a|~a" (string-length title) (string-length text) title
          text))

;; Ensure priority is only ever normal or low
;; (-> string? string?)
(define (default-priority [str "normal"])
  (if (or (equal? str "normal") (equal? str "low"))
      str
      "normal"))

;; Ensure alert-type is ‘error’, ‘warning’, ‘info’ or ‘success’. Default ‘info’.
;; (-> string? string?)
(define (default-alert-type [str "info"])
  (define types '("info" "error" "warning" "success"))
  (if (null? (filter (λ (s) (equal? s str)) types))
      "info"
      str))

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

;; Build event metric bytes
(define (event title text
               #:timestamp [timestamp #f]
               #:hostname [hostname #f]
               #:aggregation-key [aggregation-key #f]
               #:priority [priority "normal"]
               #:source-type-name [source-type-name #f]
               #:alert-type [alert-type "info"]
               #:tags [tags #f])
  (udp-send*
   (get-sock)
   (string->bytes/utf-8
    (~>> (create-event-title-text title text)
         (append-event-metric "d" timestamp)
         (append-event-metric "h" hostname)
         (append-event-metric "k" aggregation-key)
         (append-event-metric "p" (default-priority priority))
         (append-event-metric "s" source-type-name)
         (append-event-metric "t" (default-alert-type alert-type))
         (append-tags tags)))))

;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit
           racket/list
           "parameters.rkt"
           "statsd.rkt")

  ;; Create out own testing socket and listener
  (define tsock (create-socket #:host-port 8126))
  (define s (udp-open-socket #f #f))
  (udp-bind! s #f 8126 #t)

  (define (get-datagram)
    (define buffer (make-bytes 1024))
    (define-values (length host port)
      (udp-receive! s buffer 0))
    (subbytes buffer 0 length))

  (test-case "default-priority ensures normal or low"
    (check-equal? (default-priority "normal") "normal")
    (check-equal? (default-priority "low") "low")
    (check-equal? (default-priority #f) "normal"))

  (test-case "default-alert-type ensures correct type"
    (for ([type '("info" "error" "warning" "success")])
      (check-equal? (default-alert-type type) type))
    (check-equal? (default-alert-type "low") "info")
    (check-equal? (default-alert-type 1) "info")
    (check-equal? (default-alert-type #f) "info"))

  (test-case "event assembles title & text with counts"
    (define timestamp (current-seconds))
    (define hostname "COMP:1001")
    (define agg-key "thx1138")
    (define priority "HIGH")
    (define source-type "web")
    (define alert-type "FATAL")

    (event "water" "the pack ad")
    (check-equal? (get-datagram)
                  #"_e{5,11}:water|the pack ad|p:normal|t:info")


    (event "water" "the pack ad" #:timestamp timestamp)
    (check-equal? (get-datagram)
                  (string->bytes/utf-8
                   (format "_e{5,11}:water|the pack ad|d:~a|p:normal|t:info"
                           timestamp)))


    (event "water" "the pack ad" #:hostname hostname)
    (check-equal? (get-datagram)
                  (string->bytes/utf-8
                   (format "_e{5,11}:water|the pack ad|h:~a|p:normal|t:info"
                           hostname)))

    (event "water" "the pack ad" #:aggregation-key agg-key)
    (check-equal? (get-datagram)
                  (string->bytes/utf-8
                   (format "_e{5,11}:water|the pack ad|k:~a|p:normal|t:info"
                           agg-key)))

    (event "water" "the pack ad" #:source-type-name source-type)
    (check-equal? (get-datagram)
                  (string->bytes/utf-8
                   (format "_e{5,11}:water|the pack ad|p:normal|s:~a|t:info"
                           source-type)))

    ; invalid priority
    (event "water" "the pack ad" #:priority "crazy")
    (check-equal? (get-datagram)
                  (string->bytes/utf-8
                   "_e{5,11}:water|the pack ad|p:normal|t:info"))

    ; invalid alert-type
    (event "water" "the pack ad" #:alert-type "WHOOP")
    (check-equal? (get-datagram)
                  (string->bytes/utf-8
                   "_e{5,11}:water|the pack ad|p:normal|t:info"))

    (event "water" "the pack ad" #:tags '("state:tx" "zip:77036"))
    (check-equal? (get-datagram)
                  (string->bytes/utf-8
                   "_e{5,11}:water|the pack ad|p:normal|t:info|#state:tx,zip:77036")))

  (test-case "append-event-metric assembles cleansed text"
    (check-equal? (append-event-metric "a" "ba" "ab") "ab|a:ba")))
