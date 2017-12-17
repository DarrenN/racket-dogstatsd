#lang racket/base

(require racket/string
         threading)

(provide append-event-metric
         append-sample-rate
         append-tags
         create-name-value-type
         escape-tags
         escape-line-breaks
         sample-within-bounds?)

;//////////////////////////////////////////////////////////////////////////////
; PUBLIC

;; Ensure a tag values list doesn't contain spaces
;; (-> list? list?)
(define (escape-tags tags)
  (map (λ (s) (string-replace (string-trim s) " " "_")) tags))

;; \n must be converted to \\n
;; (-> string? string?)
(define (escape-line-breaks str)
  (string-trim (string-replace str #px"\n" "\\n")))

;; Metric names cannot contain : | @
;; (-> string? string?)
(define (escape-metric-name str)
  (string-trim (string-replace str #rx"[\\|:@]*" "")))

;; Build a basic name:value|type string
;; (-> string? (U number? string?) string? string? string?)
(define (create-name-value-type name value type)
  (string-trim (format "~a:~a|~a" (escape-metric-name name) value type)))

;; Ensure sample is < 1 and > 0
;; (-> number? bool?)
(define (sample-within-bounds? sample-rate)
  (and (number? sample-rate) (< sample-rate 1) (> sample-rate 0)))

;; Only append sample-rate if a number between 0 and 1
;; (-> (U bool? number?) string? string?)
(define (append-sample-rate sample-rate str)
  (if (sample-within-bounds? sample-rate)
      (format "~a|@~a" str sample-rate)
      str))

;; Append tags to metric string
;; (-> (U bool? list?) string? string?)
(define (append-tags tags str)
  (if (and (not (null? tags)) (list? tags))
      (format "~a|#~a" str (string-join (escape-tags tags) ","))
      str))

;; Append a separator, prefix and value to metric to str, used in event
;; (-> string? string? string? string?)
(define (append-event-metric prefix value str)
  (if value
      (format "~a|~a:~a" str prefix (escape-line-breaks (format "~a" value)))
      str))


;//////////////////////////////////////////////////////////////////////////////
; TESTS

(module+ test
  (require rackunit
           racket/list)

  (test-case "escape-metric-name removes : | @"
    (check-equal? (escape-metric-name "foo:bar|baz@quux") "foobarbazquux")
    (check-equal? (escape-metric-name "foo.bar.baz") "foo.bar.baz")
    (check-equal? (escape-metric-name "foo.bar:baz") "foo.barbaz"))

  (test-case "append-tags"
    (define base1 "spc:eco|c")
    (check-equal? (append-tags '() base1) base1)
    (check-equal? (append-tags '("song:fallapartforlove") base1)
                  (format "~a|#song:fallapartforlove" base1))
    (check-equal? (append-tags '("song:die" "song:alone") base1)
                  (format "~a|#song:die,song:alone" base1))
    (check-equal? (append-tags '("song:die alone" "song:full flow  ") base1)
                  (format "~a|#song:die_alone,song:full_flow" base1)))

  (test-case "append-sample-rate only appends if the value is between 0 and 1"
    (check-equal? (append-sample-rate 0.3 "tricky") "tricky|@0.3")
    (check-equal? (append-sample-rate 1 "tricky") "tricky")
    (check-equal? (append-sample-rate 0 "tricky") "tricky")
    (check-equal? (append-sample-rate "maxinquaye" "tricky") "tricky"))

  (test-case "create-name-value-type formats a string"
    (check-equal? (create-name-value-type "tricky" "maxinquaye" "triphop")
                  "tricky:maxinquaye|triphop")
    (check-equal? (create-name-value-type "tricky" 0.33 "triphop")
                  "tricky:0.33|triphop")
    (check-equal? (create-name-value-type "tricky:is|cool" 0.33 "triphop")
                  "trickyiscool:0.33|triphop"))

  (test-case "escape-tags swaps spaces for underscores and trims"
    (check-equal? (escape-tags'("little:dragon man" " little:dragon  "
                                                    "little    dragon"))
                  '("little:dragon_man" "little:dragon" "little____dragon")))

  (test-case "escape-line-breaks remove line-breaks"
    (define str "  This is
the coolest

string ever
")
    (check-equal? (escape-line-breaks str) "This is\\nthe coolest\\n\\nstring ever\\n")))
