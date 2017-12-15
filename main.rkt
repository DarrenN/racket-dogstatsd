#lang racket/base

(require "./private/statsd.rkt")
(require "./private/events.rkt")
(require "./private/servicechecks.rkt")

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here

(module+ test
  ;; Tests to be run with raco test
  (create-socket)
  (event "cthulhu.rkt" "Test event for HackDay" #:source-type-name "racket"
         #:tags '("proc:main-test"))
  (service-check "cthulhu.service" OK #:timestamp (current-seconds)
                 #:tags '("proc:main-test")
                 #:message "CSD has found a number of Old Ones available!")
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
