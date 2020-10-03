#lang racket/base

(provide backtrace)

(define (backtrace cms)
  (cond [(continuation-mark-set? cms)
         (let ([out (open-output-string)])
           (for ([context (in-list (continuation-mark-set->context cms))])
             (displayln (format "~a : ~a" (or (car context) "???") (if (srcloc? (cdr context)) (srcloc->string (cdr context))
                                                                       "No source information available")) out))
           (get-output-string out))]
        [(exn? cms)
         (backtrace (exn-continuation-marks cms))]
        [else #f]))

