#lang racket/base

(require racket/contract racket/logging "objects.rkt")
(provide griftos-log)

(define/contract
  (griftos-log level message [data (current-continuation-marks)] #:topic [topic #f])
  (-> log-level/c string? any/c #:topic (or/c #f symbol?) void?)
  (log-message (current-logger) level topic message data #f))


