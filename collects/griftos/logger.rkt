#lang racket/base

(require (for-syntax racket/base) racket/list racket/contract racket/logging racket/syntax "objects.rkt")
(provide griftos-log)

(define (log-format msg . placeholders)
  (if (empty? placeholders)
      msg
      (apply format msg placeholders)))

(define-syntax (griftos-log stx)
  (syntax-case stx ()
    [(_ level topic message ...)
     (syntax/loc stx
       (log-message (current-logger) level topic (log-format message ...) (current-continuation-marks)))]))

