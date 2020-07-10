#lang racket/base

(require (for-syntax racket/base) racket/class racket/contract racket/bool racket/list racket/stxparam "objects.rkt" "connections.rkt"
         "master.rkt"
         telnet/connection racket/match)

;(provide vars (rename-out [transmit/param transmit] [set-state!/param set-state!] ) msg make-menu menu% new-telnet-menu)
(provide menu% menu+connection% define-menu-fsm)

#|(define-syntax-parameter set-state!/param (syntax-rules ()))
(define-syntax-parameter transmit/param (syntax-rules ()))
(define-syntax-parameter vars (syntax-rules ()))

(define-syntax-parameter msg (syntax-rules ()))
|#

(define set-state!/key (generate-member-key))
(define get-state/key (generate-member-key))
(define delta/key (generate-member-key))

(define-member-name set-state! set-state!/key)
(define-member-name get-state get-state/key)
(define-member-name delta delta/key)



(define menu%
  (class* temp-object% (conn<%>)
    (super-new)
    (init [initial-state #f]
          [enter-on-create? #f])
   
    (define state initial-state)
    
    (abstract enter delta transmit)

    (define/public (connected?) #t)
    
    (define/public (receive message)
      (when (string? message)
        (define new-state (delta message))
        (when (symbol? new-state)
          (set-state! new-state))))

    (define/public (get-state)
      state)
    
    (define/public (set-state! new-state #:allow-epsilon? [allow-epsilon? #t] #:trigger? [trigger? #t])
      (set! state new-state)
      (when trigger?
        (define epsilon-state (enter))
        (when (and allow-epsilon? (symbol? epsilon-state))
          (set-state! epsilon-state #:allow-epsilon? #t))))

    (when enter-on-create? (enter))))



(define menu+connection%
  (class* menu% (inner-connection<%>)
    (define outer-connection #f)
    

    (define/override (transmit . messages)
      (void (when outer-connection
              (send/rackmud outer-connection transmit . messages))))

    (define/public (get-outer-connection)
      outer-connection)
    
    (define/public (set-outer-connection oconn [new-state #f] [delay #f])
      (set! outer-connection oconn)
      (if new-state
          (if delay
              (queue-event delay (send this set-state! new-state))
              (send this set-state! new-state))
          (if delay
              (queue-event delay (send this enter))
              (send this enter))))
    
    (super-new)))

(define-syntax (define-menu-fsm stx)
  (syntax-case stx ()
    [(_ (msg) state-clause ...)
       (let ([enter-body-code '()]
             [delta-body-code '()])
         (for ([ccstx (in-list (syntax->list #'(state-clause ...)))])
           (syntax-case ccstx ()
             ([state enter-body delta-body]
              (set! enter-body-code
                    (cons
                     (syntax-case #'enter-body ()
                       ([expr ...]
                        (syntax/loc #'enter-body
                          [(state) expr ...])))
                     enter-body-code))
              (set! delta-body-code
                    (cons
                     (syntax-case #'delta-body ()
                       ([expr ...]
                        (syntax/loc #'delta-body
                          [(state) expr ...])))
                     delta-body-code)))))
         (with-syntax ([(enter-body ...) enter-body-code]
                       [(delta-body ...) delta-body-code])
           #'(begin
               (define/override (enter)
                 (case (send this get-state)
                   enter-body ...
                   [else #f]))
               (define/override (delta msg)
                (case (send this get-state)
                   delta-body ...
                   [else #f])))))]))