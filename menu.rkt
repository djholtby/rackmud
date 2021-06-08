#lang racket/base

(require (for-syntax racket/base) racket/class racket/contract  "objects.rkt" "connections.rkt"
         "master.rkt"
         telnet/connection racket/match no-echo)


(provide menu% terminal-menu% menu+connection% define-menu-fsm has-setup-menu<%>)

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
          [enter-on-create? #f]
          [enter-epsilon? #f])
   
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
    
    (define/public (set-state! new-state
                               #:allow-epsilon? [allow-epsilon? #t]
                               #:trigger? [trigger? #t])
      (set! state new-state)
      (when trigger?
        (define epsilon-state (enter))
        (when (and allow-epsilon? (symbol? epsilon-state))
          (set-state! epsilon-state #:allow-epsilon? #t))))

    (when enter-on-create?
      (define enter-result (enter))
      (when (and enter-epsilon? (symbol? enter-result))
        (set-state! enter-result)))))

(define terminal-menu%
  (class menu%
    (init [in (current-input-port)]
          [out (current-output-port)]
          [stars? #t])
    (define in-port in)
    (define out-port out)
    (define use-stars? stars?)
    (define echo? #t)
    (define conn? #t)
    (define return-status #f)
    (super-new)

    (define/override (connected?) conn?)

    
    (define/public (get-return-status)
      return-status)

    (define/public (set-return-status! v)
      (set! return-status v))

    (define/override (transmit . messages)
      (for ([message (in-list messages)])
        (match message
          ['echo (set! echo? #t)]
          ['no-echo (set! echo? #f)]
          [(? string?) (display message out-port)]
          [(? number?) (display message out-port)]
          [(? eof-object?) (set! return-status #f) (set! conn? #f)]
          [else (set! return-status message) (set! conn? #f)])))
    
    (define/public (input-loop)
      (let loop ()
        (when (send this connected?)
          (let ([line (if echo?
                          (read-line in-port)
                          (read-line/password #:in in-port #:out out-port #:stars? use-stars?))])
            (unless (eof-object? line)
              (send this receive line)
              (loop)))))
      return-status)))
    
(define has-setup-menu<%>
  (interface (master<%>)
    [setup-complete? (->m any/c)]
    [run-setup-menu! (->m input-port? output-port? any/c)]))

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
