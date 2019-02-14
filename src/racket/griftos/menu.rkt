#lang racket/base

(require (for-syntax racket/base) racket/class racket/contract racket/bool racket/list racket/stxparam "objects.rkt" "telnet.rkt")

(provide vars (rename-out [transmit/param transmit] [set-state!/param set-state!] ) msg make-menu menu% new-telnet-menu)


(define-syntax-parameter set-state!/param (syntax-rules ()))
(define-syntax-parameter transmit/param (syntax-rules ()))
(define-syntax-parameter vars (syntax-rules ()))

(define-syntax-parameter msg (syntax-rules ()))


    
    

(define menu%
  (class* temp-object% (conn<%>)
    (super-new)
    (init-field [state #f]
                [transmit-func void])
    (init [vars (make-hasheq)])
    (field [vars-field vars])
          
    (abstract enter delta)
    
    (define/public (transmit . messages)
      (apply transmit-func messages))

    (define/public (receive message)
      (when (string? message)
        (define new-state (delta message))
        (when (symbol? new-state)
          (set-state! new-state))))
  
    (define/public (set-state! new-state #:allow-epsilon? [allow-epsilon? #t] #:trigger? [trigger? #t])
      (set! state new-state)
      (when trigger?
        (define epsilon-state (enter))
        (when (and allow-epsilon? (symbol? epsilon-state))
          (set-state! epsilon-state #:allow-epsilon? #f))))))

(define (make-telnet-transmitter tn)
  (define (transmit . messages)
    (for ([message (in-list messages)])
      (if (and (cons? message) (eq? (first message) 'menu-switch))
          (let ([new-menu (second message)])
            (set-field! transmit-func new-menu transmit)
            (set-telnet-user-data! tn new-menu))
          (telnet-send tn message))))
  transmit)

(define (new-telnet-menu menu-class% tn [vars (make-hasheq)] [state #f])
  (define new-vars (hash-copy vars))
  (hash-set! new-vars 'telnet tn)
  (new menu-class%
       [transmit-func (make-telnet-transmitter tn)]
       [state state]))

(define-syntax (make-menu stx)
  (syntax-case stx ()
    [(_ initial-state state-clause ...)
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
                     [(delta-body ...) delta-body-code]
                     [.../2 (quote-syntax ...)]) ; lol nested templates are funny
         #`(class menu%
             (super-new)
             (inherit-field state vars-field)
             (inherit transmit set-state!)
             (unless state (send this set-state! 'initial-state))
             (define/override (enter)
               (define vars/param vars-field)
               (syntax-parameterize
                   ([vars (syntax-id-rules () [_ vars/param])]
                    [transmit/param (syntax-rules ()
                                      [(transmit/param a .../2)
                                       (transmit a .../2)])]
                    [set-state!/param (syntax-rules ()
                                        [(set-state!/param new-state kw .../2)
                                         (set-state! new-state kw .../2)])])
                 (case state
                   enter-body ...
                   [else #f])))
             (define/override (delta msg/param)
               (define vars/param vars-field)
               (syntax-parameterize
                   ([vars (syntax-id-rules () [_ vars/param])]
                    [transmit/param (syntax-rules ()
                                      [(transmit/param a .../2)
                                       (transmit a .../2)])]
                    [set-state!/param (syntax-rules ()
                                        [(set-state!/param new-state kw .../2)
                                         (set-state! new-state kw .../2)])]
                    [msg (syntax-id-rules () [_ msg/param])])
                 (case state
                   delta-body ...
                   [else #f]))))))]))