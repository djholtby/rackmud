#lang racket/base

(require racket/class telnet/connection racket/match racket/list racket/string racket/contract racket/hash
         "master.rkt" "objects.rkt" "backtrace.rkt")

(provide outer-connection<%> inner-connection<%> conn-mixin)

(define outer-connection<%>
  (interface (terminal<%>)))

(define inner-connection<%>
  (interface (conn<%>)
    [set-outer-connection (->*m ((is-a?/c/rackmud outer-connection<%>)) ; mandatory
                                #:rest (listof any/c)           ; rest
                                any/c)]))                       ; range

(define conn-mixin
  (mixin (terminal<%>) (outer-connection<%>)
    (super-new)
    (inherit supports?)
    (inherit-field markup-settings)
    ;(will-register object-executor this (λ (o) (eprintf "CONN DESTROYED\n")))

    ; TODO: move this to on-connect
    ;(hash-union! markup-settings (send master-object get-server-markup-settings))
    
    (init-field [inner-connection #f])

    (define/override (receive message)
      (define pass-on?
      (match message
        [(list 'enable where 'mssp)
         (transmit (list 'mssp (send/rackmud master-object get-mssp)))]
        [(list 'enable 'local 'mxp)
         (transmit (string-append "\e[1z" (string-join (send/rackmud master-object get-mxp-preamble)) "\e[7z\n"))
         (transmit 
;          "\e[4z<SUPPORT>\e[4z<VERSION>"
          ;          "\e[4z<info type='settings'><MXP ENABLED>\e[4z</info>\n"
          '(text () (info ((type "settings")) "<MXP ENABLED>\n"))
          )
         #t]
        [(or (? eof-object?) #f)
         (set! inner-connection #f)
         (unless (eq? master-object 'shutting-down)
           (send/rackmud master-object on-disconnect this))
         #t]
        [else #t]))
      (when (and pass-on? inner-connection (send/rackmud inner-connection connected?))
        (with-handlers ([exn? (λ (e) (log-message (current-logger) 'error (exn-message e) (backtrace e)))])
          (send/rackmud inner-connection receive message))
        ))

    (define/override (transmit . messages)
      (match messages
        #|[(list (list 'menu-switch menu% vars state))
         (define menu (new-telnet-menu menu% this vars))
         (set! inner-connection menu)
         (void (send menu set-state! state))]
        [(list (list 'player-switch p))
         (send p set-connection this)
         (set! inner-connection p)]|#
        [(list (list-rest 'switch-inner-connection new-connection args))
         (set! inner-connection new-connection)
         (send/rackmud new-connection set-outer-connection this . args)]
        [else (super transmit . messages)]))
    ))

