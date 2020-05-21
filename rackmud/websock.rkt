#lang racket/base

(require racket/class racket/port racket/bytes racket/list racket/match racket/string racket/set json net/rfc6455)
(require telnet/connection telnet/mxp)

(provide websock-terminal%)

(define websock-terminal%
  (class* terminal% (terminal<%>)
    (super-new)
    (init-field websock-connection)

    (define/override (connected?)
      (not (ws-conn-closed? websock-connection)))

    (inherit receive)
    (inherit-field markup-settings)
    
    (define terminal-dimensions '(80 . 24))
    (define/override (dimensions)
      terminal-dimensions)

    (define/public (set-dimensions! width height)
      (set! terminal-dimensions (cons width height)))

    (define/override (get-encoding)
      'UTF-8)

    (define/override (set-encoding! enc)
      (eq? enc 'UTF-8))

    (define/override (supports? option)
      (memq option '(color xterm-256 true-color gmcp)))
    
    (define connection-thread
      (thread
       (lambda ()
         (let loop ()
           (define msg
             (with-handlers ([exn? (λ (e) (eprintf "~v\n" e) eof)])
               (ws-recv websock-connection #:payload-type 'text)))
           (if (eof-object? msg)
               (begin
                 (receive msg)
                 (ws-close! websock-connection))
               (let* ([msg/json (string->jsexpr (if (bytes? msg) (bytes->string/utf-8 msg) msg))]
                      [msg-type (hash-ref msg/json 'type "")])
                 (case (string->symbol msg-type)
                   [(text) (receive (hash-ref msg/json 'payload))]
                   [(gmcp) (receive (list 'gmcp (hash-ref msg/json 'package) (hash-ref msg/json 'payload #f)))]
                   [(naws)
                    (let ([width (hash-ref msg/json 'width)]
                          [height (hash-ref msg/json 'height)])
                      (set-dimensions! width height)
                      (receive (list 'naws width height)))]
                   [else (void)])
                 (loop)))))))
                                      
    
    (define/override (transmit . args)
      (for ([msg (in-list args)])
        (match msg
          [(or #f (? eof-object?))
           (ws-close! websock-connection #:reason "Connection closed by server")
           (receive eof)
           (kill-thread connection-thread)]
          [(? string?) (ws-send! websock-connection (jsexpr->string `#hasheq((type . "text") (text . ,msg))))]
          [(? bytes?) (ws-send! websock-connection (jsexpr->string `#hasheq((type . "text") (text . ,(bytes->string/utf-8 msg)))))]
          ['nop
           (ws-send! websock-connection "null")]
          [(? symbol?)
           (ws-send! websock-connection (jsexpr->string `#hasheq((type . "command") (command . ,(symbol->string msg)))))]
          [(list 'text contents ...)
           (ws-send! websock-connection (jsexpr->string `#hasheq((type . "markup") (text . ,(xexpr->string/settings msg markup-settings)))))]
          [(list 'gmcp package payload)
           (ws-send! websock-connection (jsexpr->string `#hasheq((type . "gmcp") (package . ,(symbol->string package)) (payload . ,payload))))]
          [(list 'gmcp package)
           (ws-send! websock-connection (jsexpr->string `#hasheq((type . "gmcp") (package . ,(symbol->string package)) (payload . #f))))])))))

    

