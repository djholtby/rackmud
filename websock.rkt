#lang racket/base

(require racket/class racket/port racket/bytes racket/list racket/match racket/string json
         net/rfc6455 xml "websock-tags.rkt")
(require telnet/connection telnet/mxp telnet/color)

(provide websock-terminal%)

(define (on? v)
  (eq? v 'on))


(define websock-terminal%
  (class* terminal% (terminal<%>)
    (super-new)
    (init [(websock-connection/init websock-connection)])
    (define websock-connection websock-connection/init)

    (define/override (connected?)
      (not (ws-conn-closed? websock-connection)))

    (inherit receive get-markup-settings)
    
    
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
      (memq option '(gmcp html)))

    (define (send-settings)
      (ws-send! websock-connection (jsexpr->string
                                    `#hasheq((type . "style")
                                             (style . ,(settings->CSS (get-markup-settings)))))))

    (define/override (set-markup-settings! settings)
      (super set-markup-settings! settings)
      (send-settings))
    (define/override (markup-settings-union! settings)
      (super markup-settings-union! settings)
      (send-settings))
    
    (define connection-thread
      (thread
       (lambda ()
         (with-handlers
             ([exn:break:hang-up? void])
           (let loop ()
             (define msg
               (with-handlers ([exn? (λ (e)
                                       (log-message (current-logger)
                                                    'error 'rackmud:websock
                                                    (exn-message e)
                                                    (exn-continuation-marks e) #f)
                                       eof)])
                 (ws-recv websock-connection #:payload-type 'text)))
             (if (eof-object? msg)
                 (begin
                   (receive msg)
                   (ws-close! websock-connection))
                 (let* ([msg/json (string->jsexpr (if (bytes? msg) (bytes->string/utf-8 msg) msg))]
                        [msg-type (hash-ref msg/json 'type "")])
                   (with-handlers ([exn? (λ (e)
                                           (log-message (current-logger) 'error #f
                                                        (exn-message e)
                                                        (exn-continuation-marks e) #f))])
                     (case (string->symbol msg-type)
                       [(text) (receive (hash-ref msg/json 'text))]
                       [(gmcp) (receive (list 'gmcp
                                              (hash-ref msg/json 'package)
                                              (hash-ref msg/json 'payload #f)))]
                       [(naws)
                        (let ([width (hash-ref msg/json 'width)]
                              [height (hash-ref msg/json 'height)])
                          (set-dimensions! width height)
                          (receive (list 'naws width height)))]
                       [else (log-message
                              (current-logger)
                              'debug 'rackmud:websock
                              (format "unhandled websocket message: ~v" msg/json) #f #f)]))
                   (loop))))))))
                                      
    
    (define/override (transmit . args)
      (with-handlers ([exn-expected? (λ (_)
                                       (receive eof)
                                       (break-thread connection-thread 'hang-up))]
                      [exn? (λ (e)
                              (log-message (current-logger)
                                           'error 'rackmud
                                           (format "~a\n~a" e (exn-message e))
                                           (exn-continuation-marks e)
                                           #f)
                              (receive eof)
                              (break-thread connection-thread 'hang-up))])
        (for ([msg (in-list args)])
          (if (ws-conn-closed? websock-connection)
              (begin (receive eof)
                     (break-thread connection-thread 'hang-up))
              (match msg
                [(or #f (? eof-object?))
                 (ws-close! websock-connection #:reason "Connection closed by server")
                 (receive eof)
                 (break-thread connection-thread 'hang-up)]
                [(? string?) (ws-send! websock-connection
                                       (jsexpr->string `#hasheq((type . "text")
                                                                (text . ,(xexpr->string msg)))))]
                [(? bytes?) (ws-send! websock-connection
                                      (jsexpr->string
                                       `#hasheq((type . "text")
                                                (text . ,(xexpr->string
                                                          (bytes->string/utf-8 msg))))))]
                ['nop
                 (ws-send! websock-connection
                           (jsexpr->string
                            '#hasheq((type . "command") (command . "ping"))))]
                [(? symbol?)
                 (ws-send! websock-connection (jsexpr->string
                                               `#hasheq((type . "command")
                                                        (command . ,(symbol->string msg)))))]
                [(list 'text contents ...)
                 (parameterize ([tag-settings (get-markup-settings)])
                   (ws-send! websock-connection (jsexpr->string
                                                 `#hasheq((type . "markup")
                                                          (text . ,(xexpr->string/settings msg))))))]
                [(list 'html tag)
                 (ws-send! websock-connection (jsexpr->string
                                               `#hasheq((type . "markup")
                                                        (text . ,(xexpr->string tag)))))]
                [(list 'gmcp package payload)
                 (ws-send! websock-connection (jsexpr->string
                                               `#hasheq((type . "gmcp")
                                                        (package . ,(symbol->string package))
                                                        (payload . ,payload))))]
                [(list 'gmcp package)
                 (ws-send! websock-connection (jsexpr->string
                                               `#hasheq((type . "gmcp")
                                                        (package . ,(symbol->string package))
                                                        (payload . #f))))])))))))

    
;; Yoinked from the websever library, comments and all baby
(define/match (exn-expected? _)
  [((or
     ;; This error is "Connection reset by peer" and doesn't really
     ;; indicate a problem with the server. It occurs when our end
     ;; doesn't "realize" that the connection was interrupted (for
     ;; whatever reason) and it attempts to send a packet to the other
     ;; end, to which the other end replies with an RST packet because
     ;; it wasn't expecting anything from our end.
     (exn:fail:network:errno _ _ (cons (or 54 104) 'posix))
     (exn:fail:filesystem:errno _ _ (cons (or 54 104) 'posix))
     ;; This error is "Broken pipe" and it occurs when our end attempts
     ;; to write to the other end over a closed socket. It can happen
     ;; when a browser suddenly closes the socket while we're sending
     ;; it data (eg. because the user closed a tab).
     (exn:fail:network:errno _ _ (cons 32 'posix))
     (exn:fail:filesystem:errno _ _ (cons 32 'posix))
     ;; This is error is not useful because it just means the other
     ;; side closed the connection early during writing, which we can't
     ;; do anything about.
     (exn:fail "fprintf: output port is closed" _)
     ;; The connection may get timed out while the request is being
     ;; read, when that happens we need to gracefully kill the
     ;; connection.
     (exn:fail (regexp #rx"input port is closed") _)
     ;; There is no explicit connection closing under HTTP/1.1 so we
     ;; attempt to read a a new request off of a connection as soon as
     ;; the previous one was handled.  If the client goes a way, then
     ;; this error gets triggered and we can safely ignore it.
     (exn:fail (regexp #rx"http input closed prematurely") _)))
   #t]
  [(_)
   #f])


(define (tag->html tag)
  (mxp-tag:websock-html-equiv (hash-ref (tags) tag (mxp-tag:websock tag 'inline #f '() #f 'span))))

(define (add-options-to-tags xpr)
  (let loop ([content xpr])
    (match content
      [(? string?) content]
      [(list 'br '()) content]
      [(? symbol?) `(,content ())]
      [(list tag (list params ...) body ...)
       (define tag-info (hash-ref (tags) tag #f))
       (define cust-rend (and tag-info (mxp-tag-custom-render tag-info)))
       (cond [cust-rend
              (define remapped-tag (cust-rend content 'html))
              (match remapped-tag
                [(list 'html (list new-tag new-params new-contents ...))
                 `(,new-tag ,new-params ,@(map loop new-contents))]
                [else (loop (cust-rend content 'html))])]
             [else
              `(,(tag->html tag)
                (,@(cons (list 'class (string-append "rackmud-" (symbol->string tag)))
                         (map (lambda (pair)
                                (if (eq? 'href (car pair))
                                    (list 'href (format "javascript:send_text(~v);" (cadr pair)))
                                    (cons (string->symbol (string-append
                                                           "data-mxp-"
                                                           (symbol->string (car pair))))
                                          (cdr pair))))
                              params)))
                ,@(map loop body))])]
      [else (log-message (current-logger) 'warning 'rackmud
                         (format "unknown response type: ~v" content) #f #f)
            (format "~a" content)])))

(define (xexpr->string/settings xpr)
  (xexpr->string (add-options-to-tags xpr)))




(define (byte->hex b)
  (if (< b 16)
      (string-append "0" (number->string b 16))
      (number->string b 16)))

(define (color->CSS fmc)
  (define rgb (or (font-color-rgb fmc)
                  (xterm->rgb (font-color-xterm fmc))
                  (xterm->rgb (font-color-ansi fmc))
                  ))
  (string-append "#"
                 (string-join (map byte->hex rgb) "")))


(define (fontmode->CSS fm)
  (define result (open-output-string))
  (define fg (fontmode-fore fm))
  (define bg (fontmode-back fm))
  (when fg
    (display "color: " result)
    (display (color->CSS fg) result)
    (displayln ";" result))
  (when bg
    (display "background-color: " result)
    (display (color->CSS bg) result)
    (displayln ";" result))
  (when (fontmode-italic fm)
    (display "font-mode: " result)
    (display (if (eq? (fontmode-italic fm) 'on) "italic" "normal") result)
    (displayln ";" result))
  (when (fontmode-bold fm)
    (display "font-weight: " result)
    (display (if (eq? (fontmode-bold fm) 'on) "bold" "normal") result)
    (displayln ";" result))
  (when (or (on? (fontmode-underline fm))
            (on?  (fontmode-strike fm)))
    (display "text-decoration:" result)
    (when (on? (fontmode-underline fm))
      (display " underline" result))
    (when (on? (fontmode-strike fm))
      (display " line-through" result))
    (displayln ";" result))
  (get-output-string result))

(define (settings->CSS settings)
  (define result (open-output-string))
  (for ([(tag setting) (in-hash settings)])
    (define tag/string (symbol->string tag))
    (define tag/split (regexp-match #rx"^([^#]*)\\[([^#=]*)=(.*)\\]$" tag/string))
    (define just-tag (if tag/split (second tag/split) tag/string))
    (define html-tag (hash-ref (tags) (string->symbol just-tag) #f))
    (define tag-selector (if html-tag (mxp-tag:websock-html-equiv html-tag) ""))
    (define maybe-attr (if tag/split (third tag/split) #f))
    (define maybe-val (if tag/split (fourth tag/split) #f))
    (displayln (format "~a.~a~a~a {\n~a}\n"
                       tag-selector "rackmud-" just-tag
                       (if maybe-attr (format "[data-mxp-~a=\"~a\"]" maybe-attr maybe-val) "")
                       (fontmode->CSS setting)
                       ) result))
  (get-output-string result))