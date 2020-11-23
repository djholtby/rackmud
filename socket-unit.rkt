#lang racket/base

(require net/tcp-sig racket/contract racket/unix-socket racket/unit web-server/web-server
         web-server/http/xexpr web-server/servlet-env
         web-server/servlet-dispatch)

(provide/contract
 [tcp-unix-socket@ (-> unix-socket-path? (unit/c (import) (export tcp^)))])

(define (tcp-unix-socket@ usp)
  (unless (unix-socket-path? usp)
    (raise-argument-error 'tcp-unix-socket@ "unix-socket-path?" usp))
  (define-unit @
    (import)
    (export tcp^)

    (define (tcp-listen port max-allow-wait reuse? hostname)
      (unix-socket-listen usp max-allow-wait))

    (define (tcp-connect hostname port-no [local-hostname #f] [local-port-no #f])
      (unix-socket-connect usp))

    (define (tcp-connect/enable-break hostname port-no [local-hostname #f] [local-port-no #f])
      (unix-socket-connect usp))

    (define (tcp-accept listener)
      (unix-socket-accept listener))

    (define (tcp-accept/enable-break listener)
      (unix-socket-accept listener))
      
    (define (tcp-accept-ready? listener)
      (unix-socket-listener? (sync/timeout 0 listener)))
    
    (define (tcp-close listener)
      (void))

    (define tcp-listener? unix-socket-listener?)

    (define tcp-abandon-port void)

    (define (tcp-addresses tcp-port [port-numbers? #f])
      (if port-numbers? (values "" 1 "" 1) (values "" ""))))
  @)
