#lang racket/base

(provide rackmud:domain rackmud:telnet-port rackmud:ssl-telnet-port rackmud:http-port rackmud:https-port rackmud:servlet-path
         rackmud:websock-path rackmud:websock-client-path rackmud:web-root-path rackmud:proxy-mode)

(define rackmud:domain (make-parameter #f))
(define rackmud:telnet-port (make-parameter #f))
(define rackmud:ssl-telnet-port (make-parameter #f))
(define rackmud:http-port (make-parameter #f))
(define rackmud:https-port (make-parameter #f))
(define rackmud:servlet-path (make-parameter #f))
(define rackmud:websock-path (make-parameter #f))
(define rackmud:websock-client-path (make-parameter #f))
(define rackmud:web-root-path (make-parameter #f))
(define rackmud:proxy-mode (make-parameter #f
                                           (λ (v)
                                             (unless (memq v '(#f apache nginx other))
                                               (raise-argument-error 'rackmud:proxy-mode
                                                                     "(or/c #f 'apache 'nginx 'other)"
                                                                     v))
                                             v)))