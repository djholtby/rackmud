#lang info
(define collection "rackmud")
(define deps '("uuid"
               "telnet"
               "base"
               "data-lib"
               "db-lib"
               "gregor-lib"
               "rfc6455"
               "versioned-box"
               "web-server-lib"))
(define racket-launcher-names '("rackmud"))
(define racket-launcher-libraries '("bin/rackmud-main.rkt"))
