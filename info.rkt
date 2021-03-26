#lang info

(define collection "rackmud")
(define pkg-name "rackmud")
(define pkg-authors '(djholtby))
(define version "0.01")
(define deps '("no-echo"
               "net-cookies-lib"
               "net-lib"
               "parser-tools-lib"
               "unix-signals"
               "unix-socket-lib"
               "charset"
               "net-jwt"
               "readline-lib"
               "sha"
               
;               "parser-tools-lib"
               "telnet"
               "uuid"
               "base"
               "data-lib"
               "db-lib"
               "gregor-lib"
               "rfc6455"
               "versioned-box"
               "web-server-lib"))

(define build-deps '("racket-doc"
                     "scribble-lib"))

(define racket-launcher-names '("rackmud"))
(define racket-launcher-libraries '("bin/rackmud-main.rkt"))

