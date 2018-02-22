#lang racket/base

(require racket/base
         racket/class
         racket/undefined
         racket/async-channel
         racket/local
         racket/match
         racket/list
         json
         "master.rkt"
         "objects.rkt"
         "telnet.rkt"
         "msdp.rkt")

(provide
   (except-out (all-from-out racket/base)
                    struct
                    define-struct ;; probably need more to cut
                    )
   (except-out (all-from-out racket/class)) ;; I'm sure there's stuff that should be cut...
   (all-from-out racket/undefined)
   (all-from-out racket/async-channel)
   (all-from-out racket/match)
   (all-from-out racket/local)
   (all-from-out racket/list)
   (all-from-out json)

   (all-from-out "msdp.rkt")
   (all-from-out "telnet.rkt")
   (all-from-out "master.rkt")
   
   (except-out (all-from-out "objects.rkt") 
               define-mud-struct)
   (rename-out  [define-mud-struct struct]))


(module reader syntax/module-reader griftos)
