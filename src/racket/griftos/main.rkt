#lang racket/base

(require (for-syntax racket/base)
         racket/class
         racket/undefined
         racket/async-channel
         racket/local
         racket/match
         racket/list
         racket/promise
         racket/contract
         racket/bool
         racket/set
         racket/string
         racket/file
         racket/logging
         racket/hash
         json
         "master.rkt"
         "objects.rkt" (for-syntax "objects.rkt")
         "telnet.rkt"
         "msdp.rkt"
         "syncq.rkt"
         "pq.rkt"
         "scheduler.rkt"
         "logger.rkt"
         "charset.rkt"
         )

(provide
 (for-syntax (all-from-out racket/base))
 
 
 (except-out (all-from-out racket/class)
             send get-field set-field! new instantiate make-object is-a? is-a?/c
             ) ;; I'm sure there's stuff that should be cut...
 (all-from-out racket/base racket/undefined
               racket/async-channel
               racket/match
               racket/local
               racket/list
               racket/promise
               racket/contract
               racket/bool
               racket/set
               racket/string
               racket/file
               racket/logging
               racket/hash
               json)
   
   
   
 (all-from-out "msdp.rkt")
 (all-from-out "telnet.rkt")
 (all-from-out "master.rkt")
 (all-from-out "syncq.rkt")
 (all-from-out "pq.rkt")
 (all-from-out "scheduler.rkt")
 (all-from-out "logger.rkt")
 (all-from-out "charset.rkt")
  
 (except-out (all-from-out "objects.rkt") 
             send/griftos
             get-field/griftos
             set-field!/griftos
             new/griftos
             make-object/griftos
             instantiate/griftos
             is-a?/griftos
             is-a?/c/griftos)
   
 (rename-out  
  [send/griftos send]
  [get-field/griftos get-field]
  [set-field!/griftos set-field!]
  [new/griftos new]
  [instantiate/griftos instantiate]
  [make-object/griftos make-object]
  [is-a?/griftos is-a?]
  [is-a?/c/griftos is-a?/c]
  ))


(provide griftos-version-string set-griftos-version-string!)
(define griftos-version-string "")
(define (set-griftos-version-string! s)
  (set! griftos-version-string s))


(module reader syntax/module-reader griftos)