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
         versioned-box
         json
         "master.rkt"
         "objects.rkt" (for-syntax "objects.rkt")
         "connection.rkt"
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
             send send* get-field set-field! new instantiate make-object is-a? is-a?/c
             object?
             object=?
             object-or-false=?
             object->vector
             object-interface
             object-method-arity-includes?
             field-names
             object-info

             with-method
             dynamic-send
             send/keyword-apply
             send/apply
             dynamic-get-field
             dynamic-set-field!
             field-bound?
             class-field-accessor
             class-field-mutator


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
               json
               versioned-box)
   
   
   
 (all-from-out "msdp.rkt")
 (all-from-out "telnet.rkt")
 (all-from-out "connection.rkt")
 (all-from-out "master.rkt")
 (all-from-out "syncq.rkt")
 (all-from-out "pq.rkt")
 (all-from-out "scheduler.rkt")
 (all-from-out "logger.rkt")
 (all-from-out "charset.rkt")
  
 (except-out (all-from-out "objects.rkt") 
             send/griftos
             send*/griftos
             get-field/griftos
             set-field!/griftos
             new/griftos
             make-object/griftos
             instantiate/griftos
             is-a?/griftos
             is-a?/c/griftos

             object?/griftos
             object=?/griftos
             object-or-false=?/griftos
             object->vector/griftos
             object-interface/griftos
             object-method-arity-includes?/griftos
             field-names/griftos
             object-info/griftos

             with-method/griftos
             dynamic-send/griftos
             send/keyword-apply/griftos
             send/apply/griftos
             dynamic-get-field/griftos
             dynamic-set-field!/griftos
             field-bound?/griftos
             class-field-accessor/griftos
             class-field-mutator/griftos
             )

 
 (rename-out  
  [send/griftos send]
  [send*/griftos send*]
  [get-field/griftos get-field]
  [set-field!/griftos set-field!]
  [new/griftos new]
  [instantiate/griftos instantiate]
  [make-object/griftos make-object]
  [is-a?/griftos is-a?]
  [is-a?/c/griftos is-a?/c]
  [object?/griftos object?]
  [object=?/griftos object=?]
  [object-or-false=?/griftos object-or-false=?]
  [object->vector/griftos object->vector]
  [object-interface/griftos object-interface]
  [object-method-arity-includes?/griftos object-method-arity-includes?]
  [field-names/griftos field-names]
  [object-info/griftos object-info]
  [with-method/griftos with-method]
  [dynamic-send/griftos dynamic-send]
  [send/keyword-apply/griftos send/keyword-apply]
  [send/apply/griftos send/apply]
  [dynamic-get-field/griftos dynamic-get-field]
  [dynamic-set-field!/griftos dynamic-set-field!]
  [field-bound?/griftos field-bound?]
  [class-field-accessor/griftos class-field-accessor]
  [class-field-mutator/griftos class-field-mutator]

  ))


(provide griftos-version-string set-griftos-version-string!)
(define griftos-version-string "")
(define (set-griftos-version-string! s)
  (set! griftos-version-string s))


(module reader syntax/module-reader griftos)
