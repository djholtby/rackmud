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
             send/rackmud
             send*/rackmud
             get-field/rackmud
             set-field!/rackmud
             new/rackmud
             make-object/rackmud
             instantiate/rackmud
             is-a?/rackmud
             is-a?/c/rackmud

             object?/rackmud
             object=?/rackmud
             object-or-false=?/rackmud
             object->vector/rackmud
             object-interface/rackmud
             object-method-arity-includes?/rackmud
             field-names/rackmud
             object-info/rackmud

             with-method/rackmud
             dynamic-send/rackmud
             send/keyword-apply/rackmud
             send/apply/rackmud
             dynamic-get-field/rackmud
             dynamic-set-field!/rackmud
             field-bound?/rackmud
             class-field-accessor/rackmud
             class-field-mutator/rackmud
             )

 
 (rename-out  
  [send/rackmud send]
  [send*/rackmud send*]
  [get-field/rackmud get-field]
  [set-field!/rackmud set-field!]
  [new/rackmud new]
  [instantiate/rackmud instantiate]
  [make-object/rackmud make-object]
  [is-a?/rackmud is-a?]
  [is-a?/c/rackmud is-a?/c]
  [object?/rackmud object?]
  [object=?/rackmud object=?]
  [object-or-false=?/rackmud object-or-false=?]
  [object->vector/rackmud object->vector]
  [object-interface/rackmud object-interface]
  [object-method-arity-includes?/rackmud object-method-arity-includes?]
  [field-names/rackmud field-names]
  [object-info/rackmud object-info]
  [with-method/rackmud with-method]
  [dynamic-send/rackmud dynamic-send]
  [send/keyword-apply/rackmud send/keyword-apply]
  [send/apply/rackmud send/apply]
  [dynamic-get-field/rackmud dynamic-get-field]
  [dynamic-set-field!/rackmud dynamic-set-field!]
  [field-bound?/rackmud field-bound?]
  [class-field-accessor/rackmud class-field-accessor]
  [class-field-mutator/rackmud class-field-mutator]

  ))

(module reader syntax/module-reader rackmud/main)
