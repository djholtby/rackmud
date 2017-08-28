#lang racket/base

(require racket/base
         racket/class
         racket/undefined
         "racket-mud.rkt")

(provide
   (except-out (all-from-out racket/base)
                    struct
                    define-struct ;; probably need more to cut
                    )
   (except-out (all-from-out racket/class)) ;; I'm sure there's stuff that should be cut...
   (all-from-out racket/undefined)

   (except-out (all-from-out "racket-mud.rkt") 
               define-mud-struct)
   (rename-out  [define-mud-struct struct]))


(module reader syntax/module-reader
  griftos)
