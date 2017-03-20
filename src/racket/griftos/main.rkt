(module griftos racket/base
  (require racket/base
           racket/class
           racket/undefined
           "racket-mud.rkt")
  (#%provide
   (all-from-except racket/base
                    struct
                    define-struct ;; probably need more to cut
                    )
   (all-from-except racket/class) ;; I'm sure there's stuff that should be cut...
   (all-from racket/undefined)
   (all-from-except "racket-mud.rkt"
                    define-mud-struct
                    )
   (rename  define-mud-struct struct)
   ))

