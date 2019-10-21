(module reader '#%kernel
  (#%require (submod racket/mud/main reader))
  (#%provide (all-from (submod racket/mud/main reader))))
