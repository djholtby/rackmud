(module reader '#%kernel
  (#%require (submod griftos reader))
  (#%provide (all-from (submod griftos reader))))
