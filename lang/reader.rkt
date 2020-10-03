(module reader '#%kernel
  (#%require (submod rackmud/main reader))
  (#%provide (all-from (submod rackmud/main reader))))
