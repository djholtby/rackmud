#!/usr/local/bin/racket
#lang racket

(require "config.rkt" setup/setup)

(define cfg (load-rackmud-settings))

(define mudlib (hash-ref cfg 'mudlib-path #f))
(define mudlib/path (and mudlib (path->complete-path (path->directory-path (simplify-path (string->path mudlib))))))
(define mudlib-collect (string->symbol (hash-ref cfg 'mudlib-collect "mudlib")))
(unless (module-path? mudlib-collect)
  (error 'mudlib-collection: "Expected module-path? but found ~a" mudlib-collect))
(define mudlib-module (hash-ref cfg 'master-module "main.rkt"))

(parameterize ([current-library-collection-links (if mudlib/path
                                                     (cons (hasheq mudlib-collect (list mudlib/path)) (current-library-collection-links))
                                                     (current-library-collection-links))])
  (setup #:collections `((,(symbol->string mudlib-collect)))))
  
  
