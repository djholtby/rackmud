#lang racket/base

(require compiler/compiler compiler/option compiler/cm racket/place racket/set racket/exn
         racket/file racket/path)

(provide compiler-place-main)

(define (compiler-place-main p-chan)
  (define mudlib/path (place-channel-get p-chan))
  (define mudlib-collect (place-channel-get p-chan))
  (define initial-build? (place-channel-get p-chan))
  (define changes (box '()))
  (define recompiled? (box #f))
  (parameterize ([current-library-collection-paths (if mudlib/path
                                                       (cons  mudlib/path
                                                              (current-library-collection-paths))
                                                       (current-library-collection-paths))]
                 [compile-notify-handler (λ (file-being-compiled)
                                           (printf "  compiling ~a\n"
                                                   (path->string file-being-compiled))
                                           (set-box! changes (cons file-being-compiled
                                                                   (unbox changes)))
                                           (set-box! recompiled? #t))]
                 [compile-enforce-module-constants #f])
    (define failed? #f)
    
    (define (do-compilation)
      (displayln "Compiling mudlib")
      (with-handlers ([exn? (λ (e)
                              (place-channel-put p-chan (exn->string e)))])

        (compile-directory-zos (collection-path (symbol->string mudlib-collect))
                               (λ (name thunk)
                                 (if (eq? name 'name) (symbol->string mudlib-collect) (thunk)))
                               #:verbose #f
                               #:skip-doc-sources? #t))
      (displayln "Compilation completed"))

    (when initial-build?
      (do-compilation))
      
      
    (unless failed?
      (place-channel-put p-chan #t)
    
      (let loop ()
        (set-box! changes '()) 
        (sync p-chan)
        (do-compilation)
        (place-channel-put p-chan (map bytes->path (unbox changes)))
        (loop)))))

    
    
