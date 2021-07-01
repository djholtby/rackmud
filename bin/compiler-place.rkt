#lang racket/base

(require compiler/compiler compiler/option racket/place racket/set racket/exn)

(provide compiler-place-main)

(define (compiler-place-main p-chan)
  (define mudlib/path (place-channel-get p-chan))
  (define mudlib-collect (place-channel-get p-chan))
  (define initial-build? (place-channel-get p-chan))
  
  (parameterize ([current-library-collection-paths (if mudlib/path
                                                       (cons  mudlib/path (current-library-collection-paths))
                                                       (current-library-collection-paths))]
                 [compile-enforce-module-constants #f])
    (define failed? #f)
    (define mudlib-collect-regexp
      (regexp (format "^\\s*checking: (~a.*)$" (regexp-quote (path->string (collection-path (symbol->string mudlib-collect)))))))
    (define changes (box '()))
    (define filtered-output-port
      (let ([o (current-output-port)])
        (make-output-port 'filtered-out
                          o
                          (λ (bs start end non-blocking? enable-breaks?)
                            (define m (regexp-match #px"^\\s*compiling (.*)$"
                                                    (subbytes bs start end)))
                            (when m
                              (write-bytes bs o start end)
                              (displayln "" o)
                              (set-box! changes (cons (cadr m) (unbox changes))))
                            (- end start))
                          (λ () (close-output-port o)))))

    (when initial-build?
      (displayln "Compiling mudlib")
      (parameterize ([current-output-port filtered-output-port])
        (with-handlers ([exn? (λ (e)
                                (place-channel-put p-chan (exn->string e)))])
          (compile-directory-zos (collection-path (symbol->string mudlib-collect))
                                 (λ (name thunk)
                                   (if (eq? name 'name) (symbol->string mudlib-collect) (thunk)))
                                 #:verbose #t
                                 #:skip-doc-sources? #t))))
    (unless failed?
      (place-channel-put p-chan #t)
    
      (let loop ()
        (set-box! changes '()) 
        (sync p-chan)
        (with-handlers ([exn? (λ (e)
                                (place-channel-put p-chan (exn-message e))
                                (loop))])
          (parameterize ([current-output-port filtered-output-port])
            (compile-directory-zos (collection-path (symbol->string mudlib-collect))
                                   (λ (name thunk)
                                     (if (eq? name 'name) (symbol->string mudlib-collect) (thunk)))
                                   #:verbose #t
                                   #:skip-doc-sources? #t)))
        (place-channel-put p-chan (map bytes->path (unbox changes)))
        (loop)))))

    
    
