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
    (define filtered-output-port
      (let ([o (current-output-port)])
        (make-output-port 'filtered-out
                          o
                          (λ (bs start end non-blocking? enable-breaks?)
                            (when (regexp-match #px"^\\s*compiling" (subbytes bs start end))
                              (write-bytes bs o start end)
                              (displayln "" o))
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
        (define changes '())
        (sync p-chan)
        (sleep 1)
        (with-handlers ([exn? (λ (e)
                                (log-message (current-logger) 'error (format "error recompiling mudlib: ~a" (exn-message e))
                                             (exn-continuation-marks e))
                                (place-channel-put p-chan (exn-message e))
                                (loop))])
          (parameterize ([current-output-port filtered-output-port])
            (compile-directory-zos (collection-path (symbol->string mudlib-collect))
                                   (λ (name thunk)
                                     (if (eq? name 'name) (symbol->string mudlib-collect) (thunk)))
                                   #:verbose #t
                                   #:skip-doc-sources? #t)))
        (place-channel-put p-chan changes)
        (loop)))))

    
    
