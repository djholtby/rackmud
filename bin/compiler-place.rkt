#lang racket/base

(require compiler/compiler compiler/option racket/place racket/set racket/exn)

(provide compiler-place-main)

(define (compiler-place-main p-chan)
  (define mudlib/path (place-channel-get p-chan))
  (define mudlib-collect (place-channel-get p-chan))
  (define collection-files (mutable-set))
  (parameterize ([current-library-collection-paths (if mudlib/path
                                                       (cons  mudlib/path (current-library-collection-paths))
                                                       (current-library-collection-paths))]
                 [compile-enforce-module-constants #f])
    (define failed? #f)
    (define mudlib-collect-regexp
      (regexp (format "^\\s*checking: (~a.*)$" (regexp-quote (path->string (collection-path (symbol->string mudlib-collect)))))))
    
    (displayln "Compiling mudlib")
    
    (parameterize ([current-output-port
                    (let ([o (current-output-port)])
                      (make-output-port 'filtered-out
                                        o
                                        (λ (bs start end non-blocking? enable-breaks?)
                                          (let [(m (regexp-match mudlib-collect-regexp (subbytes bs start end)))]
                                            (when m
                                              (set-add! collection-files (bytes->path (cadr m)))))
                                          (when (regexp-match #px"^\\s*compiling" (subbytes bs start end))
                                            (write-bytes bs o start end)
                                            (displayln "" o))
                                          (- end start))
                                        (λ () (close-output-port o))))
                    ])
      (with-handlers ([exn? (λ (e)
                              (place-channel-put p-chan (exn->string e)))])
        (compile-directory-zos (collection-path (symbol->string mudlib-collect))
                               (λ (name thunk)
                                 (if (eq? name 'name) (symbol->string mudlib-collect) (thunk)))
                               #:verbose #t
                               #:skip-doc-sources? #t))
      (unless failed?
        (place-channel-put p-chan #t)
        
        (let loop ()
          (define changes '())
          ;(apply sync (map filesystem-change-evt (set->list collection-files)))
          (sync p-chan)
          (sleep 1)
          (with-handlers ([exn? (λ (e) (log-message (current-logger) 'error (format "error recompiling mudlib: ~a" (exn-message e))
                                                    (exn-continuation-marks e)))])
            (parameterize ([current-output-port
                            (let ([o (current-output-port)])
                              (make-output-port 'filtered-out
                                                o
                                                (λ (bs start end non-blocking? enable-breaks?)
                                                  (let [(m (regexp-match mudlib-collect-regexp (subbytes bs start end)))]
                                                    (when m
                                                      (set-add! collection-files (bytes->path (cadr m)))))
                                                  (let ([m (regexp-match #px"^\\s*compiling (.*)$" (subbytes bs start end))])
                                                    (when m
                                                      (set! changes (cons (bytes->path (cadr m)) changes))))
                                                  (- end start))
                                                (λ () (close-output-port o))))])
              (compile-directory-zos (collection-path (symbol->string mudlib-collect))
                                     (λ (name thunk)
                                       (if (eq? name 'name) (symbol->string mudlib-collect) (thunk)))
                                     #:verbose #t
                                     #:skip-doc-sources? #t)))
          (place-channel-put p-chan changes)
          (loop))))))

    
    
  