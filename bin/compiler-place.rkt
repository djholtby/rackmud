#lang racket/base

(require compiler/compiler compiler/option compiler/cm racket/place racket/set racket/exn
         racket/file racket/path
         "parallel-build.rkt")

(provide compiler-place-main)

(define (compiler-place-main p-chan)
  (define mudlib/path (place-channel-get p-chan))
  (define mudlib-collect (place-channel-get p-chan))
  (define initial-build? (place-channel-get p-chan))
  (define errors '())
  (define changes (mutable-set))

  (define (handler id type path msg out err)
   (case type
     [(compile)
      (unless (set-member? changes path)
        (printf "  making ~a\n" path)
        (set-add! changes path))]
                
                
     ;[(done) (set! changes (cons path changes))]
     [(error fatal-error) (set! errors
                                    (cons
                                     (list path msg out err)
                                     errors))]))

  
  (parameterize ([current-library-collection-paths (if mudlib/path
                                                       (cons  mudlib/path
                                                              (current-library-collection-paths))
                                                       (current-library-collection-paths))]
                 #|[compile-notify-handler (λ (file-being-compiled)
                                           (printf "  making ~a\n"
                                                   (path->string file-being-compiled))
                                           (set-add! changes file-being-compiled))]
                 [compile-enforce-module-constants #f]|#
                 )
    (define success? #f)
    
    (define (do-compilation)
      (displayln "Compiling mudlib")
      #|(set! success? #f)
      (with-handlers ([exn? (λ (e)
                              (place-channel-put p-chan (list 'errors e)))])

        (compile-directory-zos (collection-path (symbol->string mudlib-collect))
                               (λ (name thunk)
                                 (if (eq? name 'name) (symbol->string mudlib-collect) (thunk)))
                               #:verbose #f
                               #:skip-doc-sources? #t)
        (set! success? #t))|#
      
      (set! success?
            (parallel-compile-directory
             (collection-path (symbol->string mudlib-collect))
             #:collection-paths (if mudlib/path
                                    (cons mudlib/path (current-library-collection-paths))
                                    (current-library-collection-paths))
             #:alternate-compile-path (string->path ".rackmud_compiled_tmp")
             #:copy-back? #t
             #:handler handler
             #:options `(disable-constant)))
      (unless success?
        (place-channel-put p-chan
                           (cons 'errors errors)))
      (printf "Compilation finished.  Success=~a\n" (void? success?)))

    (when initial-build?
      (do-compilation))
      
      
    (when success?
      (place-channel-put p-chan #t)
    
      (let loop ()
        (set-clear! changes) 
        (set! errors '())
        (sync p-chan)
        (do-compilation)
        (when success? (place-channel-put p-chan (cons 'changes (set->list changes))))
        ;(place-channel-put p-chan (if success? (unbox changes) '()))
        (loop)))))

    
    
