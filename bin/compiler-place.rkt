#lang racket/base

(require compiler/compiler compiler/option compiler/cm racket/place racket/set racket/exn
         racket/file racket/path racket/system
         "parallel-build.rkt")

(provide compiler-place-main)

(define (try-cdr p)
  (if (pair? p) (cdr p) p))

(define (compiler-place-main p-chan)
  (define options (place-channel-get p-chan))
  (define extra-collects-path (try-cdr (assq 'extra-collects-path options )))
  (define initial-build? (try-cdr (assq 'compile-on-launch? options)))
  (define mudlib-collect (try-cdr (assq 'mudlib-collect options)))
  (define runtime-collects-path (try-cdr (assq 'runtime-collects-path options)))

  (unless (and mudlib-collect runtime-collects-path)
    (place-channel-put p-chan #f)
    (error 'compiled-place "invalid options"))
  
  (define errors '())
  (define changes (mutable-set))

  (define (handler id type path msg out err)
   (case type
     [(compile)
      (unless (set-member? changes path)
        (printf "  making ~a\n" path)
        (set-add! changes path))]
     [(error fatal-error) (set! errors
                                    (cons
                                     (list path msg out err)
                                     errors))]))

  
  (parameterize ([current-library-collection-paths (if extra-collects-path
                                                       (cons  extra-collects-path
                                                              (current-library-collection-paths))
                                                       (current-library-collection-paths))])
    (define mudlib-collection-path
      (collection-path (symbol->string mudlib-collect)))
    (define mudlib-runtime-path
      (path->directory-path
       (simplify-path (path->complete-path
                       (build-path runtime-collects-path (symbol->string mudlib-collect))))))


    (define mcp/exploded (explode-path mudlib-collection-path))
    (define mrp/exploded (explode-path mudlib-runtime-path))
    
    (define (copy-to-runtime-path)
      (system (format "rsync -qtru '~a' '~a'"
                      (path->string mudlib-collection-path)
                      (path->string runtime-collects-path))))

    (define (try-remove-suffix lst suff)
      (let loop ([lst lst]
                 [suff suff])

        (cond [(null? suff) lst]
              [(equal? (car lst) (car suff))
               (loop (cdr lst) (cdr suff))]
              [else #f])))
    
    (define (changeset->runtime-paths cset)
      (set-map cset
               (λ (path)
                 (let* ([ex (explode-path path)]
                        [remains (try-remove-suffix ex mcp/exploded)])
                   (if remains
                       (apply build-path (append mrp/exploded remains))
                       path)))))
                   
    
    
    (define success? #f)
    
    (define (do-compilation)
      (displayln "Compiling mudlib")
      (set! success?
            (parallel-compile-directory
             mudlib-collection-path
             #:collection-paths (if extra-collects-path
                                    (cons extra-collects-path
                                          (current-library-collection-paths))
                                    (current-library-collection-paths))
             #:handler handler
             #:options `(disable-constant)))
      (unless success?
        (place-channel-put p-chan
                           (cons 'errors errors)))
      (printf "Compilation finished.  Success=~a\n" (void? success?)))

    (when initial-build?
      (do-compilation))
    (copy-to-runtime-path)
      
    (when success?
      (place-channel-put p-chan #t)
    
      (let loop ()
        (set-clear! changes) 
        (set! errors '())
        (sync p-chan)
        (do-compilation)
        (when success?
          (copy-to-runtime-path)
          (place-channel-put p-chan (cons 'changes (changeset->runtime-paths changes))))
        ;(place-channel-put p-chan (if success? (unbox changes) '()))
        (loop)))))

    
    
