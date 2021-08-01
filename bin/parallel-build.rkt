#lang racket/base

(require compiler/cm
         compiler/compilation-path
         racket/list
         racket/file
         racket/match
         racket/path
         racket/serialize
         setup/parallel-do
         racket/class
         compiler/find-exe
         racket/place
         syntax/modresolve
         setup/path-to-relative
         (for-syntax racket/base))

(provide parallel-compile-files parallel-compile-directory)

(define-syntax-rule (DEBUG_COMM a ...)
  (void)
  ;  (begin a ...)
  )

(define (path-begins-with? path prefix)
  (let loop
    ([path (explode-path path)]
     [prefix (explode-path prefix)])
    (cond [(null? prefix) #t]
          [(null? path) #f]
          [(equal? (car path) (car prefix))
           (loop (cdr path) (cdr prefix))]
          [else #f])))

(define (format-error exn
                      #:long? [long? #t]
                      #:to-string? [to-string? #f]
                      #:cache [pkg-path-cache #f])
  (let loop ([to-string? to-string?])
    (cond
      [to-string?
       (define sp (open-output-string))
       (parameterize ([current-error-port sp])
         (loop #f))
       (regexp-replace #rx"\n$" (get-output-string sp) "")]
      [long?
       ((make-compilation-context-error-display-handler
         (lambda (str exn)
           ((error-display-handler)
            str
            exn)))
        (exn-message exn)
        exn)]
      [else
       (eprintf "~a\n" (exn-message exn))
       (define path (continuation-mark-set-first (exn-continuation-marks exn)
                                                 managed-compiled-context-key))
       (when (path-string? path)
         (eprintf "  compiling: ~a"
                  (path->relative-string/setup path #:cache pkg-path-cache)))])))



(define lock-manager% 
  (class object%
    (init-field worker-count)
    (field (locks (make-hash)))
    (define depends (make-hash))
    (define currently-idle 0)
    (define/private (idle! delta)
      (set! currently-idle (+ currently-idle delta)))
    (define/public (lock fn wrkr)
      (let ([v (hash-ref locks fn #f)])
        (hash-set!
         locks fn
         (match v
           ['done
            (wrkr/send wrkr (list 'compiled))
            'done]
           [(list w waitlst) 
            (let ([fns (check-cycles w (hasheq wrkr #t) null)])
              (cond
                [fns
                 (wrkr/send wrkr (list 'cycle (cons fn fns)))
                 v]
                [else
                 (idle! +1)
                 (hash-set! depends wrkr (cons w fn))
                 (list w (append waitlst (list wrkr)))]))]
           [_
            (wrkr/send wrkr (list 'locked))
            (list wrkr null)]))
        (not v)))
    (define/public (unlock fn)
      (match (hash-ref locks fn)
        [(list w waitlst)
         (for ([x (second (hash-ref locks fn))])
           (idle! -1)
           (hash-remove! depends x)
           (wrkr/send x (list 'compiled)))
         (hash-set! locks fn 'done)]))
    (define/private (check-cycles w seen fns)
      (cond
        [(hash-ref seen w #f) fns]
        [(hash-ref depends w #f)
         => (lambda (d)
              (check-cycles (car d) (hash-set seen w #t) (cons (cdr d) fns)))]
        [else #f]))
    (super-new)))

(define/class/generics lock-manager%
  (lm/lock lock fn wrkr)
  (lm/unlock unlock fn))

(struct parallel-compile-event (worker value) #:prefab)
;; Logger that joins the events of the compiler/cm logger of the different places.
;; The attached values are (parallel-compile-event <worker-id> <original-data>).
(define pb-logger (make-logger 'setup/parallel-build (current-logger)))



(define (->bytes x)
  (cond [(path? x) (path->bytes x)]
        [(string? x) (string->bytes/locale x)]
        [(equal? x 'relative) (->bytes (path->complete-path (current-directory)))]
        [else (raise-argument-error '->bytes "(or/c path? string? 'relative)" x)]))



(define file-list-queue% 
  (class* object% (work-queue<%>) 
    (init-field filelist handler options)
    (init worker-count)
    (field (lock-mgr (new lock-manager% [worker-count worker-count])))
    (field [results (void)])
    (inspect #f)

    (define seen
      (for/hash ([k (in-list filelist)])
        (values k #t)))

    (define/public (work-done work wrkr msg)
      (define id (send wrkr get-id))
      (match msg
        [(list result-type out err)
         (match result-type
           [(list 'LOCK fn) (lm/lock lock-mgr fn wrkr) #f]
           [(list 'UNLOCK fn) (lm/unlock lock-mgr fn) #f]
           [(list 'ERROR long-msg short-msg)
            (handler id 'error work long-msg out err)
            (set! results #f)
            #t]
           [(list 'LOG level msg data)
            (when (log-level? pb-logger level)
              (log-message pb-logger level msg (parallel-compile-event id data)))
            #f]
           ['COMPILE
            (handler id 'compile work "" "" "")
            #f]
           [(or 'SKIPPED 'DONE)
            (define (string-!empty? s) (not (zero? (string-length s))))
            (if (ormap string-!empty? (list out err))
                (handler id 'output work "" out err)
                (if (eq? result-type 'DONE)
                    (handler id 'done work "" "" "")
                    (handler id 'skipped work "" "" "")))
            #t])]
        [(list 'ADD fn)
         (unless (hash-ref seen fn #f)
           (set! filelist (cons fn filelist))
           (set! seen (hash-set seen fn #t)))
         #f]
        [_
         (handler id 'fatalerror (format "Error matching work: ~a queue ~a" work filelist) "" "") #t]))
           
    (define/public (get-job workerid)
      (match filelist
        [(cons hd tail)
         (define-values (dir file b) (split-path hd))
         (set! filelist tail)
         (handler workerid 'start hd "" "" "")
         (values hd (list (->bytes hd) (->bytes (path->complete-path hd)) options))]))

    (define/public (has-jobs?) (not (null? filelist)))
    (define/public (jobs-cnt) (length filelist))
    (define/public (get-results) results)
    (super-new)))


(define (parallel-build work-queue worker-count #:use-places? use-places?)
  (define do-log-forwarding (log-level? pb-logger 'info 'setup/parallel-build))
  (define options (get-field options work-queue))
  (parallel-do
   #:use-places? use-places?
   worker-count
   (lambda (workerid) (list workerid do-log-forwarding options))
   work-queue
   (define-worker (parallel-compile-worker worker-id do-log-forwarding options)
     (DEBUG_COMM (eprintf "WORKER ~a\n" worker-id))
     (define prev-uncaught-exception-handler (uncaught-exception-handler))
     (uncaught-exception-handler 
      (lambda (x)
        (when (exn:break? x) (exit 1))
        (prev-uncaught-exception-handler x)))

     
     (use-compiled-file-paths
      (let ([cfp
             (memf (λ (v) (and (pair? v)
                               (eq? 'compiled-file-paths (car v))))
                   options)])
        (if cfp
            (cons (cdar cfp) (use-compiled-file-paths))
            (use-compiled-file-paths))))
     
     (current-path->mode
      (let ([edirs
             (memf (λ (v) (and (pair? v) (eq? 'include-directory (car v)))) options)])
        (if edirs
            (λ (path)
              (if
               (path-begins-with? path (cdar edirs))
               (car (use-compiled-file-paths))
               (cadr (use-compiled-file-paths))))
            (current-path->mode))))

     (current-library-collection-paths
      (cdar (memf (λ (v) (and (pair? v) (eq? 'collection-paths (car v)))) options)))
     
     (current-load/use-compiled (make-compilation-manager-load/use-compiled-handler))
     (define cmc (make-caching-managed-compile-zo))
       
     (match-message-loop
      [(list name _full-file options)
       (DEBUG_COMM (eprintf "COMPILING ~a ~a ~a\n" worker-id name _full-file))
       (define full-file (bytes->path _full-file))
       (define-values (dir file _) (split-path full-file))
       (define out-str-port (open-output-string))
       (define err-str-port (open-output-string))
       (define cip (current-input-port))
       (define cop (current-output-port))
       (define cep (current-error-port))
       (define (send/recv msg) (send/msg msg) (recv/req))
       (define (send/add fn) (send/msg (list 'ADD fn)))
       (define (send/resp type)
         (send/msg (list type (get-output-string out-str-port) (get-output-string err-str-port))))
       (define did? #f)
       (define (notify x)
         (send/msg (list 'COMPILE "" ""))
         (set! did? #t))
       (define (lock-client cmd fn)
         (match cmd
           ['lock
            (DEBUG_COMM (eprintf "REQUESTING LOCK ~a ~a ~a\n" worker-id name _full-file))
            (match (send/recv (list (list 'LOCK (path->bytes fn)) "" ""))
              [(list 'locked) #t]
              [(list 'cycle fns)
               (error 'setup "dependency cycle: ~s" fns)]
              [(list 'compiled) #f]
              [(list 'DIE) (worker/die 1)]
              [x (send/error (format "DIDNT MATCH B ~v\n" x))]
              [_ (send/error (format "DIDNT MATCH B\n"))])]
           ['unlock 
            (DEBUG_COMM (eprintf "UNLOCKING ~a ~a ~a\n" worker-id name _full-file))
            (send/msg (list (list 'UNLOCK (path->bytes fn)) "" ""))]
           [x (send/error (format "DIDNT MATCH C ~v\n" x))]
           [_ (send/error (format "DIDNT MATCH C\n"))]))
       (with-handlers ([exn:fail? (lambda (x)             
                                    (send/resp (list 'ERROR
                                                     ;; Long form shows context:
                                                     (format-error x #:long? #t #:to-string? #t)
                                                     ;; Short form for summary omits context:
                                                     (format-error x #:long? #f #:to-string? #t))))])
         (parameterize ([parallel-lock-client lock-client]
                        [compile-context-preservation-enabled (memq 'disable-inlining options)]
                        [compile-enforce-module-constants (not (memq 'disable-constant options))]
                        [manager-trace-handler (if (member 'very-verbose options)
                                                   (lambda (p) (printf "  ~a\n" p))
                                                   (manager-trace-handler))]
                        [current-namespace (make-base-empty-namespace)]
                        [current-directory (if (memq 'set-directory options)
                                               dir
                                               (current-directory))]
                        [current-compile-target-machine (if (memq 'compile-any options)
                                                            #f
                                                            (current-compile-target-machine))]
                        [managed-recompile-only (if (memq 'recompile-only options)
                                                    #t
                                                    (managed-recompile-only))]
                        [current-load-relative-directory dir]
                        [current-input-port (open-input-string "")]
                        [current-output-port out-str-port]
                        [current-error-port err-str-port]
                        [manager-compile-notify-handler notify])
           
           ;; Watch for module-prefetch events, and queue jobs in response
           (define prefetch-thread (start-prefetch-thread send/add))
           ;; Watch for logging events, and send log messages to parent
           (define stop-logging-thread
             (if do-log-forwarding
                 (start-logging-thread send/log)
                 void))
             
           (cmc (build-path dir file))

           (kill-thread prefetch-thread)
           (stop-logging-thread))
         (send/resp (if did? 'DONE 'SKIPPED)))
       ]
      [x (send/error (format "DIDNT MATCH A ~v\n" x))]
      [_ (send/error (format "DIDNT MATCH A\n"))]))))


(define (parallel-compile-files list-of-files
                                #:worker-count [worker-count (processor-count)]
                                #:handler [handler void]
                                #:options [options '()]
                                #:use-places? [use-places? #t])
  (unless (exact-positive-integer? worker-count)
    (raise-argument-error 'parallel-compile-files "exact-positive-integer?" worker-count))
  (unless (and (list? list-of-files) (andmap path-string? list-of-files))
    (raise-argument-error 'parallel-compile-files "(listof path-string?)" list-of-files))
  (unless (and (procedure? handler) (procedure-arity-includes? handler 6))
    (raise-argument-error 'parallel-compile-files "(procedure-arity-includes/c 6)" handler))
  (parallel-build (make-object file-list-queue% list-of-files handler options worker-count) worker-count
                  #:use-places? use-places?))


(define (cons-if test val tail)
  (if test (cons val tail) tail))

(define (parallel-compile-directory dir
                                    #:alternate-compile-path [alt-compile-path #f]
                                    #:collection-paths [collection-paths (current-library-collection-paths)]
                                    #:copy-back? [copy-back? #f]
                                    #:filter [file-filter (λ (path) #t)]
                                    #:worker-count [worker-count (processor-count)]
                                    #:handler [handler void]
                                    #:options [options '()]
                                    #:use-places? [use-places? #t])
  
  (define list-of-files (find-files
                         (λ (path)
                           (and (file-filter path)
                                (member (path-get-extension path) '(#".rkt" #".ss" #".scribl"))))
                         dir))
  (define compiled-path/list (reverse (explode-path
                                       (if (pair? (use-compiled-file-paths))
                                           (car (use-compiled-file-paths))
                                           (string->path "compiled")))))

  (define (cons-if-not-present val lst)
    (if (member val lst) lst (cons val lst)))
    
  (define compiled-dirs
    (if alt-compile-path
        (let loop ([lst list-of-files]
                   [acc null])
          (if (null? lst)
              acc
              (loop (cdr lst)
                    (cons-if-not-present
                     (get-compilation-dir (car lst))
                     acc))))
        null))
    
  (define alt/list (and alt-compile-path (explode-path alt-compile-path)))

       

  (define (rewrite-compiled-path p)
    (let loop ([p/list (reverse (explode-path p))]
               [cp/l compiled-path/list])
      (if (empty? cp/l)
          (apply build-path (append (reverse p/list) alt/list))
          (loop (cdr p/list) (cdr cp/l)))))
    
  (when alt-compile-path
    
    (for ([path (in-list compiled-dirs)])
      (let ([new-path (rewrite-compiled-path path)])
        (delete-directory/files new-path #:must-exist? #f)	
        (when (directory-exists? path)
          (copy-directory/files path new-path
                              #:keep-modify-seconds? #t)))))
    
    
  
  
  (unless (exact-positive-integer? worker-count)
    (raise-argument-error 'parallel-compile-files "exact-positive-integer?" worker-count))
  (unless (and (list? list-of-files) (andmap path-string? list-of-files))
    (raise-argument-error 'parallel-compile-files "(listof path-string?)" list-of-files))
  (unless (and (procedure? handler) (procedure-arity-includes? handler 6))
    (raise-argument-error 'parallel-compile-files "(procedure-arity-includes/c 6)" handler))
  (define result
    (parallel-build (make-object file-list-queue% list-of-files handler
                      (cons-if (path? alt-compile-path)
                               (cons 'include-directory (normalize-path dir))
                               (cons-if (path? alt-compile-path)
                                        (cons 'compiled-file-paths alt-compile-path)
                                        (cons (cons 'collection-paths collection-paths)
                                              options))) worker-count)
                    worker-count
                    #:use-places? use-places?))
  (when (and alt-compile-path copy-back?)
    (for ([path (in-list compiled-dirs)])
        (cond
          [result
           (delete-directory/files path #:must-exist? #f)	
           (copy-directory/files (rewrite-compiled-path path) path
                                 #:keep-modify-seconds? #t)]
          [else
           (delete-directory/files (rewrite-compiled-path path) #:must-exist? #f)])))
      
  result)

(define (start-prefetch-thread send/add)
  (define pf (make-log-receiver (current-logger) 'info 'module-prefetch))
  (thread
   (lambda ()
     (let loop ()
       (let ([v (sync pf)])
         (define l (vector-ref v 2))
         (when (and (list? l)
                    (= 2 (length l))
                    (list? (car l))
                    (path? (cadr l))
                    (andmap module-path? (car l)))
           (define dir (cadr l))
           (define (quote? p) (and (pair? p) (eq? (car p) 'quote)))
           (define (planet? p) (and (pair? p) (eq? (car p) 'planet)))
           (define (submod? p) (and (pair? p) (eq? (car p) 'submod)))
           ;; Add prefetch modules to work queue --- but skip the first one,
           ;; because it's going to be compiled immediately, anyway:
           (for/fold ([prev #f]) ([p (in-list (reverse (car l)))])
             (cond
               [(or (quote? p)
                    (and (submod? p) (quote? (cadr p))))
                ;; skip `quote` module paths
                prev]
               [(or (planet? p)
                    (and (submod? p) (planet? (cadr p))))
                ;; skip `planet` module paths
                prev]
               [else
                (when prev
                  (define path
                    (let loop ([prev prev])
                      (cond
                        [(submod? prev)
                         (define base (cadr prev))
                         (cond
                           [(or (equal? base "..") (equal? base "."))
                            #f]
                           [else
                            (loop (cadr prev))])]
                        [else (resolve-module-path prev (build-path dir "dummy.rkt"))])))
                  (when (path? path)
                    (send/add path)))
                p])))
         (loop))))))

;; This thread is run in the worker's place. For every compiler event in the worker, this sends a
;; message back to the original place, which will be turned into a log event in `pb-logger`.
(define (start-logging-thread send/log)
  (define log-rec (make-log-receiver (current-logger) 'info 'compiler/cm))
  (define sema (make-semaphore))
  (define t
    (thread
     (lambda ()
       (define (handle-msg v)
         (send/log (vector-ref v 0) (vector-ref v 1) (vector-ref v 2)))
       (define (drain)
         (sync/timeout 0 (handle-evt log-rec (λ (v) (handle-msg v) (drain)))))

       (let loop ()
         (sync
          (handle-evt log-rec
                      (λ (v) (handle-msg v) (loop)))
          (handle-evt sema
                      (λ (_) (drain))))))))
  (lambda ()
    (semaphore-post sema)
    (sync t)))