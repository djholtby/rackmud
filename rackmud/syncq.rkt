#lang racket/base

(require data/queue)

(provide syncq? make-syncq sync-dequeue! sync-enqueue! sync-empty?)

(struct syncq (q mod-sema empty-sema)
  #:property prop:evt (λ (q) (wrap-evt (semaphore-peek-evt (syncq-empty-sema q))
                                       (λ (e) (sync-dequeue! q)))))

(define (make-syncq)
  (syncq (make-queue) (make-semaphore 1) (make-semaphore 0)))

(define (sync-enqueue! q v)
  (semaphore-wait (syncq-mod-sema q))
  (begin0
    (enqueue! (syncq-q q) v)
    (semaphore-post (syncq-empty-sema q))
    (semaphore-post (syncq-mod-sema q))))

(define (sync-dequeue! q)
  (semaphore-wait (syncq-mod-sema q))
  (define q/real (syncq-q q))
  (begin0
    (if (not (queue-empty? q/real))
        (begin0
          (dequeue! q/real)
          (semaphore-wait (syncq-empty-sema q)))
        #f)
    (semaphore-post (syncq-mod-sema q))))
        
(define (sync-empty? q)
  (semaphore-wait (syncq-mod-sema q))
  (begin0
    (queue-empty? (syncq-q q))
    (semaphore-post (syncq-mod-sema q))))

    
(module+ test
  (define counter 0)
  (define counter-sema (make-semaphore 1))
  (define (count)
    (semaphore-wait counter-sema)
    (set! counter (add1 counter))
    (semaphore-post counter-sema))

  (define waste-counter 0)
  (define waste-counter-sema (make-semaphore 1))
  (define (count-waste)
    (semaphore-wait waste-counter-sema)
    (set! waste-counter (add1 waste-counter))
    (semaphore-post waste-counter-sema))
  

  (define my-q (make-syncq))

  
  (time
   (for ([i (in-range 30000)])
    (sync-enqueue! my-q (random 1024))))

  (define worker-threads
    (build-list 64 (λ (i) (thread (letrec ([loop (λ()
                                                   (define msg (sync my-q))
                                                   (if msg (count) (count-waste))
                                                   (loop))])
                                    loop)))))

  (time
   (let loop ()
     (unless (= 30000 counter)
       (sleep 1/1000)
       (loop))))
  waste-counter
)

  