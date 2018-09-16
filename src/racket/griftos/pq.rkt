#lang racket/base

(provide make-pq pq? pq-peek pq-add! pq-remove! pq-tick!)
(require racket/list)
(require data/queue)
(require data/skip-list)
(require data/order)


(struct pq (sl sema))

(define (make-pq)
  (pq
   (make-skip-list real-order
                   #:key-contract exact-nonnegative-integer?
                   #:value-contract queue?)
   (make-semaphore 1)))

(define (pq-peek q)
  (semaphore-wait (pq-sema q))
  (let ([front (skip-list-iterate-first (pq-sl q))])
    (begin0
      (and front (skip-list-iterate-key front))
      (semaphore-post (pq-sema q)))))

(define (pq-add! q key val)
  (semaphore-wait (pq-sema q))
  (let ([result (skip-list-ref (pq-sl q)
                               key
                               (λ () (let ([r (make-queue)])
                                       (skip-list-set! (pq-sl q) key r)
                                       r)))])
    (enqueue! result val))
  (semaphore-post (pq-sema q)))

(define (pq-remove! q)
  (semaphore-wait (pq-sema q))
  (begin0
    (let* ([front (skip-list-iterate-first (pq-sl q))]
           [k (if front (skip-list-iterate-key (pq-sl q) front) #f)]
           [v (if front (skip-list-iterate-value (pq-sl q) front) #f)])
      (if v (begin0
              (dequeue! v)
              (when (queue-empty? v) (skip-list-remove! (pq-sl q) k)))
          #f))
    (semaphore-post (pq-sema q))))
    
(define (pq-empty? q)
  (semaphore-wait (pq-sema q))
  (begin0
    (zero? (skip-list-count (pq-sl q)))
    (semaphore-post (pq-sema q))))

;; (pq-tick! q key proc) removes all queued values from q with key <= parameter key
;;   and calls procedure proc with these values, once each
;; pq-tick! (PQ X) Nat (X -> Void) -> Void

(define (pq-tick! q key proc)
  (semaphore-wait (pq-sema q))
  (define qs (let loop ([i (skip-list-iterate-first (pq-sl q))]
                        [acc empty])
               (if (and i (>= key (skip-list-iterate-key (pq-sl q) i)))
                 (loop (skip-list-iterate-next (pq-sl q) i)
                       (cons (skip-list-iterate-value (pq-sl q) i)
                             acc))
                 acc)))
  (skip-list-remove-range! (pq-sl q) 0 (add1 key))
  (semaphore-post (pq-sema q))
  (for* ([sq (in-list qs)]
        [v (in-queue sq)])
    (proc v)))


(module+ test

  (require racket/random)
  (define myq (make-pq))
  
  (time
   (for ([i (in-range 30000)])
    (pq-add! myq (random 1024) #f)))

  (time
   (for ([i (in-range 30000)])
     (pq-remove! myq))))
