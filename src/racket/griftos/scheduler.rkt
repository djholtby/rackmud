#lang racket/base

(require racket/list)
(require "syncq.rkt" "pq.rkt")
(provide event-scheduler event-scheduler-time make-event-scheduler scheduler-add! scheduler-tick! scheduler-yield! scheduler-call-and-return!)

(struct event-scheduler (pq pending [time #:mutable])
  #:property prop:evt (struct-field-index pending))

(define (make-event-scheduler time)
  (event-scheduler (make-pq)
                   (make-syncq)
                   time))

;; (scheduler-add! sched time event) adds event to sched, and will dispatch it at the given tick
;;   (if the current tick is already >= time, it will be immediately dispatched to the pending queue

(define (scheduler-add! sched time event)
  (if (<= time (event-scheduler-time sched))
      (sync-enqueue! (event-scheduler-pending sched) event)
      (pq-add! (event-scheduler-pq sched) time event)))

;; (scheduler-tick! sched) increments sched's tick counter, and dispatches any pending events with time <= the new tick

(define (scheduler-tick! sched)
  (set-event-scheduler-time! sched (add1 (event-scheduler-time sched)))
  (pq-tick! (event-scheduler-pq sched)
            (event-scheduler-time sched)
            (λ (event)
              (sync-enqueue! (event-scheduler-pending sched) event))))

;; (scheduler-yield sched time) breaks the current worker thread and enqueues an event for the given time
;;   to finish the worker thread's current thunk

(define (scheduler-yield! sched [time 0])
  (let/cc k (scheduler-add! sched time k) (break-thread (current-thread))))


(define (scheduler-call-and-return! sched thunk [time 0])
  (let/cc k (scheduler-add! sched time (lambda ()
                                        (k (thunk))))
    (break-thread (current-thread))))

(module+ test
  (define my-sched (make-event-scheduler 1))

  (define (worker-thunk)
    (with-handlers ([exn:break? void])
      (let ([msg (sync my-sched)])
        (when msg (writeln (msg)))))
    (worker-thunk))
    
  (define worker-thread (thread worker-thunk))
  
  (define (foo lst)
    (if (empty? lst)
        (begin0 0 (scheduler-yield! my-sched) (printf "LOL\n") )
        (+ (first lst) (foo (rest lst)))))
  
  (scheduler-add! my-sched 0 (λ () (foo '(1 2 3 4 5 6 7 8 9 10))))

  (define (f2 lst)
    (define ans (scheduler-call-and-return! my-sched (λ () (foo lst))))
    (printf "ans=~v\n" ans)
    ans)

  (scheduler-add! my-sched 0 (λ () (f2 '(1 2 3 4 5 6 7 8 9 10))))
  
    
  )


