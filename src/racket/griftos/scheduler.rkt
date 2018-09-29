#lang racket/base

(require racket/list "syncq.rkt" "pq.rkt")
(provide make-event-scheduler scheduler-add! scheduler-add!/now scheduler-add!/abs scheduler-start! scheduler-stop! scheduler-yield! scheduler-call-and-return!)


(struct event-scheduler (pq pending semaphore [next-event #:mutable] [thread #:mutable])
  #:property prop:evt (struct-field-index pending))

(define (make-event-scheduler)
  (event-scheduler (make-pq) (make-syncq) (make-semaphore 1) #f #f))

(define (scheduler-add! sched delay event)
  (scheduler-add!/abs sched
                      (+ delay (inexact->exact (round (current-inexact-milliseconds))))
                      event))

(define (scheduler-add!/now sched event)
  (scheduler-add!/abs sched (inexact->exact (round (current-inexact-milliseconds))) event))

(define (scheduler-add!/abs sched time event)
  (semaphore-wait (event-scheduler-semaphore sched))
  (cond [(<= time (inexact->exact (round (current-inexact-milliseconds))))
         (sync-enqueue! (event-scheduler-pending sched) event)]
        [(>= time (or (event-scheduler-next-event sched) +inf.0))
         (pq-add! (event-scheduler-pq sched) event)]
        [else
         (pq-add! (event-scheduler-pq sched) time event)
         (set-event-scheduler-next-event! sched time)
         (thread-send (event-scheduler-thread sched) #f)])
  (semaphore-post (event-scheduler-semaphore sched)))

(define (make-scheduler-thunk sched)
  (letrec ([loop (λ ()
                   (semaphore-wait (event-scheduler-semaphore sched))
                   (pq-tick! (event-scheduler-pq sched)
                             (inexact->exact (round (current-inexact-milliseconds)))
                             (λ (event)
                               (sync-enqueue! (event-scheduler-pending sched) event)))
                   (set-event-scheduler-next-event! sched (pq-peek (event-scheduler-pq sched)))
                   (semaphore-post (event-scheduler-semaphore sched))
                   (let ([next-time (and (event-scheduler-next-event sched)
                                         (max 0 (/ (- (event-scheduler-next-event sched) (current-inexact-milliseconds)) 1000)))])
                     (cond [(not next-time) (thread-receive)] ; no events are queued, thread sleeps until an enqueue sends a wakeup signal
                           [(zero? next-time) (sleep 0)]      ; some jackass enqueued an event while we were dequeuing, no need to wait
                           [else (when (sync/timeout (max 0 (/ (- (event-scheduler-next-event sched) (current-inexact-milliseconds)) 1000)) (thread-receive-evt))
                                   (thread-receive))]))       ; sleep until time == next-event time (or until a wakeup signal, thread-receive clears the signal)
                   (loop))])
    loop))

(define (scheduler-start! sched)
  (define thunk (make-scheduler-thunk sched))
  (set-event-scheduler-thread! sched (thread thunk))
  thunk)

(define (scheduler-stop! sched)
  (when (event-scheduler-thread sched)
    (kill-thread (event-scheduler-thread sched))
    (set-event-scheduler-thread! sched #f)))

(define (scheduler-yield! sched [time 0])
  (let/cc k (scheduler-add! sched time k) (break-thread (current-thread))))


(define (scheduler-call-and-return! sched thunk [time 0])
  (let/cc k (scheduler-add! sched time (lambda ()
                                         (k (thunk))))
    (break-thread (current-thread))))

#|
(define my-sched (make-event-scheduler))
  (define (worker-thunk)
    (with-handlers ([exn:break? void])
      (let ([msg (sync my-sched)])
        (when msg (msg))))
    (worker-thunk))

(scheduler-start! my-sched)
(define worker-thread (thread worker-thunk))
|#