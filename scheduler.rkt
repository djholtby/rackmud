#lang racket/base

(require racket/list "syncq.rkt" "pq.rkt")
(provide make-event-scheduler scheduler-add! scheduler-add!/now scheduler-add!/abs scheduler-start! scheduler-stop! scheduler-yield! scheduler-call-and-return!)
(provide event event-active? set-event-active?! event-period set-event-period!)

(struct event-scheduler (pq pending semaphore [next-event #:mutable] [thread #:mutable])
  #:property prop:evt (struct-field-index pending))

(struct event ([active? #:mutable] [period #:mutable] thunk owner)
  #:property prop:procedure
  (λ (evt)
    (when (event-active? evt)
      ((event-thunk evt))
      (when (event-period evt)
        (scheduler-add! (event-owner evt)
                        (event-period evt)
                        evt)))))

(define (make-event thunk sched #:period [period #f])
  (event #t period thunk sched))

(define (make-event-scheduler)
  (event-scheduler (make-pq) (make-syncq) (make-semaphore 1) #f #f))

;; (scheduler-add! sched delay event) schedules event/proc event into sched, to be run delay ms from now.
;;   returns event (or an event that wraps it, if it was a procedure / continuation)
;;   You can use this event to cancel a pending event.  This doesn't bother dequeuing it yet.
;; scheduler-add!: Event-Scheduler Nat (or (* -> Any) Event) -> Event

(define (scheduler-add! sched delay event)
  (scheduler-add!/abs sched
                      (+ delay (inexact->exact (round (current-inexact-milliseconds))))
                      event))
;; equivalent to (scheduler-add! sched 0 event)

(define (scheduler-add!/now sched event)
  (scheduler-add!/abs sched (inexact->exact (round (current-inexact-milliseconds))) event))

;; (scheduler-add! sched time event) schedules event/proc event into sched, to be run when (current-inexact-milliseconds) = time
;;   returns event (or an event that wraps it, if it was a procedure / continuation)
;; scheduler-add!/abs: Event-Scheduler Nat (or (* -> Any) Event) -> Event

(define (scheduler-add!/abs sched time evt)
  (define wrapped-event (if (event? evt) evt (make-event evt sched)))
  (semaphore-wait (event-scheduler-semaphore sched))
  
  (cond [(<= time (inexact->exact (round (current-inexact-milliseconds))))
         (sync-enqueue! (event-scheduler-pending sched) wrapped-event)]
        [(>= time (or (event-scheduler-next-event sched) +inf.0))
         (pq-add! (event-scheduler-pq sched) wrapped-event)]
        [else
         (pq-add! (event-scheduler-pq sched) time wrapped-event)
         (set-event-scheduler-next-event! sched time)
         (thread-send (event-scheduler-thread sched) #f)])
  (semaphore-post (event-scheduler-semaphore sched)))

;; (make-scheduler-thunk sched) returns a recursive thunk that moves events from the priority-queue to the sync-channel of pending events
;;   This thunk is informed of new events by writing #f to its thread's mailbox (but only if the new event has a trigger time < the
;;   current head of the PQ, otherwise there's no sense waking it).
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
  (set-event-scheduler-thread! sched (thread thunk)))

(define (scheduler-stop! sched)
  (when (event-scheduler-thread sched)
    (kill-thread (event-scheduler-thread sched))
    (set-event-scheduler-thread! sched #f)))

;; (scheduler-yield! sched time) pushes the current continuation to sched, and then breaks the current thread.  Useful if a calc is taking a
;;   while and you want some snappiness to your worker threads.

(define (scheduler-yield! sched [time 0])
  (let/cc k (scheduler-add! sched time k) (break-thread (current-thread))))


(define (scheduler-call-and-return! sched thunk [time 0])
  (let/cc k (scheduler-add! sched time (event #t #f
                                              (lambda ()
                                                (k (thunk)))))
    (break-thread (current-thread))))
