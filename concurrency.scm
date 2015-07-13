

(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))

(define (make-serializer)
  (let ((mutex (make-semaphore 1)))
    (lambda (p)
      (define (serialized-p . args)
        (semaphore-wait mutex)
        (let ((val (apply p args)))
          (semaphore-post mutex)
          val))
      serialized-p)))


(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (+ x 1))))

x