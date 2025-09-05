;;;
;;; FIFO queues utility
;;;
;;;
;;;
;;;
;;;


;;;
;;;; fifo-queue structure
;;;


(define-structure fifo-queue data write-mutex read-mutex size)
(define-structure sticky-queue data write-mutex read-mutex size sticky)

;;;
;;;; new-fifo-queue
;;;


(define (new-fifo-queue #!key (size 0))
  (let ((q (make-fifo-queue (list) (make-mutex) (make-mutex) size)))
    (mutex-lock! (fifo-queue-read-mutex q) #f #f)
    q))

(define (new-sticky-queue #!key (default 0) (size 0))
  (make-sticky-queue (list) (make-mutex) (make-mutex) size default))


;;;
;;;; fifo-queue-full?
;;;


(define (fifo-queue-full? q)
  (let ((size (fifo-queue-size q)))
    (and (not (= size 0))
         (>= (length (fifo-queue-data q)) size))))

(define (sticky-queue-full? q)
  (let ((size (sticky-queue-size q)))
    (and (not (zero? size))
         (>= (length (sticky-queue-data q)) size))))


;;;
;;;; fifo-queue-empty?
;;;


(define (fifo-queue-empty? q)
  (= (length (fifo-queue-data q)) 0))

(define (sticky-queue-empty? q)
  (= (length (sticky-queue-data q)) 0))

;;;
;;;; fifo-queue-push
;;;


(define (fifo-queue-push q data)
  (mutex-lock! (fifo-queue-write-mutex q) #f #f)
  (fifo-queue-data-set! q (cons data (fifo-queue-data q)))
  (if (not (fifo-queue-full? q))
      (mutex-unlock! (fifo-queue-write-mutex q)))
  (mutex-unlock! (fifo-queue-read-mutex q)))

(define (sticky-queue-push q data)
  (mutex-lock! (sticky-queue-write-mutex q) #f #f)
  (sticky-queue-data-set! q (cons data (sticky-queue-data q)))
  (if (not (sticky-queue-full? q))
      (mutex-unlock! (sticky-queue-write-mutex q)))
  (mutex-unlock! (sticky-queue-read-mutex q)))


;;;
;;;; fifo-queue-pull
;;;


(define (fifo-queue-pull q)
  (mutex-lock! (fifo-queue-read-mutex q) #f #f)
  (if (fifo-queue-empty? q)
      (begin
        (log-message "trying to pull from empty queue!")
        (fifo-queue-pull q))
      (let ((l (last (fifo-queue-data q)))
            (r (butlast (fifo-queue-data q))))
        (fifo-queue-data-set! q r)
        (if (not (fifo-queue-empty? q))
            (mutex-unlock! (fifo-queue-read-mutex q)))
        (mutex-unlock! (fifo-queue-write-mutex q))
        l)))


(define (sticky-queue-pull q)
  (mutex-lock! (sticky-queue-read-mutex q) #f #f)
  (if (sticky-queue-empty? q)
      (let ((sticky (sticky-queue-sticky q)))
        (mutex-unlock! (sticky-queue-read-mutex q))
        sticky)
      (let ((l (last (sticky-queue-data q)))
            (r (butlast (sticky-queue-data q))))
        (sticky-queue-data-set! q r)
        (sticky-queue-sticky-set! q l)
        (mutex-unlock! (sticky-queue-read-mutex q))
        (mutex-unlock! (sticky-queue-write-mutex q))
        l)))

;;;
;;;; tests
;;;

#|

(define myqueue (new-fifo-queue size: 2))

; a thread that reads the queue and blocks when there is nothing... ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

(thread
(lambda ()
(let ((i 0))
(let loop ()
(let ((e (fifo-queue-push myqueue i)))
(set! i (+ i 1))
(pp (list "pushed" (- i 1)))
(force-output)
(thread-sleep! 1)
(loop))))))


(thread
(lambda ()
(let loop ()
(let ((e (fifo-queue-pull myqueue)))
(pp (list "received:" e))
(force-output)
(thread-sleep! 4)
(loop)))))


(thread
(lambda ()
(let loop ()
(let ((e (fifo-queue-pull myqueue)))
(pp (list "received 2:" e))
(force-output)
(thread-sleep! 2)
(loop)))))

(fifo-queue-push myqueue 3)
(fifo-queue-push myqueue 4)


|#
