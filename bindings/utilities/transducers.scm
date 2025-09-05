
;;; 
;;; SRFI-171: Transducers 
;;;
;;; Gambit version from the SRFI sample implementation by Linus Björnstam
;;;
;;; Original code: https://github.com/scheme-requests-for-implementation/srfi-171
;;;
;;; Video with Rich Hickey: https://www.youtube.com/watch?v=6mTbuzafcII&t=16s
;;; 
;;; Original copyright below:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright 2019 Linus Björnstam
;;
;; You may use this code under either the license in the SRFI document or the
;; license below.
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all source copies.
;; The software is provided "as is", without any express or implied warranties.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; A reduced value is stops the transduction.
(define-record-type <reduced>
  (reduced val)
  reduced?
  (val unreduce))


;; helper function which ensures x is reduced.
(define (ensure-reduced x)
  (if (reduced? x)
      x
      (reduced x)))


;; helper function that wraps a reduced value twice since reducing functions (like list-reduce)
;; unwraps them. tconcatenate is a good example: it re-uses it's reducer on it's input using list-reduce.
;; If that reduction finishes early and returns a reduced value, list-reduce would "unreduce"
;; that value and try to continue the transducing process.
(define (preserving-reduced reducer)
  (lambda (a b)
    (let ((return (reducer a b)))
      (if (reduced? return)
          (reduced return)
          return))))


;; This is where the magic tofu is cooked
(define (list-reduce f identity lst)
  (if (null? lst)
      identity
      (let ((v (f identity (car lst))))
        (if (reduced? v)
            (unreduce v)
            (list-reduce f v (cdr lst))))))

(define (vector-reduce f identity vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0) (acc identity))
      (if (= i len)
          acc
          (let ((acc (f acc (vector-ref vec i))))
            (if (reduced? acc)
                (unreduce acc)
                (loop (+ i 1) acc)))))))

(define (string-reduce f identity str)
  (let ((len (string-length str)))
    (let loop ((i 0) (acc identity))
      (if (= i len)
          acc
          (let ((acc (f acc (string-ref str i))))
            (if (reduced? acc)
                (unreduce acc)
                (loop (+ i 1) acc)))))))

(define (bytevector-u8-reduce f identity vec)
  (let ((len (bytevector-length vec)))
    (let loop ((i 0) (acc identity))
      (if (= i len)
          acc
          (let ((acc (f acc (bytevector-u8-ref vec i))))
            (if (reduced? acc)
                (unreduce acc)
                (loop (+ i 1) acc)))))))


(define (safe-reader reader)
  (lambda (port)
    (with-exception-catcher 
      (lambda (e)
        (eof-object))
      (lambda ()
        (reader port)))))
    

(define (port-reduce f identity reader port)
  (let loop ((val ((safe-reader reader) port))
             (acc identity))
       (if (eof-object? val)
           acc
         (let ((acc (f acc val)))
           (if (reduced? acc)
               (unreduce acc)
             (loop ((safe-reader reader) port) acc))))))


(define (channel-reduce f identity channel)
  (generator-reduce f identity (channel-generator channel)))
                  

(define (generator-reduce f identity gen)
  (let loop ((val (gen)) (acc identity))
    (if (eof-object? val)
        acc
        (let ((acc (f acc val)))
          (if (reduced? acc)
              (unreduce acc)
              (loop (gen) acc))))))


;;;
;;;; transducers
;;;;


;; A special value to be used as a placeholder where no value has been set and #f
;; doesn't cut it. Not exported, and not really needed.

(define-record-type <nothing>
  (make-nothing)
  nothing?)
(define nothing (make-nothing))



;;;
;;;; reducers (% prefix)
;;; 

;; Reducing functions meant to be used at the end at the transducing
;; process.    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Reducer that does not produce an output
(define %ignore
  (case-lambda
    (() '())
    ((lst) lst)
    ((lst x) lst)))

;; a transducer-friendly cons with the empty list as identity
(define %cons
  (case-lambda
    (() '())
    ((lst) (reverse! lst))
    ((lst x) (cons x lst))))


(define %log
  (case-lambda
    (() '())
    ((lst) '())
    ((lst x) 
     (write x)
     (newline)
     (list))))


(define (%processor thunk)
  (case-lambda
    (() '())
    ((lst) '())
    ((lst x) 
     (thunk x)
     (list))))


(define %reverse-cons
  (case-lambda
    (() '())
    ((lst) lst)
    ((lst x) (cons x lst))))


;; Use this as the f in transduce to count the amount of elements passed through.
;; (transduce (tfilter odd?) tcount (list 1 2 3)) => 2
(define %count
  (case-lambda
    (() 0)
    ((result) result)
    ((result input)
     (+ 1 result))))


;; These two take a predicate and returns reducing functions that behave
;; like any and every from srfi-1
(define (%any pred)
  (case-lambda
    (() #f)
    ((result) result)
    ((result input)
     (let ((test (pred input)))
       (if test
           (reduced test)
           #f)))))


(define (%every pred)
  (case-lambda
    (() #t)
    ((result) result)
    ((result input)
     (let ((test (pred input)))
       (if (and result test)
           test
           (reduced #f))))))


(define (%pipeline-reducer c2)
  (case-lambda
    (() #t)
    ((result) result)
    ((result input)
     (>> c2 input))))


;;;
;;;; transducing contexts
;;;

(define (transduce-one-value xform val)
  (first (list-transduce xform %cons (list val))))


(define list-transduce
  (case-lambda
    ((xform f coll)
     (list-transduce xform f (f) coll))
    ((xform f init coll)
     (let* ((xf (xform f))
            (result (list-reduce xf init coll)))
       (xf result)))))

(define vector-transduce
  (case-lambda
    ((xform f coll)
     (vector-transduce xform f (f) coll))
    ((xform f init coll)
     (let* ((xf (xform f))
            (result (vector-reduce xf init coll)))
       (xf result)))))

(define string-transduce
  (case-lambda
    ((xform f coll)
     (string-transduce xform f (f) coll))
    ((xform f init coll)
     (let* ((xf (xform f))
            (result (string-reduce xf init coll)))
       (xf result)))))

(define bytevector-u8-transduce
  (case-lambda
    ((xform f coll)
     (bytevector-u8-transduce xform f (f) coll))
    ((xform f init coll)
     (let* ((xf (xform f))
            (result (bytevector-u8-reduce xf init coll)))
       (xf result)))))

(define port-transduce
  (case-lambda
    ((xform f by)
     (port-transduce xform f by))
    ((xform f by port)
     (port-transduce xform f (f) by port))
    ((xform f init by port)
     (let* ((xf (xform f))
            (result (port-reduce xf init by port)))
       (xf result)))))


(define channel-transduce
  (case-lambda
    ((xform f channel)
     (channel-transduce xform f (f) channel))
    ((xform f init channel)
     (let* ((xf (xform f))
            (result (channel-reduce xf init channel)))
       (xf result)))))

#; (define (channel-transduce xform f channel)
     (generator-transduce xform f (channel-generator channel))) 
; much simpler but no variable arity support!

(define generator-transduce
  (case-lambda
    ((xform f gen)
     (generator-transduce xform f (f) gen))
    ((xform f init gen)
     (let* ((xf (xform f))
            (result (generator-reduce xf init gen)))
       (xf result)))))


;;;
;;;; transducers
;;;


(define $identity
  (lambda (reducer)
    reducer))


(define ($compose . functions)
  (define (make-chain thunk chain)
    (lambda args
      (call-with-values (lambda () (apply thunk args)) chain)))
  (if (null? functions)
      values
      (improper-fold make-chain (car functions) (cdr functions))))



(define ($map f)
  (lambda (reducer)
    (case-lambda
      (() (reducer))
      ((result) (reducer result)) 
      ((result input)
       (reducer result (f input))))))


(define ($filter pred)
  (lambda (reducer)
    (case-lambda
      (() (reducer))
      ((result) (reducer result))
      ((result input)
       (if (pred input)
           (reducer result input)
           result)))))


(define ($debounce t)
  (lambda (reducer)
    (let* ((last-acc #f)
           (last-event #f)
           (sender (thread 
                     (lambda ()
                       (let loop ()
                            (thread-sleep! (/ t 1000))
                            (if last-event
                                (begin
                                  (reducer last-acc last-event)
                                  (set! last-acc #f)
                                  (set! last-event #f)))
                            (loop))))))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (set! last-event input)
         (set! last-acc result))))))


(define ($remove pred)
  (lambda (reducer)
    (case-lambda
      (() (reducer))
      ((result) (reducer result))
      ((result input)
       (if (not (pred input))
           (reducer result input)
           result)))))


(define ($filter-map f) 
  ($compose ($map f) ($filter values)))


(define (make-replacer map)
  (cond
   ((list? map)
    (lambda (x)
      (let ((replacer? (assoc x map)))
        (if replacer?
            (cdr replacer?)
            x))))
   ((hash-table? map)
    (lambda (x)
      (hash-table-ref/default map x x)))
   ((procedure? map) map)
   (else
    (error "Unsupported mapping in treplace" map))))


(define ($replace map)
  ($map (make-replacer map)))


(define ($drop n)
  (lambda (reducer)
    (let ((new-n (+ 1 n)))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (set! new-n (- new-n 1))
         (if (positive? new-n)
             result
             (reducer result input)))))))


(define ($drop-while pred)
  (lambda (reducer)
    (let ((drop? #t))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (if (and (pred input) drop?)
             result
             (begin
               (set! drop? #f)
               (reducer result input))))))))


(define ($take n)
  (lambda (reducer)
    ;; we need to reset new-n for every new transduction
    (let ((new-n n))
      (case-lambda
        (() (reducer))
        ((result) (reducer result))
        ((result input)
         (let ((result (if (positive? new-n)
                           (reducer result input)
                           result)))
           (set! new-n (- new-n 1))
           (if (not (positive? new-n))
               (ensure-reduced result)
               result)))))))



(define $take-while
  (case-lambda
    ((pred) ($take-while pred (lambda (result input) result)))
    ((pred retf)
     (lambda (reducer)
       (let ((take? #t))
         (case-lambda
           (() (reducer))
           ((result) (reducer result))
           ((result input)
            (if (and take? (pred input))
                (reducer result input)
                (begin
                  (set! take? #f)
                  (ensure-reduced (retf result input)))))))))))



(define ($concatenate reducer)
  (let ((preserving-reducer (preserving-reduced reducer)))
    (case-lambda
      (() (reducer))
      ((result) (reducer result))
      ((result input)
       (list-reduce preserving-reducer result input)))))


(define ($append-map f)
  ($compose ($map f) $concatenate))



;; Flattens everything and passes each value through the reducer
;; (list-transduce tflatten rcons (list 1 2 (list 3 4 '(5 6) 7 8))) => (1 2 3 4 5 6 7 8)
(define $flatten
  (lambda (reducer)
    (case-lambda
      (() '())
      ((result) (reducer result))
      ((result input)
       (if (list? input)
           (list-reduce (preserving-reduced ($flatten reducer)) result input)
           (reducer result input))))))



;; removes duplicate consecutive elements
(define $delete-neighbor-duplicates
  (case-lambda
    (() ($delete-neighbor-duplicates equal?))
    ((equality-pred?) 
     (lambda (reducer)
       (let ((prev nothing))
         (case-lambda
           (() (reducer))
           ((result) (reducer result))
           ((result input)
            (if (equality-pred? prev input)
                result
                (begin
                  (set! prev input)
                  (reducer result input))))))))))


;; Deletes all duplicates that passes through.
(define $dedup
  (case-lambda
    (() ($dedup equal?))
    ((equality-pred?)
     (lambda (reducer)
       (let ((already-seen (make-table test: equality-pred?)))
         (case-lambda
           (() (reducer))
           ((result) (reducer result))
           ((result input)
            (if (table-ref already-seen input #f)
                result
                (begin
                  (table-set! already-seen input #t)
                  (reducer result input))))))))))


;; Partitions the input into lists of N items. If the input stops it flushes whatever
;; it has collected, which may be shorter than n.
(define ($segment n)
  (if (not (and (integer? n) (positive? n)))
      (error "argument to tsegment must be a positive integer")
      (lambda (reducer)
        (let ((i 0)
              (collect (make-vector n)))
          (case-lambda
            (() (reducer))
            ((result)
             ;; if there is anything collected when we are asked to quit
             ;; we flush it to the remaining transducers
             (let ((result
                    (if (zero? i)
                        result
                        (reducer result (vector->list collect 0 i)))))
               (set! i 0)
               ;; now finally, pass it downstreams
               (if (reduced? result)
                   (reducer (unreduce result))
                   (reducer result))))
            ((result input)
             (vector-set! collect i input)
             (set! i (+ i 1))
             ;; If we have collected enough input we can pass it on downstream
             (if (< i n)
                 result
                 (let ((next-input (vector->list collect 0 i)))
                   (set! i 0)
                   (reducer result next-input)))))))))


(define ($partition f)
  (lambda (reducer)
    (let* ((prev nothing)
           (collect '()))
      (case-lambda
        (() (reducer))
        ((result)
         (let ((result
                (if (null? collect)
                    result
                    (reducer result (reverse! collect)))))
           (set! collect '())
           (if (reduced? result)
               (reducer (unreduce result))
               (reducer result))))
        ((result input)
         (let ((fout (f input)))
           (cond
            ((or (equal? fout prev) (nothing? prev)) ; collect
             (set! prev fout)
             (set! collect (cons input collect))
             result)
            (else ; flush what we collected already to the reducer
             (let ((next-input  (reverse! collect)))
               (set! prev fout)
               (set! collect (list input))
               (reducer result next-input))))))))))


;; Interposes element between each value pushed through the transduction.
(define ($add-between elem)
  (lambda (reducer)
    (let ((send-elem? #f))
      (case-lambda
        (() (reducer))
        ((result)
         (reducer result))
        ((result input)
         (if send-elem?
             (let ((result (reducer result elem)))
               (if (reduced? result)
                   result
                   (reducer result input)))
             (begin
               (set! send-elem? #t)
               (reducer result input))))))))


;; indexes every value passed through in a cons pair as in (index . value). By default starts at 0
(define $enumerate
  (case-lambda
    (() ($enumerate 0))
    ((n)
     (lambda (reducer)
       (let ((n n))
         (case-lambda
           (() (reducer))
           ((result) (reducer result))
           ((result input)
            (let ((input (cons n input)))
              (set! n (+ n 1))
              (reducer result input)))))))))


(define $log
  (case-lambda
    (() ($log (lambda (result input) (write input) (newline))))
    ((log-function)
     (lambda (reducer)
       (case-lambda
         (() (reducer))
         ((result) (reducer result))
         ((result input)
          (log-function result input)
          (reducer result input)))))))


(define ($toFST fst)
  ($map (lambda (e)
          (^ fst e)
          e)))