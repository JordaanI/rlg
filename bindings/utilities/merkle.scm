;;;
;;;; merkle trees of events
;;;; inspired by James Long 
;;;
;;; https://github.com/jlongster/crdt-example-app/tree/master
;;;



;;;
;;;; base3
;;;


(define (base3->integer s n)
  (let ((padded (pad-string-with s #\0 n)))
    (apply 
      +
      (map 
        (lambda (i)
          (* (expt 3 (- n i 1)) (string->number (char->string (string-ref padded i)))))
        (iota n)))))


(define (integer->base3 x n)
  (define (integer->base3-rec x n acc)
    (if (= n 0)
        (string-append acc (number->string (modulo x 3)))
      (let ((nth-digit (floor (/ x (expt 3 n)))))
        (integer->base3-rec (- x (* nth-digit (expt 3 n))) (- n 1) (string-append acc (number->string nth-digit))))))
  (integer->base3-rec x (- n 1) ""))

#|

> (base3->integer "0000000000121102" 16)
443

> (integer->base3 443 16)
"0000000000121102"

|# 


;;;
;;;; merkle-get-keys
;;;


(define (merkle-get-keys trie)
  (collect-if (lambda (k) (not (equal? k "hash"))) (map car (table->list trie))))


;;;
;;;; merkle-key2minutes
;;;


(define (merkle-key2minutes key)
  (base3->integer key 16))


;;;
;;;; merkle-insert
;;;


(define (merkle-insert trie timestamp)
  (let ((hash (hlc-hash timestamp))
        (key (integer->base3 (floor (/ (hlc-get-millis timestamp) 1000 60)) 16))
        (oldhash (table-ref trie "hash" 0)))
    
    ;(pp (list " merkle-insert" key))
    (table-set! trie "hash" (bitwise-xor hash oldhash))
    (merkle-insertkey trie key hash)))


(define (rolling-hash timestamps)
  (let ((oldhash 0))
    (for-each (lambda (ts)
                (set! oldhash (bitwise-xor (hlc-hash (hlc-parse ts)) oldhash)))
              timestamps)
    oldhash))


;;;
;;;; merkle-insertkey
;;;


(define (merkle-insertkey trie key hash)
  (if (> (string-length key) 0)
      (let* ((c (substring key 0 1))
             (node (table-ref trie c (make-table))))
        (table-set! node "hash" (bitwise-xor hash (table-ref node "hash" 0)))
        (table-set! trie c node)
        (merkle-insertkey node (substring key 1 (string-length key)) hash))))


;;;
;;;; merkle-build
;;;


(define (merkle-build timestamps)
  (let ((trie (make-table)))
    (for-each (lambda (ts)
                (merkle-insert trie ts))
              timestamps)
    trie))


;;;
;;;; merkle-diff
;;;


(define (merkle-diff trie1 trie2)
  (define (merkle-diff-rec trie1 trie2 key)
    (let* ((keys (sort (make-set2 (append (merkle-get-keys trie1) (merkle-get-keys trie2))) string<?))
           (diffkey (find-if (lambda (k)
                               (not
                                 (= 
                                   (table-ref (table-ref trie1 k (make-table)) "hash" 0)
                                   (table-ref (table-ref trie2 k (make-table)) "hash" 0))))
                             keys)))
      (if (not diffkey)
          (merkle-key2minutes key)
        (merkle-diff-rec (table-ref trie1 diffkey (make-table)) (table-ref trie2 diffkey (make-table)) (string-append key diffkey)))))
  (if (= (table-ref trie1 "hash" 0) (table-ref trie2 "hash" 0))
      #f
    (merkle-diff-rec trie1 trie2 "")))



#|

(define mytrie1 (make-table))
(define mytrie2 (make-table))

(merkle-insert mytrie1  (hlc-make-timestamp 1738250449085 0 "node1"))
(merkle-insert mytrie1  (hlc-make-timestamp 1738250523158 0 "node1"))

(merkle-insert mytrie2  (hlc-make-timestamp 1738250449085 0 "node1"))
(merkle-insert mytrie2  (hlc-make-timestamp 1738250523158 0 "node1"))



(merkle-insert mytrie2  (hlc-make-timestamp 1738250449085 0 "node1"))
(merkle-insert mytrie2  (hlc-make-timestamp 1738250523158 0 "node1"))
(merkle-insert mytrie2  (hlc-make-timestamp 1738250584163 0 "node2"))

(merkle-diff mytrie1 mytrie2)


|#


         
        