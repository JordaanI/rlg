
(define (last l)
  (car (reverse l)))


(define (butlast l)
  (reverse (cdr (reverse l))))


(define (remove-element-at l i)
  (cond 
    ((null? l)
     (error "Invalid index to remove" l i))
    ((= i 0)
     (cdr l))
    ((> i 0)
     (cons (car l) (remove-element-at (cdr l) (- i 1))))
    ((< i 0)
     (error "Invalid index to remove" l i))))


(define (copy-list l)
  (if (null? l)
      l
      (cons (if (pair? (car l)) (copy-list (car l)) (car l))
            (copy-list (cdr l)))))

(define (list-head l i)
  (if (zero? i) (list)
    (cons (car l) (list-head (cdr l) (- i 1)))))

;;;
;;;; iota
;;;


(define (iota n)
  (if (= n 0)
      (list)
    (append (iota (- n 1)) (list (- n 1)))))


;;;
;;;; reduce
;;;


(define (reduce fn l init)
  (if (null? l) init
      (fn (car l)
          (reduce fn (cdr l) init))))


;;;
;;;; pick-random-element
;;;


(define (pick-random-element l)
  (list-ref 
    l
    (random-integer (length l))))


;;;
;;;; pick-random-subset
;;;


(define (pick-random-subset l k)
  (if (or (= (length l) 0) (= k 0))
      (list)
    (let ((random-index (random-integer (length l))))
      (cons (list-ref l random-index) 
            (pick-random-subset (remove-index l random-index) (- k 1))))))

;;;
;;;; Functional list processing
;;;

;;;
;;;; sets
;;;


(define (union lst . lsts)
  (let ((res (list)))
    (for-each (lambda (x)
                (if (not (member x res))
                    (set! res (cons x res))))
              lst)
    (for-each (lambda (l)
                (for-each (lambda (x)
                            (if (not (member x res))
                                (set! res (cons x res))))
                          l))
              lsts)
    ;(append! res (difference list2 res))
    res))


(define (intersection list1 list2)
  (collect-if (lambda (x)
                (member x list2))
              (union list1)))


(define (intersection-m list1 . lists)
  (if (null? lists)
      list1
    (apply intersection-m (cons (intersection list1 (car lists)) (cdr lists)))))


(define (difference list1 list2)
  (collect-if (lambda (x)
                (not (member x list2)))
              (union list1)))


(define (difference2 list1 list2 #!key (key identity))
  (collect-if (lambda (x)
                (not (member2 x list2 key: key)))
              (make-set2 list1 key: key)))
  
  
(define (member2 x l  #!key (key identity))
  (find-tail (lambda (y)
               (equal? (key x) (key y)))
             l))

(define (find-tail pred list)
  (let lp ((list list))
    (and (not (null-list? list))
	 (if (pred (car list)) list
	     (lp (cdr list))))))


;;;
;;;; (non special-form and and or)
;;;


(define (andf . rest)
  (or (null? rest)
      (and (car rest) (apply andf (cdr rest)))))


(define (orf . rest)
  (if (null? rest) 
      #f
    (or (car rest) (apply orf (cdr rest)))))


;;;
;;;; mapcount
;;;


(define (mapcount f l)
  (mapcount-aux f l (length l)))


(define (mapcount-aux f l n)
  (if (null? l)
      l
    (cons (f (car l) (- n (length l))) (mapcount-aux f (cdr l) n))))


;;;
;;;; ormap
;;;


(define (ormap f l . rest)
  (if (null? l)
      #f
    (let ((cres (apply f (cons (car l) rest))))
      (or cres
          (apply ormap (append (list f (cdr l)) rest))))))


;;;
;;;; andmap
;;;


(define (andmap f l . rest)
  (if (null? l)
      #t
    (and
      (apply f (cons (car l) rest))
      (apply andmap (append (list f (cdr l)) rest)))))


;;;
;;;; andmap_v2
;;;


(define (andmap_v2 f l)
  (or (null? l)
      (and (f (car l)) 
	   (andmap_v2 f (cdr l)))))

;;;
;;;; special-map
;;;


(define (special-map f l . rest)
  (apply map
         (append
           (list f l)
           (map (lambda (add-arg)
                  (or (and (list? add-arg) add-arg)
                      (make-list (length l) add-arg)))
                rest))))


;;;
;;;; multi-map
;;;


(define (multi-map f . rest)
  (if (null? (car rest))
      (list)
    (cons (apply f (map car rest)) (apply multi-map (cons f (map cdr rest))))))



;;;
;;;; improper-map
;;;

;;; a map accepting improper lists (i.e. including non-pairs as l)

(define (improper-map fn l #!optional (tail '()))
  (let rec ((l l))
    (cond ((null? l)
           tail)
          ((pair? l)
           (cons (fn (car l))
                 (rec (cdr l))))
          (else
            (fn l)))))



(define (improper-fold fn tail l)
  (let lp ((tail tail)
           (l l))
    (cond ((null? l)
           tail)
          ((pair? l)
           (lp (fn (car l) tail)
               (cdr l)))
          (else
            (fn l tail)))))



(define (improper-fold-right fn tail l)
  (let rec ((l l))
    (cond ((null? l)
           tail)
          ((pair? l)
           (fn (car l)
               (rec (cdr l))))
          (else
            (fn l tail)))))



(define (improper-append a b)
  (improper-fold-right cons b a))




(define (improper-last v)
  (if (pair? v)
      (let ((v* (cdr v)))
	(if (null? v*)
	    (car v)
	    (improper-last v*)))
      v))



(define (improper-for-each proc v)
  (let lp ((v v))
    (cond ((pair? v)
	   (proc (car v))
	   (lp (cdr v)))
	  ((null? v)
	   (void))
	  (else
	   (proc v)
	   (void)))))



;;;
;;;; collect-if
;;;


(define (collect-if p l)
  (if (null? l)
      (list)
    (if (p (car l))
        (cons (car l) (collect-if p (cdr l)))
      (collect-if p (cdr l)))))

;;;
;;;; filter
;;;


; (define (filter pred? lst)
;   (cond
;    ((null? lst) '())
   
;    ((pred? (car lst))
;     (cons (car lst)
;           (filter pred? (cdr lst))))
;    (else
;     (filter pred? (cdr lst)))))
;now provided by srfi-1


(define (every pred? l)
  (cond ((null? l)       #t)
        ((pred? (car l)) (every pred? (cdr l)))
        (else            #f)))


(define (some pred? l)
  (cond ((null? l)       #f)
        ((pred? (car l)) #t)
        (else            (some pred? (cdr l)))))


;; |every| is also in srfi-1; some is called |any| in srfi-1. Testing
;; short-cutting behaviour:




(define (prisme-true? x)
  (cond ((eq? x #t) #t)
        ((eq? x #f) #f)
        (else       (not (null? x)))))


;;;
;;;; remove
;;;


(define (remove pred? lst)
  (filter (lambda (x)
            (not (pred? x)))
          lst))


;;;
;;;; remove-index
;;;


(define (remove-index lst index)
  (append (sublist lst 0 index) (sublist lst (+ index 1)  (length lst))))



;(remove-index '(a b c 3 4 5) 3)


;;;
;;;; find-if
;;;


(define (find-if p l)
  (if (null? l)
      #f
    (if (p (car l))
        (car l)
      (find-if p (cdr l)))))


;;;
;;;; find-it
;;;; return first (sub)list with key as first element
;;;


(define (find-it key l)
  (let ((res #f))
    (letrec ((proc (lambda (substruc)
                     (cond ((or res (null? substruc))
                            #t)
                           ((or (not (list? (car substruc)))
                                (null? (car substruc)))
                            (proc (cdr substruc)))
                           ((equal? key (caar substruc))
                            (set! res (car substruc)))
                           (else
                             (proc (car substruc))
                             (proc (cdr substruc)))))))
      (proc l)
      res)))


;;;
;;;; find-it-sxml
;;;; return first (sub)list with key as first element ignoring namespaces
;;;; For list built from XML
;;;

(define (find-it-sxml key l)
  (let ((res #f))
    (letrec ((proc (lambda (substruc)
                     (cond ((or res (null? substruc))
                            #t)
                           ((or (not (list? (car substruc)))
                                (null? (car substruc)))
                            (proc (cdr substruc)))
                           ((or (equal? key (caar substruc))
                                (let ((l (split-string (symbol->string (caar substruc)) #\:))) 
                                  (and (= (length l) 2)                                        
                                       (equal? key (string->symbol (second l)))))) ;;namespace ignored                                     
                            (set! res (car substruc)))
                           (else
                             (proc (car substruc))
                             (proc (cdr substruc)))))))
      (proc l)
      res)))


;;;
;;;; find-it-anywhere
;;;; return first (sub)list containing key
;;;

(define (find-it-anywhere key l)
  (let ((res #f))
    (letrec ((proc (lambda (substruc)
                     (cond ((or res (null? substruc))
                            #t)
                           ((or (not (list? (car substruc)))
                                (null? (car substruc)))
                            (proc (cdr substruc)))
                           ((member key (car substruc))
                            (set! res (car substruc)))
                           (else
                             (proc (car substruc))
                             (proc (cdr substruc)))))))
      (proc l)
      res)))

;;;
;;;; first-same
;;;


(define (first-same l1 l2)
  (find-if (lambda (e1)
             (member e1 l2))
           l1))

;;;
;;;; list-position
;;;


(define (list-position e l)
  (letrec ((lprec (lambda (e l i)
                    (if (equal? (car l) e)
                        i
                      (if (null? (cdr l))
                          #f
                        (lprec e (cdr l) (+ i 1)))))))
    (lprec e l 0)))


;;;
;;;; replace
;;;
          

(define (replace l e1 e2)
  (if (null? l)
      l
    (if (equal? (car l) e1)
        (cons e2 (replace (cdr l) e1 e2) )
      (cons (car l) (replace (cdr l) e1 e2)))))


          
;;;
;;;; decompose-list-in-pairs
;;;


(define (decompose-list-in-pairs l)
  (let ((qnb-pairs (/ (length l) 2)))
    (and (integer? qnb-pairs)
         (let ((result (list))) 
           (for 0 (- qnb-pairs 1)
             (lambda (i)
               (set! result (append result (list (list (list-ref l (* i 2)) (list-ref l (+ 1 (* i 2)))))))))
           result))))
    
 
;;;
;;;; repeat
;;;


(define (repeat n proc)
  (if (> n 0)
      (begin
        (proc)
        (repeat (- n 1) proc))))


;;;
;;;; for loop
;;; 


(define (for si ei fct)
  (fct si)
  (if (< si ei)
      (for (+ si 1) ei fct)))


;;;
;;;; exceptions
;;;


(define-macro (catch catcher . body)
  `(with-exception-handler
    ,catcher
    (lambda ()
      ,@body)))


;;;
;;;; sublist
;;;
;;; (sublist '(1 2 3 4)  1  2) = (2)
;;; (sublist '(1 2 3 4)  1)    = (2 3 4)
;;; (sublist '(1 2 3 4) -1)    = (4)
;;; (sublist '(1 2 3 4) -2 -1) = (3)
;;; (sublist '(1 2 3 4)  1 -1) = (2 3)
;;;


(define (sublist lst start #!optional (end #f))
  (letrec ((find-end   (lambda (l i) (if (>= 0 i) (list) (cons (car l) (find-end (cdr l) (- i 1))))))
           (find-start (lambda (l i) (if (>= 0 i) l (find-start (cdr l) (- i 1))))))
    (let* ((start      (if (< start 0) (+ (length lst) start) start))
           (end        (and end (- (if (< end 0) (+ (length lst) end) end) start)))
           (res        (find-start lst start)))
      (if end
          (find-end res end)
        res))))


;;;
;;;; identities
;;;


(define (identities x . rest)
  (cons x rest))


;;;
;;;; flatten
;;;


(define (flatten tree)
  (cond ((null? tree) '())
        ((null? (car tree))
         (flatten (cdr tree)))
        ((pair? (car tree))
         (append (flatten (car tree))
                 (flatten (cdr tree))))
        (else
          (cons (car tree)
                (flatten (cdr tree))))))

;;;
;;;; make-set
;;;


(define (make-set l)
  (letrec ((make-set-rec (lambda (ltp pl)
                           (if (null? ltp)
                               pl
                             (if (member (car ltp) pl)
                                 (make-set-rec (cdr ltp) pl)
                               (make-set-rec (cdr ltp) (cons (car ltp) pl)))))))
    (reverse (make-set-rec l (list)))))


(define (set-minus s1 s2)
  (make-set (collect-if (lambda (e1)
                          (not (member e1 s2)))
                        s1)))



(define (make-set2 l #!key (key identity))
  (letrec ((make-set-rec (lambda (ltp pl plk)
                           (if (null? ltp)
                               pl
                             (if (member (key (car ltp)) plk)
                                 (make-set-rec (cdr ltp) pl plk)
                               (make-set-rec (cdr ltp) (cons (car ltp) pl) (cons (key (car ltp)) plk)))))))
    (reverse (make-set-rec l (list) (list)))))


;;;
;;; --
;;; Add an element to the end of a list 
;;;


(define (add-end-list x l)
  (if (null? l)
      (list x)
      (cons (car l) (add-end-list x (cdr l)))))


(define (add-end-list! x l)
  (if (null? (cdr l))
      (set-cdr! l (list x))
      (add-end-list! x (cdr l))))



(define-macro (push! var val)
  `(set! ,var (cons ,val ,var)))



(define (list-preferred lis prefer?)
  (if (pair? lis)
      (let lp ((lis (cdr lis))
	       (min-val (car lis)))
	(cond ((pair? lis)
	       (lp (cdr lis)
		   (let ((val (car lis)))
		     (if (prefer? val
				  min-val)
			 val
			 min-val))))
	      ((null? lis)
	       min-val)
	      (else
	       (error "list-preferred: improper list:" lis))))
      (error "list-preferred: need non-empty list, got:" lis)))


;; same as list-preferred but also return the list fragments after
;; (including) and before (excluding) the choosen element

(define (split-preferred lis prefer?)
  (if (pair? lis)
      (let lp ((l (cdr lis))
	       (rbefore (cons (car lis) '()))
	       (min-v (car lis))
	       (min-rest lis)
	       (min-rbefore '()))
	(cond ((pair? l)
	       (let* ((v (car l))
		      (r (cdr l))
		      (rbefore* (cons v rbefore)))
		 (if (prefer? v min-v)
		     (lp r rbefore*
			 v
			 l
			 rbefore)
		     (lp r rbefore*
			 min-v
			 min-rest
			 min-rbefore))))
	      ((null? l)
	       (values min-v min-rest min-rbefore))
	      (else
	       (error "improper list ending in:" l))))
      (error "need non-empty list, got:" lis)))


;; an append that reverses l1 when appending to l2
(define (rappend l1 l2)
  (let lp ((l1 l1)
	   (l l2))
    (cond ((pair? l1)
	   (lp (cdr l1)
	       (cons (car l1) l)))
	  ((null? l1)
	   l)
	  (else
	   (error "improper list l1 ending in:" l1)))))



;; an alist reference function that requires key to be a symbol and
;; expects it to be found

(define (symbol-alist-ref alis key)
  (if (symbol? key)
      (cond ((assq key alis)
	     => cdr)
	    (else
	     (error "key not found:" alis key)))
      (error "key is not a symbol:" key)))


;;;
;;;; mutex operations
;;;

(define (make-named-mutex n)
  (let ((newm (make-mutex)))
    (mutex-specific-set! newm n)
    newm))



(define (safe-mutex-lock! m)
  (with-exception-catcher
    (lambda (e)
      (continuation-capture
        (lambda (k)
          (let* ((specific (mutex-specific m))
                 (cspec (and specific (symbol? specific) specific))
                 (detailed-log-message (string-append "\n\n" (formatted-current-time) ": " "Error locking mutex: " (symbol->string specific) "\n"  (compute-verbose-error-message e k))))
            (log-message detailed-log-message)
            (eval `(let ((newm (make-mutex)))
                     (and 
                       cspec 
                       (begin 
                         (mutex-specific-set! newm ',cspec)
                         (set! ,cspec newm)))
                     (mutex-lock! newm)))))))
    (lambda ()
      (mutex-lock! m))))


(define (safe-mutex-unlock! m)
  (with-exception-catcher
    (lambda (e)
      (continuation-capture
        (lambda (k)
          (let* ((specific (mutex-specific m))
                 (cspec (and specific (symbol? specific) specific))
                 (detailed-log-message (string-append "\n\n" (formatted-current-time) ": " "Error unlocking mutex: " (symbol->string specific) "\n"  (compute-verbose-error-message e k))))
            (log-message detailed-log-message)
            (eval `(let ((newm (make-mutex)))
                     (and cspec 
                          (begin 
                            (mutex-specific-set! newm ',cspec)
                            (set! ,cspec newm)))))))))
    (lambda ()
      (mutex-unlock! m))))


;;;
;;;; log-message
;;;



(define (log-message message)
  (dart-display
    (string-append 
      (format-datetime (get-current-time))
      " | "
      message
      "\n")))


(define (_pp obj)
  (log-message (>string obj)))


;;;
;;;; with-safe-mutex
;;;
  

(define (with-safe-mutex m thunk)
  (safe-mutex-lock! m)
  (with-exception-catcher
    (lambda (e)
      (let* ((specific (mutex-specific m))
             (cspec (and specific (symbol? specific) specific)))
        (if cspec 
            (log-message (string-append "Error inside safe mutex: " (symbol->string cspec)))
          (log-message "Error inside safe mutex."))))
    thunk)
  (safe-mutex-unlock! m))
      

(define (test-safe-nomutexops)
  (mutex-lock! Test-Mutex)
  (mutex-unlock! Test-Mutex)
  
  (thread-start!
    (make-thread
      (lambda ()
        (mutex-lock! Test-Mutex)
        (thread-terminate! (current-thread))
        (mutex-unlock! Test-Mutex))))
  
  
  (mutex-lock! Test-Mutex))


(define (test-safe-mutexops)
  (safe-mutex-lock! Test-Mutex)
  (safe-mutex-unlock! Test-Mutex)
  
  (thread-start!
    (make-thread
      (lambda ()
        (safe-mutex-lock! Test-Mutex)
        (thread-terminate! (current-thread))
        (safe-mutex-unlock! Test-Mutex))))
  
  
  
  (safe-mutex-lock! Test-Mutex)
  (safe-mutex-unlock! Test-Mutex))


;;;
;;;; with-conditional-exception-catcher
;;;


(define (with-conditional-exception-catcher flag exeption-handler thunk)
  (if flag
      (with-exception-catcher
        exeption-handler thunk)
    (thunk)))
