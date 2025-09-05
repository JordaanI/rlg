;;;
;;;; hybrid logical clocks
;;;; inspired by James Long 
;;;
;;; https://github.com/jlongster/crdt-example-app/tree/master
;;;


;;;
;;;; constants
;;;


(define ClockMaxDrift 60000)


;;;
;;;; hlc-make-timestamp
;;;


(define (hlc-make-timestamp ms counter node)
  (list->table 
    `(("millis" . ,ms)
      ("counter" . ,counter)
      ("node" . ,node))))


(define (hlc-make-timestamp-since min)
  (list->table 
    `(("millis" . ,(* min 60 1000))
      ("counter" . 0)
      ("node" . "00000000000000000000000000000000"))))


(define (hlc-get-millis ts)
  (table-ref ts "millis"))


(define (hlc-get-counter ts)
  (table-ref ts "counter"))


(define (hlc-get-node ts)
  (table-ref ts "node"))


(define (hlc-set-millis ts millis)
  (table-set! ts "millis" millis))


(define (hlc-set-counter ts counter)
  (table-set! ts "counter" counter))


(define (hlc-set-node ts node)
  (table-set! ts "node" node))


(define (hlc-timestamp-from ts)
  (hlc-make-timestamp (hlc-get-millis ts) (hlc-get-counter ts) (hlc-get-node ts)))


;;;
;;;; hlc-timestamp-tostring
;;;


(define (hlc-timestamp-tostring ts)
  (let ((milis (table-ref ts "millis"))
        (counter (table-ref ts "counter"))
        (node (table-ref ts "node")))
    (join-strings
      (list 
        (time-list->iso-timestamp (miliseconds->time-list milis zone-factor: 0))
        (unsignedint->hex counter)
        node)
      "-")))


;;;
;;;; hlc-send
;;;


(define (hlc-send clock)
  (let* ((phys (get-current-timestamp-millis))
         (lOld (hlc-get-millis (clock-get-timestamp clock)))
         (cOld (hlc-get-counter (clock-get-timestamp clock)))
         (lNew (max lOld phys))
         (cNew (if (= lOld lNew)
                   (+ cOld 1)
                 0)))
    (if (> (- lNew phys) ClockMaxDrift)
        (error "clock drift error"))
    (if (> cNew 65535)
        (error "clock counter overflow error"))
    
    
    (hlc-set-millis (clock-get-timestamp clock) lNew)
    (hlc-set-counter (clock-get-timestamp clock) cNew)
    (hlc-make-timestamp lNew cNew (hlc-get-node (clock-get-timestamp clock)))))


;;;
;;;; hlc-recv
;;;


(define (hlc-recv clock sent-ts)
  (let* ((phys (get-current-timestamp-millis))
         (lSent (hlc-get-millis sent-ts))
         (cSent (hlc-get-counter sent-ts)))
    (if (equal? (hlc-get-node sent-ts) (hlc-get-node (clock-get-timestamp clock)))
        (error "Duplicate node ID"))
    
    (if (> (- lSent phys) ClockMaxDrift)
        (error "clock drift error in recv"))
    
    (let* ((lOld (hlc-get-millis (clock-get-timestamp clock)))
           (cOld (hlc-get-counter (clock-get-timestamp clock)))
           (lNew (max lOld phys lSent))
           (cNew (if (and (= lNew lOld) (= lNew lSent))
                     (+ 1 (max cOld cSent))
                   (if (= lNew lOld)
                       (+ 1 cOld)
                     (if (= lNew lSent)
                         (+ cSent 1)
                       0)))))
      (if (> (- lNew phys) ClockMaxDrift)
          (error "clock drift error in recv"))
      (if (> cNew 65535)
          (error "clock counter overflow error in recv"))
      (hlc-set-millis (clock-get-timestamp clock) lNew)
      (hlc-set-counter (clock-get-timestamp clock) cNew)
      (hlc-make-timestamp lNew cNew (hlc-get-node (clock-get-timestamp clock))))))


;;;
;;;; hlc-parse
;;;


(define (hlc-parse s)
  (let* ((parts (split-string s #\-))
         (year (string->number (first parts)))
         (month (- (string->number (second parts)) 1))
         (day/time (split-string (third parts) #\T))
         (day (- (string->number (first day/time)) 1))
         (timestring (second day/time))
         (hour/min/secms (split-string timestring #\:))
         (hour (string->number (first hour/min/secms)))
         (minutes (string->number (second hour/min/secms)))
         (secms (third hour/min/secms))
         (sec/ms (split-string secms #\.))
         (sec (string->number (first sec/ms)))
         (ms (string->number (substring (second sec/ms) 0 3))))
    (hlc-make-timestamp
      (time-list->milliseconds (list year month day hour minutes sec ms)
                               zone-factor: 0)
      (hexstring->unsigned-int (fourth parts))
      (fifth parts))))


;;;
;;;; hlc-hash
;;;


(define (hlc-hash ts)
  (murmur3 (hlc-timestamp-tostring ts)))


;;;
;;;; hlc-make-clock
;;;


(define (hlc-make-clock ts #!key (merkle (make-table)))
  (list->table
    `(("timestamp" . ,(hlc-timestamp-from ts))
      ("merkle" . ,merkle))))


(define (clock-get-timestamp clock)
  (table-ref clock "timestamp"))


(define (clock-get-merkle clock)
  (table-ref clock "merkle"))


;;;
;;;; hlc-serialize-clock
;;;


(define (hlc-serialize-clock clock)
  (json-write-string 
    (list->table
      `(("timestamp" . ,(hlc-timestamp-tostring (clock-get-timestamp clock)))
        ("merkle" . ,(table-ref clock "merkle"))))))


;;;
;;;; hlc-deserialize-clock
;;;


(define (hlc-deserialize-clock sclock)
  (let ((parsed (json-read-string sclock)))
    (list->table
      `(("timestamp" . ,(hlc-timestamp-from (hlc-parse (table-ref parsed "timestamp"))))
        ("merkle" . ,(table-ref parsed "merkle"))))))
  
         


      
         

      

