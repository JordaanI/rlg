
;;;
;;;; truncate-string
;;;


(define (truncate-string s size)
  (if (> (string-length s) size)
      (string-append (substring s 0 (- size 3)) "...")
    s))
 


;;;
;;;; split-string
;;;


(define (split-string str sep-char)
  (call-with-input-string
   str
   (lambda (p) (read-fields p sep-char #f))))

(define (join-strings strings sep-char)
  (cond ((null? strings) "")
        ((= (length strings) 1) (car strings))
        (else (string-append (car strings)
			     (or
			      (and (string? sep-char) sep-char)
			      (char->string sep-char))
                             (join-strings (cdr strings) sep-char)))))

(define (join-strings-with-string strings sep-str)
  (cond ((null? strings) "")
        ((= (length strings) 1) (car strings))
        (else (string-append (car strings)
                             sep-str
                             (join-strings-with-string (cdr strings) sep-str)))))

(define (read-fields port sep inc-sep?)
  (read-all port (lambda (p) (read-line p sep inc-sep?))))


;;;
;;;; char->string
;;;


(define (char->string char)
  (list->string (list char)))


;;;
;;;; 8bitstring->utf8string
;;;


(define (8bitstring->utf8string s)
  (u8vector->utf8string (8bitstring->u8vector s)))


(define (u8vector->utf8string u8vector)
  (with-input-from-u8vector (list init: u8vector char-encoding: 'UTF-8)
   (lambda () 
     (read-line (current-input-port) #f))))


(define (utf8-string->u8vector utf8string)
  (with-output-to-u8vector (list char-encoding: 'UTF-8)
    (lambda ()
      (display utf8string))))


(define (8bitstring->u8vector s)
  (let* ((len (string-length s))
         (res (##make-u8vector len)))
    (let lp ((i 0))
         (if (= i len)
             res
           (let ((c (char->integer (string-ref s i))))
             (if (> c 255)
                 (error "8bitstring->u8vector: input string contains character over 8bit range:" c))
             (u8vector-set! res i c)
             (lp (inc i)))))))


(define (u8vector->utf8string-new u8vector)
  (let ((res (string)))
    (with-input-from-u8vector (list init: u8vector char-encoding: 'UTF-8)
      (lambda ()
        (letrec ((reader (lambda ()
                           (let ((char (read-char)))
                             (if (eof-object? char)
                                 res
                               (begin
                                 (set! res (string-append res (list->string (list char))))
                                 (reader)))))))
          (reader))))
    res
    ))
  

;;;
;;;; string-starts-with?
;;;


(define (string-starts-with? s p)
  (and (>= (string-length s) (string-length p))
       (equal? p (substring s 0 (string-length p)))))
       
       
(define (string-end-with? match s)
  (let ((match-length (string-length match))
        (s-length (string-length s)))
    (and (>= s-length match-length)
         (equal? (substring s (- s-length match-length) s-length) match))))


;;;
;;;; Scheme parsing of strings
;;;


(define (read-from-string string)
  (with-input-from-string string (lambda () (read))))


(define (read-all-from-string string)
  (with-input-from-string string (lambda () (read-all))))


(define (lstring-append l)
  (apply string-append l))


;;;
;;;; convert-to-symbol
;;;


(define (convert-to-symbol symbolOrString)
  (cond 
   ((symbol? symbolOrString)
    symbolOrString)
   ((string? symbolOrString)
    (string->symbol symbolOrString))
   (else
    #f)))
    
    
;;;
;;;; convert-to-string 
;;;


(define (convert-to-string symbolOrString)
  (cond 
   ((symbol? symbolOrString)
    (symbol->string symbolOrString))
   ((string? symbolOrString)
    symbolOrString)
   (#t
    #f)))


;;;
;;;; escape-string-escapes
;;;


(define (escape-string-escapes s)
  (list->string (do-replace-escapes (string->list (string-replace-all s "&apos;" "'")))))


;(escape-string-escapes "Type d'equipement")
;(escape-string-escapes "Type d'equipement")

(define (do-replace-escapes l)
  (cond 
    ((null? l)
     (list))
    ((equal? (car l) #\")
     (append (list #\\ #\") (do-replace-escapes (cdr l))))
    ((equal? (car l) #\')
     (append (list #\\ #\') (do-replace-escapes (cdr l))))
    ((equal? (car l) #\newline)
     (append (list #\\ #\n) (do-replace-escapes (cdr l))))
    (#t
      (cons (car l) (do-replace-escapes (cdr l))))))


;;;
;;;; memstring
;;;


(define (memstring sstring string)
  (let ((res #f))
    (if (>= (string-length string) (string-length sstring))
        (for 0 (- (string-length string) (string-length sstring))
          (lambda (i)
            (if (and (not res) (matching-initial-segment? sstring (substring string i (string-length string))))
                (set! res i)))))
    (and res (substring string res (string-length string)))))


;(define (memstring sstring string)
;  (if (matching-initial-segment? sstring string)
;      string
;      (if (string=? string "")
;          #f
;          (memstring sstring (substring string 1 (string-length string))))))


(define (string-position ss s)
  (let ((memtail (memstring ss s)))
    (and memtail
         (- (string-length s) (string-length memtail)))))


(define (matching-initial-segment? sstring string)
  (let ((sslength (string-length sstring)))
    (if (<= sslength (string-length string))
        (equal? sstring (substring string 0 sslength))
        #f)))


;;;
;;;; lower/upper string-+
;;;


(define (lowerString s)
  (list->string (map char-downcase (string->list s))))

(define (upperString s)
  (list->string (map char-upcase (string->list s))))

(define (capitalizeString s)
  (if (<= (string-length s) 1)
      (upperString s)
      (let ((chars (string->list s)))
        (list->string (append (list (char-upcase (car chars))) (cdr chars))))))


;;;
;;;; string-replace-all
;;;


(define (string-replace-all s x y)
  (let ((mem (memstring x s)))
  (if mem
      (string-append (substring s 0 (- (string-length s) (string-length mem))) 
                     y
                     (string-replace-all (substring mem (string-length x) (string-length mem)) x y))
      s)))


;;;
;;;; Given a list of symbol-value pairs (like '( ("$NAME" . "Fred") ("$AGE" . 35) ))
;;; Returns a string where all the occurences of the symbols have been replaced by
;;; their associated value. 
;;;


(define (substitute-symbols str value-pairs)
  (if (null? value-pairs)
      str
      (let* ((value-pair (car value-pairs))
            (symbol (car value-pair))
            (value (cdr value-pair)))
        (substitute-symbols (string-replace-all str symbol value) (cdr value-pairs)))))


;;;
;;;; strip-all-spaces
;;;


(define (strip-all-spaces s)
  (let ((without-spaces (string-replace-all s " " "")))
    (string-replace-all without-spaces "\n" "")))


;;;
;;;; generate-guid
;;;


(define (generate-guid)
  (string-append "ID" (number->string (random-integer 10000000000000)) "_" (get-current-timestamp)))

(define (get-current-time) (time->seconds (current-time)))
(define (get-current-time-round) (inexact->exact (round (get-current-time))))
(define (get-current-timestamp)       (number->string (get-current-timestamp-micro)))

(define (get-current-timestamp-millis)       (floor (/ (string->number (get-current-timestamp)) 1000)))
(define (get-current-timestamp-micro)       (inexact->exact (floor (* 1000000 (get-current-time)))))

;;;
;;;; safe-getenv
;;;
                

(define (safe-getenv vname)
  (with-exception-catcher 
    (lambda (e)
      #f)
    (lambda ()
      (getenv vname))))


(define (>string v)
  (if (string? v)
      v
    (with-output-to-string (string)
      (lambda ()
        (display v)))))


;;;
;;;; u8vector->hexstring
;;;


(define (u8vector->hexstring v)
  (let ((res (string)))
    (for-each (lambda (byte-int)
                (set! res (string-append res (short->hex byte-int))))
              (u8vector->list v))
    res))


(define hex-chars (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))


(define (short->hex byte-int)
  (string-append
    (list-ref hex-chars (floor (/ byte-int 16)))
    (list-ref hex-chars (modulo byte-int 16))))


(define (unsignedint->hex i)
  (let* ((x1 (floor (/ i 4096)))
         (r1 (- i (* x1 4096)))
         (x2 (floor (/ r1 256)))
         (r2 (- r1 (* x2 256)))
         (x3 (floor (/ r2 16)))
         (x4 (modulo i 16)))
    (string-append
      (list-ref hex-chars x1)
      (list-ref hex-chars x2)
      (list-ref hex-chars x3)
      (list-ref hex-chars x4))))


(define (hexstring->unsigned-int s)
  (let ((x1 (substring s 0 1))
        (x2 (substring s 1 2))
        (x3 (substring s 2 3))
        (x4 (substring s 3 4)))
    (+ (* (list-position x1 hex-chars) 4096)
       (* (list-position x2 hex-chars) 256)
       (* (list-position x3 hex-chars) 16)
       (list-position x4 hex-chars))))


;;;
;;;; pad-string-with
;;;


(define (pad-string-with s p n)
  (let ((l (string-length s)))
    (string-append (list->string (map (lambda (i) p) (iota (- n l)))) s)))
    

;;;
;;;; write-bytes-to-file
;;;


(define (write-bytes-to-file file v)
  (with-output-to-file (list path: file char-encoding: 'ISO-8859-1)
    (lambda ()
      (write-subu8vector v 0 (u8vector-length v)))))


;;;
;;;; read-bytes-from-file
;;;


(define (read-bytes-from-file file)
  (let* ((info (file-info file))
         (len (file-info-size info))
         (v (make-u8vector len)))
    (with-input-from-file (list path: file char-encoding: 'ISO-8859-1)
      (lambda ()
        (read-subu8vector v 0 len)))
    v))


;;;
;;;; empty-string?
;;;


(define (empty-string? s)
  (equal? s ""))


;;;
;;;; code-encode / code-decode
;;;


; simple obfuscation to not store sensitive info in repo in clear.

(define (_shift-n b)
  (lambda (x)
    (let* ((n (u8vector-length x))
           (y (make-u8vector n)))
      (for-each (lambda (i)
                  (u8vector-set! y i (modulo (+ (u8vector-ref x i) b) 256)))
                (iota n))
      y)))

(define (_shift-n^-1 b)
  (lambda (x)
    (let* ((n (u8vector-length x))
           (y (make-u8vector n)))
      (for-each (lambda (i)
                  (u8vector-set! y i (modulo (- (u8vector-ref x i) b) 256)))
                (iota n))
      y)))


(define code-encode
  (lambda (s)
    (u8vector->base64-string ((_shift-n 4) (object->u8vector s)))))


(define code-decode
  (lambda (e)
    (u8vector->object ((_shift-n^-1 4) (base64-string->u8vector e)))))
