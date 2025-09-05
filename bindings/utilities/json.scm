
;;;
;;;;
;;;; json-write
;;;

;; Modified version of snowball JSON 
;; Original code from Dominique Boucher (SchemeWay) <schemeway at sympatico.ca>


(define (json-write-string value)
  (with-output-to-string ""
             (lambda ()
               (json-write value))))


(define (json-write value)
  (write-value value))
  
  
(define (write-object object)
    (write-char #\{)
    (let ((first? #t))
      (table-for-each (lambda (key value)
                        (if (not (string? key))
                            (raise 'invalid-json-object))
                        (if (not first?)
                            (display ", "))
                        (write key)
                        (display ": ")
                        (write-value value)
                        (set! first? #f))
                      object))
    (write-char #\}))

  
(define (write-array elements)
  (write-char #\[)
  (let ((first? #t))
      (for-each (lambda (value)
                  (if (not first?)
                      (display ", "))
                  (write-value value)
                  (set! first? #f))
                elements))
    (write-char #\]))
  
(define (write-string str)
  (let* ((cr-str (string-replace-all str "\n" "\\n"))
         (c2 (string-replace-all str "\"" "\\\"")))
    (display (string-append "\"" c2 "\""))))
  
(define (write-number num)
  (let ((str (number->string num)))
    (if (char=? (string-ref str 0) #\.)
    (begin
      (display "0")
      (display str))
    (if (char=? (string-ref str (- (string-length str) 1)) #\.)
        (begin
          (display str)
          (display "0"))
        (display str)))))

(define (write-constant value)
  (cond ((eq? value #f)
     (display "false"))
    ((eq? value #t)
     (display "true"))
    ((json-null? value)
     (display "null"))
    (else
     ;(__pp value)
     (raise 'invalid-json-object))))

(define (write-value value)
  (cond ((table? value)
     (write-object value))
    ((list? value)
     (write-array value))
    ((real? value)
     (write-number value))
    ((string? value)
     (write-string value))
    ((symbol? value)
     (write-string (symbol->string value)))
    (else
     (write-constant value))))


;;;
;;;; Some constants
;;;


(define json-null (vector 'json-null))

(define (json-null? obj)
  (eq? obj json-null))



;;;
;;;;
;;;; json-write example
;;;



(define (test-json-write)
  (json-write-string (list->table
              `(("documents" .
             (,(list->table
                '(("id" . "d1")
                  ("title" . "inventory")
                  ("location" . "http://www.inv.com")))
              ,(list->table
                '(("id" . "d2")
                  ("title" . "todo")
                  ("location" . "http://www.todo.com")))))
            ("actors" . 
             (,(list->table
                '(("id" . "fbergeron")
			      ("name" . "Frederic Bergeron")
                  ("location" . "http://www.fbergeron.com")))
              ,(list->table
                '(("id" . "magnan")
                  ("name" . "Francois Magnan")
                  ("location" . "http://www.iro.umontreal.ca/~gambit")))))))))
          

;;;
;;;; JSON object constructor
;;;

;; Converts an a-list to a JSON object (a Scheme table)
;; TODO: check values and keys

(define json-object list->table)


;;;
;;;; JSON reader
;;;


(define (json-read #!optional (port (current-input-port)))
  
  (define lookahead (peek-char port))
  
  (define (consume)
    (read-char port)
    (set! lookahead (peek-char port)))
  
  (define (match-char ch message #!optional (consume? #t))
    (if (not (eqv? lookahead ch))
        (error message)
      (if consume?
          (consume))))
  
  (define (skip-ws)
    (if (and (char? lookahead) (char-whitespace? lookahead))
        (begin
          (consume)
          (skip-ws))))
  
  (define (read-object)
    (let ((object (make-table)))
      (match-char #\{ "object must begin with a '{'")
      (skip-ws)
      (if (eq? lookahead #\})
          (begin
            (consume)
            object)
        (let loop ()
             (let ((key (read-value)))
               (if (not (string? key))
                   (error "key must be a string"))
               (skip-ws)
               (match-char #\: "key must be following by ':'")
               (let ((value (read-value)))
                 (table-set! object key value)
                 (skip-ws)
                 (if (eq? lookahead #\,)
                     (begin
                       (consume)
                       (loop))
                   (begin
                     (match-char #\} "object must be terminated by '}'")
                     object))))))))
  
  
  (define (read-array)
    (match-char #\[ "array must begin with a '['")
    (skip-ws)
    (if (eq? lookahead #\])
        (begin (consume) '())
      (let loop ((elements (list (read-value))))
           (skip-ws)
           (cond ((eq? lookahead #\])
                  (consume)
                  (reverse elements))
                 ((eq? lookahead #\,)
                  (consume)
                  (loop (cons (read-value) elements)))
                 (else
                  (raise 'invalid-json-array))))))
  
  
  
  (define (read-string)
    (match-char #\" "string must begin with a double quote" #f)
    (let ((str (read port)))
      (set! lookahead (peek-char port))
      str))
  
  
  (define (read-number)
    (let ((op (open-output-string)))
      ;; optional minus sign
      (if (eq? lookahead #\-)
          (begin
            (consume)
            (write-char #\- op)))
      
      ;; integral part
      
      (cond ((eq? lookahead #\0)
             (consume)
             (write-char #\0 op))
            ((and (char? lookahead) (char-numeric? lookahead))
             (let loop ()
                  (write-char lookahead op)
                  (consume)
                  (if (and (char? lookahead) (char-numeric? lookahead))
                      (loop))))
            (else
             (raise 'invalid-json-number)))
      
      (if (eq? lookahead #\.)
          (begin
            (write-char #\. op)
            (consume)
            (if (and (char? lookahead) (char-numeric? lookahead))
                (let loop ()
                     (write-char lookahead op)
                     (consume)
                     (if (and (char? lookahead) (char-numeric? lookahead))
                         (loop)
                       ;;  e | E
                       (if (or (eq? lookahead #\e) (eq? lookahead #\E))
                           (begin
                             (write-char lookahead op)
                             (consume)
                             ;; [ + | - ]
                             (if (or (eq? lookahead #\+) (eq? lookahead #\-))
                                 (begin
                                   (write-char lookahead op)
                                   (consume)))
                             ;; digit+
                             (if (and (char? lookahead) (char-numeric? lookahead))
                                 (let loop ()
                                      (write-char lookahead op)
                                      (consume)
                                      (if (and (char? lookahead) (char-numeric? lookahead))
                                          (loop)))
                               (raise 'invalid-json-number))))))
              (raise 'invalid-json-number))))
      
      (string->number (get-output-string op))))
  
  
  
  (define (read-constant)
    (let loop ((chars '()))
         (if (and (not (eof-object? lookahead))
                  (char-alphabetic? lookahead))
             (let ((ch lookahead))
               (consume)
               (loop (cons ch chars)))
           (let ((str (list->string (reverse chars))))
             (cond ((string=? str "false") #f)
                   ((string=? str "true")  #t)
                   ((string=? str "null")  json-null)
                   (else                   (raise 'invalid-json-constant)))))))
  
  
  
  (define (read-value)
    (skip-ws)
    (cond ((eof-object? lookahead)
           (raise 'unexpected-eof))
          ((char=? lookahead #\{)
           (read-object))
          ((char=? lookahead #\[)
           (read-array))
          ((char=? lookahead #\")
           (read-string))
          ((or (char-numeric? lookahead) (char=? lookahead #\-))
           (read-number))
          ((char-alphabetic? lookahead)
           (read-constant))
          (else
           (raise 'json-syntax-error))))
  
  (read-value))


;;;
;;;; json-read-string
;;;


(define (json-read-string s)
  (with-input-from-string s
    (lambda ()
      (json-read))))


;;;
;;;; table-apply-properties 
;;;


(define (table-apply-properties t props)
  (for-each (lambda (ppair)
              (table-set! t (car ppair) (cadr ppair)))
            props))


; shortcut version

(define @p table-apply-properties)

