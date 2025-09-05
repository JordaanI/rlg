;;;
;;; Monadic Parser Combinators
;;; ==========================
;;;
;;; An efficient functional way to implement parsers.
;;; inspired by the paper: https://people.cs.nott.ac.uk/pszgmh/monparsing.pdf
;;; 
;;; Followed Racket implementation: https://github.com/stchang/parsack/blob/master/parsack-lib/parsack/parsack.rkt
;;;


;;; 
;;;; special types used by parsers
;;;

(define-structure Consumed reply)
(define-structure Empty reply)
(define-structure Ok parsed)
(define-structure Error)

; shorcuts
(define (Consumed r) (make-Consumed r))
(define (Empty r) (make-Empty r))
(define (Ok p) (make-Ok p))
(define (Error) (make-Error))


;;;
;;;; pstring-port
;;;


(define-structure pstring-port pos str)

(define (pstring-port str)
  (make-pstring-port 0 str))
  


;;;
;;;; ports-api
;;; because string-ports don't have capability to backtrack in gambit
;;;


(define (_read-char in)
  (cond
    ((pstring-port? in)
     (let* ((pos (pstring-port-pos in))
            (eof? (>= pos (string-length (pstring-port-str in))))
            (c (if eof?
                   (eof-object) 
                 (string-ref (pstring-port-str in) pos))))
       (if (not eof?) (pstring-port-pos-set! in (+ pos 1)))
       c))
    ((input-port? in)
     (read-char in))))


(define (_read-all in proc)
  (cond
    ((pstring-port? in)
     (let ((str (pstring-port-str in)))
       (substring str (pstring-port-pos in) (string-length str))))
    ((input-port? in)
     (read-all in read-line))))


(define (_peek-char in)
  (cond
    ((pstring-port? in)
     (let ((str (pstring-port-str in))
           (pos (pstring-port-pos in)))
       (if (>= pos (string-length str))
           (eof-object)
         (string-ref str (pstring-port-pos in)))))
    ((input-port? in)
     (peek-char in))))


(define (_input-port-byte-position in #!optional (pos #f))
  (cond
    ((pstring-port? in)
     (if pos
         (pstring-port-pos-set! in pos)
       (pstring-port-pos in)))
    ((input-port? in)
     (if pos
         (input-port-byte-position in pos)
       (input-port-byte-position in)))))


;;;
;;;; misc functional utils
;;;


(define-macro (thunk e) `(lambda () ,e))
(define (negate f) (lambda (x) (not (f x))))
(define (curry f x) (lambda args (apply f x args)))
(define (compose1 . fcts)
  (reduce compose-two fcts identity))
(define (compose-two f g)
  (lambda (x)
    (g (f x))))



;;;
;;;; global parameters 
;;;


(define (create-thread-env)
  (thread-specific-set! (current-thread) (make-table)))

(define (reset!-unexpected) (table-set! (thread-specific (current-thread)) 'current-unexpected ""))
(define (get-unexpected) (table-ref (thread-specific (current-thread)) 'current-unexpected))
(define (set!-unexpected str) (table-set! (thread-specific (current-thread)) 'current-unexpected str))

(define (reset!-expected) (table-set! (thread-specific (current-thread)) 'current-expected (list)))
(define (get-expected) (table-ref (thread-specific (current-thread)) 'current-expected))
(define (set!-expected exps) (table-set! (thread-specific (current-thread)) 'current-expected exps))
(define (merge!-expected exps1 exps2) (set!-expected (append exps1 exps2)))
(define (cons!-expected exp) (set!-expected (cons exp (get-expected))))
(define (append!-expected exps) (set!-expected (append exps (get-expected))))

(define (user-state-reset!) (table-set! (thread-specific (current-thread)) 'user-state (make-table)))
(define (user-state-get key) (table-ref (table-ref (thread-specific (current-thread)) 'user-state) key #f))
(define (user-state-set! key val) (table-set! (table-ref (thread-specific (current-thread)) 'user-state) key val))


;;;
;;;; errors
;;;


(define (err expected)
  (lambda (in)
    (set!-unexpected (thunk (read-all in read-line)))
    (set!-expected (list expected))
    (Empty (Error))))

(define $$err (err ""))

(define-macro (parser-error msg)
  `(error (string-append "parse ERROR: " ,msg)))


;;;
;;;; $$return
;;;


(define ($$return x)
  (lambda (in)
    (reset!-unexpected)
    (reset!-expected)
    (Empty (Ok x))))


;;;
;;;; $$satisfy
;;;


(define ($$satisfy p? #!key (read-one _read-char) (peek-one _peek-char))
  (lambda (in)
    (define c (peek-one in))
    (cond
      ((eof-object? c)
       (set!-unexpected "end of input")
       (reset!-expected)
       (Empty (Error)))
      ((p? c)
       (reset!-unexpected)
       (reset!-expected)
       (Consumed (Ok (read-one in))))
      (#t
       (set!-unexpected (thunk (if (char? c)
                                   (list->string (list c))
                                 (>string c))))
       (reset!-expected)
       (Empty (Error))))))


;;;
;;;; ofString
;;;


(define ($$ofString ? exps)
  (lambda (in)
    (let ((result (($$satisfy ?) in)))
      (cond 
        ((and result (or (equal? result (Consumed (Error)))
                         (equal? result (Empty (Error))))) ; error
         (cons!-expected exps)
         result)
        (#t 
         result)))))


(define ($$oneOf str)
  ($$ofString (make-char-in-string? str)
             (thunk (string-append "one of: " (str->strs str)))))

(define ($$noneOf str)
  ($$ofString (negate (make-char-in-string? str))
             (thunk (string-append "none of: " (str->strs str)))))

(define (make-char-in-string? str)
  (let ((ht (make-table))) 
    (for-each (lambda (cc) 
                (table-set! ht cc #t))
              (string->list str))
    (lambda (c)
      (table-ref ht c #f))))

(define (str->strs str)
  (format-exp (map (lambda (c) (list->string (list c))) (string->list str))))

(define ($$oneOfStrings . ss)
  ($$<?> ($$choice (map (compose1 try string) ss))
        (string-append "one of: "
          (join-strings-with-string ss ", "))))

(define ($$oneOfStringsAnyCase . ss)
  ($$<?> ($$choice (map (compose1 try $$stringAnyCase) ss))
        (string-append "one of: "
          (join-strings-with-string ss ", ")
          " (case insensitive)")))

;;;
;;;; >>= (monadic bind)
;;;


(define (>>= p f)
  (lambda (in)
    (let ((pres (p in)))
      (cond
        ((and (Empty? pres) (Ok? (Empty-reply pres)))
         (let* ((saved-expected (get-expected))
                (x (Ok-parsed (Empty-reply pres)))
                (fxres ((f x) in)))
           (if (Empty? fxres)
               (begin
                 (append!-expected saved-expected)
                 fxres)
             fxres)))
        ((and (Consumed? pres) (Ok? (Consumed-reply pres)))
         (let* ((saved-expected (get-expected))
               (x (Ok-parsed (Consumed-reply pres)))
               (fxres ((f x) in)))
           (Consumed
             (cond 
               ((and (Empty? fxres) (Error? (Empty-reply fxres)))
                (append!-expected saved-expected)
                (Error))
               ((Consumed? fxres)
                (Consumed-reply fxres))
               ((Empty? fxres)
                (Empty-reply fxres))))))
        (#t
         pres)))))


(define (>> p q) (>>= p 
                   (lambda (x) 
                     q)))


;;;
;;;; $$parser-compose
;;;


(define ($$parser-compose p . rest)
  (if (null? rest)
      p
    (>>= p 
         (lambda (x)
           (apply $$parser-compose rest)))))


;;;
;;;; $$seq
;;;


(define-macro $$seq
  (lambda (c . cs)
    (if (null? cs)
        `(>>= ,c (lambda (x)
                   ($$return (list x))))
      `(>>= ,c
         (lambda (x)
           (>>= ($$seq ,@cs)
             (lambda (y)
               ($$return (cons x y)))))))))


;;;
;;;; $$parser-cons
;;;


(define-macro $$parser-cons
  (lambda (p q)
    `(>>= ,p
       (lambda (x)
         (>>= ,q
           (lambda (y)
             ($$return (cons x y))))))))


;;;
;;;; <or>
;;;


(define ($$<or>2 p q)
  (lambda (in)
    (let ((pres (p in)))
      (cond
        ((equal? pres (Empty (Error)))
         (let ((saved-expected (get-expected))
               (qres (q in)))
           (if (Empty? qres)
               (begin
                 (append!-expected saved-expected)
                 qres)
             qres)))
        ((and (Empty? pres) (Ok? (Empty-reply pres)))
         (let ((saved-expected (get-expected))
               (qres (q in)))
           (if (Empty? qres)
               (begin
                 (append!-expected saved-expected)
                 pres)
             qres)))
        (#t 
         pres)))))


(define ($$<or> . args)
  (improper-fold (lambda (p acc) ($$<or>2 acc p)) (car args) (cdr args)))


;;;
;;;; $$<any>
;;;


(define ($$<any>2 p q)
  (lambda (in)
    (let ((pres (p in)))
      (if (and (Empty? pres) (Error? (Empty-reply pres)))
          (let ((saved-expected (get-expected)))
            (let ((qres (q in)))
              (if (Empty? qres)
                  (begin
                    (append!-expected saved-expected)
                    qres)
                qres)))
        pres))))


(define ($$<any> . args)
  (improper-fold (lambda (p acc) ($$<any>2 acc p)) (car args) (cdr args)))


;;;
;;;; $$option
;;;


(define ($$option x p) ($$<or> p ($$return x)))
(define ($$optionMaybe p) ($$option #f p))
(define ($$optional p) 
  ($$<or> (>> p ($$return (list)))
         ($$return (list))))


;;;
;;;; $$try
;;;


(define ($$try p)
  (lambda (in)
    (let ((byte-pos (_input-port-byte-position in)))
      (let ((pres (p in)))
        (if (and (Consumed? pres) (Error? (Consumed-reply pres)))
            (begin
              (_input-port-byte-position in byte-pos) ; backtrack
              pres)
          pres)))))

;;;
;;;; $$lookAhead
;;;


(define ($$lookAhead p)
  (lambda (in)
    (let ((byte-pos (_input-port-byte-position in))
          (pres (p in)))
      (if (and (Consumed? pres) (Ok? (Consumed-reply pres)))
          (begin
            (_input-port-byte-position in byte-pos) ; backtrack
            (set!-unexpected (thunk (_read-all in read-line)))
            pres)
        pres))))


;;;
;;;; result->str
;;;

(define (result->str res)
  (cond ((char? res) 
         (list->string (list res)))
        ((and (list? res) (andmap char? res))
         (list->string res))
        (else res)))


;;;
;;;; $$<!>
;;;


(define ($$<!> p #!key (q $$anyChar))
  (lambda (in)
    (let ((byte-pos (_input-port-byte-position in))
          (res (p in)))
      (_input-port-byte-position in byte-pos) ; always backtrack
      (if (and (Consumed? res) (Ok? (Consumed-reply res)))
          (let ((okres (Ok-parsed (Consumed-reply res)))) 
            (set!-unexpected (thunk (result->str okres)))
            (set!-expected (list (thunk (format "not: ~a" (result->str okres)))))
            (Empty (Error)))
        (q in)))))


;;;
;;;; $$notFollowedBy
;;;


(define ($$notFollowedBy p)
  (lambda (in)
    (let ((byte-pos (_input-port-byte-position in))
          (pres (p in)))
      (_input-port-byte-position in byte-pos) ; always backtrack; never consume
      (if (and (Consumed? pres) (Ok? (Consumed-reply pres)))
          (let ((res (Ok-parsed (Consumed-reply pres))))
            (set!-unexpected (thunk (result->str res)))
            (set!-expected (list (thunk (string-append "not: " (result->str res)))))
            (Empty (Error)))
        (begin
          (reset!-unexpected)
          (reset!-expected)
          (Empty (Ok (list))))))))



;;;
;;;; $$many1 
;;;


(define ($$many1 p)
  ($$parser-cons p ($$many p)))


(define $$some $$many1) ; other name for $$many1


;;;
;;;; $$many
;;;


(define ($$many p) 
  ($$<or>2
    ($$return (list))
    ($$many1 p)))


;;;
;;;; sepBy
;;;


(define ($$sepBy1 p sep) ($$parser-cons p ($$many (>> sep p))))
(define ($$sepBy p sep) ($$<or> ($$sepBy1 p sep) 
                              ($$return (list))))


;;;
;;;; $$<?>
;;;


(define ($$<?> p exp)
  (lambda (in)
    (let ((pres (p in)))
      (if (Empty? pres)
          (begin
            (set!-expected (list exp))
            pres)
        pres))))


;;;
;;;; elementary parsers
;;;



(define ($$char c)
  ($$<?> ($$satisfy (curry char=? c))
        (list->string (list c))))

(define ($$charAnyCase c)
  ($$<?> ($$satisfy (curry char-ci=? c))
        (string-append (list->string (list (char-upcase c))) " or " (list->string  (list (char-downcase c))))))

(define $$letter
  ($$<?> ($$satisfy char-alphabetic?)
        "letter"))

(define $$digit
  ($$<?> ($$satisfy char-numeric?)
        "digit"))

(define $$alphaNum 
  ($$<?> ($$satisfy (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))) 
        "letter or digit"))

(define $$hexDigit
  ($$<?> ($$<or> $$digit
               ($$oneOf "abcdef")
               ($$oneOf "ABCDEF"))
        "hexadecimal digit"))



(define $$space ($$<?> ($$satisfy char-whitespace?) "space"))
;(define $$spaces ($$<?> ($$skipMany $$space) "white space"))
(define $$anyChar ($$satisfy (lambda _ #t)))
(define $$newline ($$<?> ($$char #\newline) "new-line"))
(define $$tab ($$<?> ($$char #\tab) "tab"))


;;;
;;;; $$string
;;;


(define ($$string* str p)
  ($$chars (string->list str) p))

(define ($$chars cs p)
  (if (null? cs)
      ($$return (list))
    ($$parser-cons (p (car cs)) ($$chars (cdr cs) p))))

(define ($$string str) ;case sensitive
  ($$string* str $$char))

(define ($$stringAnyCase str) ;case insensitive
  ($$string* str $$charAnyCase))


;;;
;;;; $$eof
;;;


(define $$eof
  ($$<?>
    (lambda (in)
      (let ((c (_peek-char in)))
        (cond
          ((eof-object? c)
           (reset!-unexpected)
           (reset!-expected)
           (Empty (Ok (list))))
          (else
           (set!-unexpected "non-empty input")
           (reset!-expected)
           (Empty (Error))))))
    "end-of-file"))


;;;
;;;; $$eol
;;;


(define $$eol 
  ($$<?> ($$<or> ($$try ($$string "\n\r"))
               ($$try ($$string "\r\n"))
               ($$try ($$string "\n"))
               ($$try ($$string "\r")))
        "end-of-line"))


;;;
;;;; $$identifier
;;;


(define $$identifier 
  ($$<?> ($$many1 ($$<or> $$letter $$digit ($$char #\_))) "identifier"))


;;;
;;;; format-exp
;;;


(define (frc e) (if (procedure? e) (e) e))

(define (format-exp exp) 
  (join-strings-with-string (map frc exp) ", "))


;;;
;;;; parse
;;;


(define (apply-parser p #!optional (inp (current-input-port)))
  (reset!-unexpected)
  (reset!-expected)
  (user-state-reset!)
  (let ((res (cond ((input-port? inp)
                    (p inp))
                   ((string? inp)
                    (p (pstring-port inp)))
                   (#t 
                    (raise-user-error 'parse
                                      "input not input port, file path, or string file name")))))
    (cond
      ((or (equal? res (Empty (Error))) (equal? res (Consumed (Error))))
       (parser-error 
         (string-append 
           "unexpected: " 
           (frc (get-unexpected))
           "\n  expected: "
           (format-exp (get-expected)))))
      (#t res))))


;;;
;;;; parse-result
;;;


(define (parse-result p s)
  (let ((res (apply-parser p s)))
    (cond
      ((and (Consumed? res) (Ok? (Consumed-reply res)))
       (Ok-parsed (Consumed-reply res)))
      ((and (Empty? res) (Ok? (Empty-reply res)))
       (Ok-parsed (Empty-reply res)))
      (#t 
       (parser-error res)))))


;;;
;;;; $$choice
;;;


(define ($$choice ps) (apply $$<or> ps))


;;;
;;;; tests
;;;


#|

(parse ($$seq2 ($$oneOf "abc") ($$oneOf "def")) "ae")
(parse ($$seq3 ($$oneOf "abc") ($$oneOf "def") ($$oneOf "123")) "ae3")



(parse ($$seq ($$oneOf "abc") ($$oneOf "def") ($$oneOf "123")) "ae3")
(parse ($$seq ($$oneOf "abc")) "ae3")
(parse ($$seq ($$oneOf "abc") ($$oneOf "def")) "ae3")

(parse ($$simpler-seq ($$oneOf "abc") ($$oneOf "def") ($$oneOf "123")) "ae3")

(parse ($$parser-compose ($$oneOf "abc")) "ae3")
(parse ($$parser-compose ($$oneOf "abc") ($$oneOf "def")) "ae3")


(parse ($$<or>2 ($$oneOf "abc") ($$oneOf "def")) "eae3")
(parse ($$<or> ($$oneOf "abc") ($$oneOf "def") ($$oneOf "123")) "eae3")
(parse ($$seq ($$<or>2 ($$oneOf "abc") ($$oneOf "def")) ($$<or>2 ($$oneOf "123") ($$oneOf "456"))) "e5e3")


(parse ($$<any>2 ($$oneOf "abc") ($$oneOf "def")) "d")
(parse ($$<any> ($$oneOf "abc") ($$oneOf "def") ($$oneOf "123")) "d")


(parse ($$try ($$oneOf "abc")) "a45")

(parse ($$seq ($$lookAhead ($$oneOf "abc")) ($$oneOf "bcd")) "b45") 

(parse ($$seq ($$oneOf "abc") ($$<!> ($$oneOf "123")) ($$oneOf "abc")) "ab")

(parse ($$many $$anyChar) "toto2$$%##")

(parse ($$string "toto") "toto2")

(parse ($$parser-cons ($$oneOf "abc") ($$oneOf "abc")) "ab2")

(parse ($$many ($$oneOf "abc")) "ab2")

(parse ($$many1 ($$oneOf "abc")) "ab2")

(define $$email-prefix
  ($$many ($$<or> 
           $$letter
           $$digit
           ($$oneOf "!#$$%&'*+-/=?^_`{|}~")
           (>> ($$seq ($$char #\.) 
                     ($$notFollowedBy ($$char #\.))) 
               ($$return #\.)))))


(define $$email-domain
  ($$sepBy ($$many ($$<or> 
                   $$letter
                   $$digit
                   ($$char #\-))) 
          ($$char #\.)))


(define $$email
  (>>=
    ($$seq $$email-prefix ($$char #\@) $$email-domain)
    (lambda (seq)
      ($$return (list 
                 (list->string (first seq))
                 (second seq)
                 (map list->string (third seq)))))))



> (parse-result $$email "f.ma_gnan@catai.systems")
returns: ("f.ma_gnan" #\@ ("catai" "systems"))


> (parse-result $$email "toto..titi@me.com")
returns: *** ERROR IN parse-result, "utilities/mpc.scm"@682.14-682.25 -- parse ERROR: unexpected: .
  expected: not: ., @


|#