;;;
;;;; murmurhash
;;;  quick hashing function

(define (murmurhash key seed)
  (let* ((r (modulo (string-length key) 4))
        (bytes (- (string-length key) r))
        (h1 seed)
        (c1 3432918353)
        (c2 461845907))
    ; loop for each 4 chars groups
    (for-each 
      (lambda (j)
        (let* ((i (* j 4))
              (k1 (fxior
                    (fxand (char->integer (string-ref key i)) 256)
                    (fxwraparithmetic-shift-left (char->integer (string-ref key (+ i 1))) 8)
                    (fxwraparithmetic-shift-left (char->integer (string-ref key (+ i 2))) 16)
                    (fxwraparithmetic-shift-left (char->integer (string-ref key (+ i 3))) 32)))
              (h1b 0))
          (set! k1 (fxand
                     (fx+ 
                       (fx* (fxand k1 65535) c1)
                       (fxwraparithmetic-shift-left (fxand (fx* (fxwraplogical-shift-right k1 16) c1) 65535) 16))
                     4294967295))
          (set! k1 (fxior 
                     (fxwraparithmetic-shift-left k1 15) 
                     (fxwraplogical-shift-right k1 17)))
          (set! k1 (fxand
                     (fx+
                       (fx* (fxand k1 65535) c1)
                       (fxwraparithmetic-shift-left (fxand (fx* (fxwraplogical-shift-right k1 16) c2) 65535) 16))
                     4294967295))
          (set! h1 (fxxor h1 k1))
          (set! h1 (fxior (fxwraparithmetic-shift-left h1 13)
                          (fxwraplogical-shift-right h1 19)))
          (set! h1b (fxand 
                      (fxwraparithmetic-shift-left
                        (fx+
                          (fx* (fxand h1 65535) 5)
                          (fxand (fx* (fxwraplogical-shift-right h1 16) 5) 65535))
                        16)
                      4294967295))
          (set! h1 (fx+
                     (fxand h1b 65535) 
                     27492
                     (fxwraparithmetic-shift-left 
                       (fxand (fx+ (fxwraplogical-shift-right h1b 16) 58964) 65535)
                       16)))))
      (iota (/ bytes 4)))
    (let ((k1 0))
    
      (case r
        ((3)
         (set! k1 (fxxor k1 (fxwraparithmetic-shift-left (char->integer (string-ref key (+ bytes 2))) 16))))
        ((2)
         (set! k1 (fxxor k1 (fxwraparithmetic-shift-left (char->integer (string-ref key (+ bytes 1))) 8))))
        ((1)
         (set! k1 (fxxor k1 (fxwraparithmetic-shift-left (char->integer (string-ref key bytes)) 8)))))
      
      ; ...
      
      
      (set! h1 (fxxor h1 (string-length key)))
      (set! h1 (fxxor h1 (fxwraplogical-shift-right h1 16)))
      h1)))
                     
         