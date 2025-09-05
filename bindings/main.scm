;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                        ;;;
;;;     __  .______     ______   .__   __.    .______    __    _______.                    ;;;
;;;    |  | |   _  \   /  __  \  |  \ |  |    |   _  \  |  |  /  _____|        _____       ;;;
;;;    |  | |  |_)  | |  |  |  | |   \|  |    |  |_)  | |  | |  |  __      ^..^     \9     ;;;
;;;    |  | |      /  |  |  |  | |  . `  |    |   ___/  |  | |  | |_ |     (oo)_____/      ;;;
;;;    |  | |  |\  \  |  `--'  | |  |\   |    |  |      |  | |  |__| |        WW  WW       ;;;
;;;    |__| | _| `._|  \______/  |__| \__|    | _|      |__|  \______|                     ;;;
;;;                                                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;; Author: Ivan Jordaan
;; Date: 2025-08-29
;; email: ivan@axoinvent.com
;; Project: rlg-bindings
;;

(c-declare "#include <raylib.h>")


(define-macro (define-c-lambda name types return)

  (define (gambitify-string str)
    (let ((strl (string->list str)))
      (list->string
       (cons (char-downcase (car strl))
             (let loop ((strl (cdr strl)) (in-capital? #f))
               (if (null? strl) (list)
                   (let ((fc (car strl)))
                     (cond
                      ((and in-capital? (char-upper-case? fc)) (cons (char-downcase fc) (loop (cdr strl) #t)))
                      ((char-upper-case? fc) (cons #\- (cons (char-downcase fc) (loop (cdr strl) #t))))
                      (#t (cons fc (loop (cdr strl) #f)))))))))))

  `(define ,(string->symbol (gambitify-string name))
     (c-lambda ,types ,return ,name)))

(define-macro (define-const symbol)
  (let* ((str (##symbol->string symbol))
         (ref (##string-append "___return (" str ");")))
    `(define ,symbol
       ((c-lambda () int ,ref)))))
