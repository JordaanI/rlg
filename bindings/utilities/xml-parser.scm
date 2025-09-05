;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright LICEF 2005
;; TELUQ
;; 
;; Project: LORNET
;; 
;; File: SXML-Parser.scm
;; Authors: Perry Lake
;; 
;;
;;

;;; This file is part of GIT System.
;;;
;;;    GIT System is free software: you can redistribute it and/or modify
;;;    it under the terms of the GNU Lesser General Public License as published by
;;;    the Free Software Foundation, either version 3 of the License, or
;;;    (at your option) any later version.
;;;
;;;    GIT System is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU Lesser General Public License for more details.
;;;
;;;    You should have received a copy of the GNU Lesser General Public License
;;;    along with GIT System.  If not, see <http://www.gnu.org/licenses/>.


;;;
;;;; main entry
;;;



(define (parse-xml-stream2 stream)
  (letrec ((proc (lambda ()
                   (set! Chars-Buffer (list))
                   (if (find-start-markup stream)
                       (cons (cond ((read-PI? stream)      (read-PI stream))
                                   ((read-comment? stream) (read-comment stream))
                                   ((read-PE? stream)      (read-PE stream))
                                   (else                   (read-markup-toplevel stream)))
                             (proc))
                       (list)))))
    (cons '*TOP* (proc))))


;;;
;;;; read-PI
;;;


(define (read-PI? stream)
  (read-string? stream "?"))


(define (read-PI stream)
  (letrec ((tagname (read-tagname stream))
           (dum     (skip-spaces stream))
           (proc    (lambda ()
                      (if (read-string? stream "?>")
                          (list)
                          (cons (do-read-char stream) (proc)))))
           (result (list)))
    (set! result (proc))
    (list '*PI* tagname (list->string result))))

;;;
;;;; read-HOOK []
;;;


(define (read-HOOK? stream)
  (read-string? stream "["))


(define (read-HOOK stream)
  (letrec ((tagname (read-tagname stream))
           (dum     (skip-spaces stream))
           (proc    (lambda ()
                      (if (read-string? stream "]")
                          (list)
                          (cons (do-read-char stream) (proc)))))
           (result (list)))
    (set! result (proc))
    (list '*HOOK* tagname (list->string result))))

;;;
;;;; read-PE !
;;;


(define (read-PE? stream)
  (read-string? stream "!"))


(define (read-PE stream)
  (letrec ((tagname (read-tagname stream))
           (dum     (skip-spaces stream))
           
           (proc    (lambda ()
                      (if (read-HOOK? stream)
                          (read-HOOK stream)
                        (if (read-string? stream ">")
                            (list)
                            (cons (do-read-char stream) (proc))))))
           ;(proc    (lambda ()
           ;           (if tagname (read-HOOK stream))))
           (result (list)))
    (set! result (proc))
    (list '*PE* tagname)))



;;;
;;;; read-comment
;;;


(define (remove-comment l)
  (collect-if (lambda (q) (not (equal? '*COMMENT* (car q)))) l))

(define (read-comment? stream)
  (read-string? stream "!--"))


(define (read-comment stream)
  (letrec ((proc   (lambda ()
                     (if (read-string? stream "-->")
                         (list)
                         (cons (do-read-char stream) (proc)))))
           (result (list)))
    (set! result (proc))
    (list '*COMMENT* (list->string result))))


;;;
;;;; read-markup-toplevel
;;;


(define (read-markup-toplevel stream)
  (let* ((res         (read-markup stream))
         (markup      (first res))
         (close?      (second res))
         (auto-close? (third res)))
    (if (null? markup)
        (list)
        (if auto-close?
            markup
            (if close?
                (error (string-append "Closing unopened tagname: " (symbol->string (car markup))))
                (append markup (read-children stream (car markup))))))))


(define (read-children stream parent-tagname)
  (letrec ((proc (lambda ()
                   (let* ((cdata       (read-cdata stream))
                          (res         (read-markup stream))
                          (markup      (first res))
                          (close?      (second res))
                          (auto-close? (third res)))
                     (if (null? markup)
                         cdata
                         (if auto-close?
                             (append cdata (cons markup (proc)))
                             (if close?
                                 (if (equal? parent-tagname (car markup))
                                     cdata
                                     (error (string-append "Bad closing tagname: " (symbol->string parent-tagname) " " (symbol->string (car markup)))))
                                 (cons (append cdata markup (read-children stream (car markup))) (proc)))))))))
    (proc)))
        

(define (read-markup stream)
  (if (read-comment? stream)
      (list (read-comment stream) #f #t)
      (let ((close?      (read-slash? stream))
            (tagname     (read-tagname stream))
            (attributes  (read-attributes stream))
            (auto-close? (read-slash? stream)))
        (do-read-char stream) ; skip ">"
        (list (if (null? attributes)
                  (list tagname)
                  (list tagname attributes))
              close?
              auto-close?))))


(define (read-tagname stream)
  (letrec ((proc (lambda ()
                   (let ((char (do-peek-char stream)))
                     (if (member char '(#!eof #\space #\tab #\return #\newline #\/ #\>))
                         (list)
                         (cons (do-read-char stream) (proc)))))))
    (string->symbol (list->string (proc)))))


(define (read-attributes stream)
  (letrec ((proc (lambda ()
                   (if (find-start-attribute stream)
                       (cons (read-attribute stream) (proc))
                       (list))))
           (res (list)))
    (set! res (proc))
    (if (null? res)
        (list)
        (cons '@ res))))


(define (read-attribute stream)
  (letrec ((read-name  (lambda () (let ((char (do-peek-char stream)))
                                    (if (member char '(#!eof #\= #\space #\tab #\return #\newline))
                                        (list)
                                        (cons (do-read-char stream) (read-name))))))
           (read-value (lambda () (let ((char (do-peek-char stream)))
                                    (if (member char '(#!eof #\"))
                                        (list)
                                        (cons (do-read-char stream) (read-value))))))
           (name  (list))
           (value (list)))
    (set! name (read-name))
    (if (null? name)
        (list)
        (begin
          (find-start-attribute-value stream)
          (do-read-char stream) ; skip dquote
          (set! value (read-value))
          (do-read-char stream) ; skip dquote
          (list (string->symbol (list->string name))
                (decode-entities (list->string value)))))))


(define (find-start-attribute-value stream)
  (letrec ((mode 'search-equal)
           (scan (lambda ()
                   (if (not (is-eof? stream))
                       (let ((char (do-peek-char stream)))
                         (case mode
                           ('search-equal
                             (cond ((member char '(#\space #\tab #\return #\newline))
                                    (do-read-char stream)
                                    (scan))
                                   ((equal? char #\=)
                                    (do-read-char stream)
                                    (set! mode 'found-equal)
                                    (scan))
                                   (else
                                    (error (string-append "Illegal attribute definition!")))))
                           ('found-equal
                             (cond ((member char '(#\space #\tab #\return #\newline))
                                    (do-read-char stream)
                                    (scan))
                                   (else
                                    'done)))))))))
    (scan)))


(define (find-start-attribute stream)
  (letrec ((found? #f)
           (find   (lambda ()
                     (if (not (is-eof? stream))
                         (let ((char (do-peek-char stream)))
                           (if (member char '(#\space #\tab #\return #\newline))
                               (begin
                                 (do-read-char stream) ; skip space
                                 (find))
                               (if (not (member char '(#\> #\/)))
                                   (set! found? #t))))))))
    (find)
    found?))


(define (read-cdata stream)
  (letrec ((proc (lambda ()
                   (let ((char (do-peek-char stream)))
                     (cond ((equal? char #!eof) (list))
                           ((equal? char #\<)   (do-read-char stream) (list))
                           (else                (cons (do-read-char stream) (proc)))))))
           (chars (list)))
    (set! chars (proc))
    (if (find-if (lambda (char) (not (member char '(#\space #\tab #\return #\newline)))) chars)
        (list (list->string chars))
        (list))))


(define (find-start-markup stream)
  (letrec ((found? #f)
           (find (lambda ()
                   (if (not (is-eof? stream))
                       (let ((char (do-read-char stream)))
                         (if (equal? char #\<)
                             (set! found? #t)
                             (find)))))))
    (find)
    found?))


;;;
;;;; Utilities
;;;


(define (read-slash? stream)
  (read-string? stream "/"))


(define (skip-spaces stream)
  (let ((char (do-peek-char stream)))
    (if (and (not (equal? #!eof char))
             (equal? char #\space))
        (begin
          (do-read-char stream) ; skip space
          (skip-spaces stream)))))


(define (is-eof? stream)
  (equal? #!eof (do-peek-char stream)))


(define Chars-Buffer (list))

(define (do-peek-char stream)
  (if (null? Chars-Buffer)
      (peek-char stream)
      (car Chars-Buffer)))


(define (do-read-char stream)
  (if (null? Chars-Buffer)
      (read-char stream)
      (let ((res (car Chars-Buffer)))
        (set! Chars-Buffer (cdr Chars-Buffer))
        res)))

(define (read-string? stream string)
  (let ((len (string-length string)))
    (if (< (length Chars-Buffer) len)
        (repeat (- len (length Chars-Buffer)) (lambda () (fetch-char stream))))
    (if (equal? (string->list string) (sublist Chars-Buffer 0 len))
        (begin
          (set! Chars-Buffer (sublist Chars-Buffer len (length Chars-Buffer)))
          #t)
        #f)))


(define (fetch-char stream)
  (let ((char (read-char stream)))
    (if (not (equal? char #!eof))
        (set! Chars-Buffer (append Chars-Buffer (list char))))))

(define (repeat n proc)
  (if (> n 0)
      (begin
        (proc)
        (repeat (- n 1) proc))))

;;;
;;;; parse-xml-file
;;;

(define (parse-xml-file file #!key (encoding 'UTF-8))
  (call-with-input-file (list char-encoding: encoding 
                          path: file)
    (lambda (stream)
      (parse-xml-stream2 stream))))


;;;
;;;; parse-xml-string
;;;


(define (parse-xml-string  s)
  (call-with-input-string s
    (lambda (stream)
      (parse-xml-stream2 stream))))

;;;
;;;; tests
;;;


(define (tst-decode)
  (__pp (decode-entities "abc &lt;qqq&gt;"))
  (__pp (decode-entities "abc &amp;amp;")))

(define (test-parse-xml-file2)
  (pp (parse-xml-file "/Users/magnan/Devel/catai/axo/models/registration.gmot"))
  (parse-xml-file "C:/Telos/local/resources/TELOS086-650668B1-AD30-4F9E-AD3A-065E83FE8A2F/TELOS086-247134C3-FE43-48E0-9644-91A3796F0FA0.xml"))

(define (test-parse-xml-string2)
  (parse-xml-string "<html><center att=\"val\">A2</center></html>"))


