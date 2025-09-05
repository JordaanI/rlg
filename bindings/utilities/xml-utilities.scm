;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright LICEF 2005
;; TELUQ
;; 
;; Project: LORNET
;; 
;; File: XML-Utilities.scm
;; Authors: Francois Magnan
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
;;;; globals
;;;


(define Encode-Entities? (string))
(define Fool-HTML-Entities (string))
(set! Encode-Entities? #t)
(set! Fool-HTML-Entities #f)


;;;
;;;; XML-FO functions
;;;


(define (start-tag tagname attributes stream)
  (let ((ctag (if Output-With-Namespaces (compute-output-tagname (list tagname)) (get-tagname-from-namespace tagname))))
    (display (string-append "<" (symbol->string ctag) (format-attributes-string attributes) ">") stream)))


(define (auto-closed-tag tagname attributes stream)
  (start-tag tagname attributes stream)
  (end-tag tagname stream)
  
  ;(let ((ctag (if Output-With-Namespaces tagname (get-tagname-from-namespace tagname))))
  ;  (display (string-append "<" (symbol->string ctag) (format-attributes-string attributes) "/>") stream))
  )


(define (end-tag tagname stream)
  (let ((ctag (if Output-With-Namespaces (compute-output-tagname (list tagname)) (get-tagname-from-namespace tagname))))
    (display (string-append "</" (symbol->string ctag) ">") stream)))


(define (format-attributes-string attributes)
  (let ((result ""))
    (for-each (lambda (attr)
                (if (second attr)
                    (set! result (string-append result " " (symbol->string (compute-output-attribute-name (first attr))) "=\"" (format-value (second attr)) "\""))))
              attributes)
    result))


(define (format-value v)
  (cond
    ((string? v)
     v)
    ((symbol? v)
     (symbol->string v))
    ((number? v)
     (let ((sv (if (and (not (integer? v)) (rational? v))
                   (number->string (exact->inexact v))
                 (number->string v)))) ; Dangerous global patch was: (number->string (inexact->exact (round v)))
       
       (cond
         ((string-end-with? "." sv)
          (string-append sv "0"))
         ((string-starts-with?  sv ".")
          (string-append  "0" sv))
         (#t
           sv))))))



(define (compute-output-attribute-name pre-attr-name)
  (if Output-With-Namespaces
      (factorize-known-namespaces pre-attr-name)
      (get-attribute-name-no-ns pre-attr-name)))


(define (output-cdata cdata stream)
  (cond
   ((string? cdata)
    (cond 
     (Replace-HTML-Entities?
        (display (decode-entities cdata) stream))
     (Fool-HTML-Entities
      (display (fool-entities cdata) stream))
     (#t
      (display cdata stream))))
   ((number? cdata)
    (display (number->string cdata) stream))
   (#t
    (abort "Invalid cdata submitted"))))


(define (filter-amp cdata)
  (string-replace-all cdata " & " " &amp; "))


(define (fool-entities cdata)
  (string-replace-all cdata "&" "%%"))


(define (unfool-entities cdata)
  (string-replace-all cdata "%%" "&"))


;;;
;;;; output-sxml-expression
;;;


(define $Pretty-Print? #t)
(define Output-With-Namespaces #t)
(define Replace-HTML-Entities? #f)
(define Known-Namespaces (list))

(define $pp-level 0)
(define $parent-type ':children)


(define (reset-$pp)
  (set! $pp-level 0)
  (set! $parent-type ':children))


(define (output-sxml-expression sxmlexp stream)
  (cond
   ((boolean?  sxmlexp)
    #t)
   ((string? sxmlexp)
    (output-cdata sxmlexp stream))
   ((pair? sxmlexp)
    (if $Pretty-Print?
        (pretty-print sxmlexp stream)
      (let* ((tagname  (compute-output-tagname sxmlexp))
             (attr     (if Output-With-Namespaces (get-node-attributes-with-ns sxmlexp) (get-node-attributes sxmlexp)))
             (children (get-children sxmlexp))
             (children-conf (get-children-configuration children)))
        (if (equal? children-conf ':empty)
            (auto-closed-tag tagname attr stream)
          (begin
            (start-tag tagname attr stream)
            (special-map output-sxml-expression children stream)
            (end-tag tagname stream))))))
   (#t
    (abort "Invalid sxml expression"))))


(define (compute-output-tagname sxmlexp)
  (let ((fulltagname  (if Output-With-Namespaces (get-tagname sxmlexp) (get-tagname-no-ns sxmlexp))))
    (factorize-known-namespaces fulltagname)))


(define (factorize-known-namespaces fulltagname)
  (let* ((sft (symbol->string fulltagname))
         (res sft))
    (for-each (lambda (ns-info)
                (if (string-starts-with? sft (second ns-info))
                    (begin
                      (if (equal? (first ns-info) "")
                          (set! res (substring sft (+ 1 (string-length (second ns-info))) (string-length sft)))
                          (set! res (string-append (first ns-info) (substring sft (string-length (second ns-info)) (string-length sft))))))))
              Known-Namespaces)
    (string->symbol res)))


(define (pretty-print sxmlexp stream)
  (let* ((tagname  (compute-output-tagname sxmlexp))
         (attr     (if Output-With-Namespaces (get-node-attributes-with-ns sxmlexp) (get-node-attributes sxmlexp)))
         (children (get-children sxmlexp))
         (children-conf (get-children-configuration children))
         (old-parent-type $parent-type))
    (if (equal? children-conf ':empty)
        (begin
          (if (equal? $parent-type ':children)
              (insert-indent-level-string children stream))
          (auto-closed-tag tagname attr stream)
          (if (equal? $parent-type ':children)
              (insert-pp-return-string children stream)))
        (begin
          (if (equal? $parent-type ':children)
              (begin
                (insert-indent-level-string children stream)))
          (start-tag tagname attr stream)
          (if (equal? children-conf ':children)
              (insert-pp-return-string children stream))
          (set! $pp-level (+ $pp-level 1))
          (set! $parent-type children-conf)
          (special-map output-sxml-expression children stream)
          (set! $parent-type old-parent-type)
          (set! $pp-level (- $pp-level 1))
          (if (equal? children-conf ':children)
              (begin
                ;(insert-pp-return-string children stream)
                (insert-indent-level-string children stream)))
          (end-tag tagname stream)
          (if (equal? $parent-type ':children)
              (insert-pp-return-string children stream))
          ))))
  
  
(define (insert-indent-level-string children stream)
  (repeat $pp-level
          (lambda ()
            (display "   " stream))))



(define (insert-pp-return-string children stream)
  (display "\n" stream))
                


(define (get-children-configuration children)
  (cond
   ((null? children)
    ':empty)
   ((andmap pair? children)
    ':children)
   ((andmap string? children)
    ':text)
   (#t
    ':mixed)))


;;;
;;;; encode entities
;;;


(define (encode-entities string)
  (if (equal? "" string)
      string
      (let* ((nextchar           (substring string 0 1))
             (found-entity-assoc (assoc nextchar Entity-Assoc-List))
             (found-entity       (and found-entity-assoc (second found-entity-assoc))))
        (if found-entity
            (string-append found-entity (encode-entities (substring string 1 (string-length string))))
            (string-append nextchar (encode-entities (substring string 1 (string-length string))))))))

(define (encode-entities-basic string)
  (if (equal? "" string)
      string
      (let* ((nextchar           (substring string 0 1))
             (found-entity-assoc (if (equal? Platform-Type 'WinXP) (assoc nextchar Entity-Assoc-List-PC-Basic) (assoc nextchar Entity-Assoc-List-Basic)))
             (found-entity       (and found-entity-assoc (second found-entity-assoc))))
        (if found-entity
            (string-append found-entity (encode-entities-basic (substring string 1 (string-length string))))
            (string-append nextchar (encode-entities-basic (substring string 1 (string-length string))))))))


(define (decode-entities string)
  (decode-entities-rec string ""))


(define (decode-entities-rec string astring)
  (if (equal? string "")
      astring
      (if (equal? (substring string 0 1) "&")
          (let* ((eei (string-position ";" string))
                 (entity (substring string 0 (+ 1 eei)))
                 (entity-found (find-if (lambda (eai)
                                              (equal? (second eai) entity))
                                            Entity-Assoc-List))
                 (replacement (if entity-found (first entity-found) entity)))
            (decode-entities-rec (substring string (+ 1 eei) (string-length string)) (string-append astring replacement)))
          (decode-entities-rec (substring string 1 (string-length string)) (string-append astring (substring string 0 1))))))
          

(define Entity-Assoc-List
  '(
    ; basic standard 
    ("<"  "&lt;")
    (">"  "&gt;")
    ("&"  "&amp;")
    ("\"" "&quot;")
    ;("'"  "\\'")
    
    ))


(define Entity-Assoc-List-PC Entity-Assoc-List)
(define Entity-Assoc-List-Basic Entity-Assoc-List)
(define Entity-Assoc-List-PC-Basic Entity-Assoc-List)


;;;
;;;; sxml-to-xml
;;;


(define (sxml-to-xml sxml-exp)
  (with-output-to-string ""
                         (lambda ()
                           (let ((old-$Pretty-Print? $Pretty-Print?))
                             (set! $Pretty-Print? #f)
                             (if (equal? (first sxml-exp) 'html)
                                 (display "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"  \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"))
                             (output-sxml-expression sxml-exp (current-output-port))
                             (set! $Pretty-Print? old-$Pretty-Print?)))))
                         
