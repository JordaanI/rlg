;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copyright LICEF 2005
;; TELUQ
;; 
;; Project: LORNET
;; 
;; File: SXML-Accessors.scm
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
;;;; match-child-sequence
;;;  Digs into the SXML stucture and tries to match the first child of each level with the given tagname sequence.
;;; 


(define (match-child-sequence xml-node seq)
  (if (null? seq)
      xml-node
      (and (equal? (get-tagname-no-ns xml-node) (car seq))
           (match-child-sequence (find-child-tagged xml-node (car seq)) (cdr seq)))))

;;;
;;;; get-first-child
;;;  Retrieves the first child of a given XML node 
;;; 


(define (get-first-child xml-node)
  (let ((children (get-children xml-node)))
    (and (not (null? children)) (car children))))

;;;
;;;; set-children!
;;;

(define (set-children! xml-node new-children)
  (if (and (list? xml-node)
           (> (length xml-node) 1))
      (set-cdr! (if (node-has-attributes? xml-node) (cdr xml-node) xml-node) new-children)))


;;;
;;;; get-children
;;;


(define (get-children xml-node)
  (or
   (and (list? xml-node)
        (> (length xml-node) 1)
        (or
         (and (node-has-attributes? xml-node)
              (cddr xml-node))
         (cdr xml-node)))
   '()))


(define (node-has-attributes? xml-node)
  (and (list? xml-node)
       (> (length xml-node) 1)
       (pair? (second xml-node))
       (equal? (car (second xml-node)) '@)))


(define (children-start-index xml-node)
  (or 
   (and (node-has-attributes? xml-node)
        2)
   1))


;;;
;;;; find-child-tagged
;;;


(define (find-child-tagged $node tagname)
  (let ((result (find-if (lambda ($child)
                           (equal? (car $child) tagname))
                         (get-children $node))))
    result))


;;;
;;;; collect-children-tagged
;;;


(define (collect-children-tagged $node tagname)
  (collect-if (lambda ($child)
                (equal? (get-tagname-no-ns $child) tagname))
              (get-children $node)))
                            

;;;
;;;; get-text-content
;;;


(define (get-text-content xml-node)
  (let ((children (get-children xml-node))
        (result ""))
    (for-each (lambda (child)
                (if (string? child)
                    (set! result (string-append result child))))
              children)
    result))
    
             
;;;
;;;; get-tagname
;;;  Strips the namespace from a given tag identifier portion 
;;; 


(define (get-tagname xml-node)
  (car xml-node))


(define (get-tagname-no-ns xml-node)
  ;(logger " get-tagname-no-ns")
  ;(logger xml-node)
  (or (and (list? xml-node) (car xml-node);(get-tagname-from-namespace (car xml-node))
        )
      (and (string? xml-node) ':cdata)))


(define (get-tagname-from-namespace name-with-namespace)
  (string->symbol (strip-namespace (symbol->string name-with-namespace))))


(define (get-attribute-name-no-ns attr-name)
  (get-tagname-from-namespace attr-name))


(define (strip-namespace string)
  (last (split-string string #\:)))

  
;;;
;;;; get-node-attributes
;;; 


(define (get-node-attributes xml-node)
  (or 
   (and (list? xml-node) 
        (> (length xml-node) 1) 
        (let ((elem2 (second xml-node)))
          (and (list? elem2)
               (equal? (car elem2) '@)
               (map fix-attributes-names-no-namespace (cdr elem2)))))
   '())) ; todo: utiliser get-node-attributes-with-ns


(define (get-node-attributes-with-ns xml-node)
  (or 
   (and (list? xml-node) 
        (> (length xml-node) 1) 
        (let ((elem2 (second xml-node)))
          (and (list? elem2)
               (equal? (car elem2) '@)
               (cdr elem2))))
   '()))


(define (fix-attributes-names-no-namespace attr-pair)
  (list 
   (get-attribute-name-no-ns (car attr-pair))
   (second attr-pair)))


;;;
;;;; get-attribute-value
;;;


(define (get-attribute-value xml-node attr-name)
  (let ((try-find-attr (assoc attr-name (get-node-attributes xml-node))))
    (and try-find-attr (second try-find-attr))))


;;;
;;;; get-subnode-by-address 
;;;


(define (get-subnode-by-address xml-node node-addr)
  (or 
   (and (null? (get-children xml-node)) xml-node)
   (and 
    (null? node-addr)
    xml-node)
   (and (> (length xml-node) (+ 1 (car node-addr)))
        (get-subnode-by-address (list-ref (get-children xml-node) (car node-addr)) (cdr node-addr)))))


;;;
;;;; Tests
;;; 

