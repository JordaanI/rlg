;;;
;;;; finite state transducers
;;;

;;
;; A finite state transducer is a two tapes finite state machine.
;; It contains a control-state and an extended state.
;; Transitions form a directed graph over the control-state nodes.
;; A fst can have a parent and children fst so that we can organize them in a hierarchy of states.
;; 
;; A transition is composed of:
;;
;;   - event trigger: name of the event that can tigger the transition
;;   - from: the start control-state from which the transition can happen
;;   - to: the end control-state to which the transition could lead to
;;   - guard: a guard test function that must return true for the transition to happen type: (lambda (fst event) ...)
;;   - action: the action to perform if transition happens: (lambda (fst event) ...)
;;
;; Sending events to an fst 
;; ------------------------
;;
;; (^ <fst> (>event "valueChanged" 
;;            `(("value" . "test"))))
;;
;; sending to the parent of the <fst>: 
;;
;; (^^ <fst> (>event "fieldChanged"))
;;
;;


;;;
;;;; fst
;;;


(define-type fst id: CFC90B7E-D2B9-40A6-BC89-421375C9BAC5 name id init state control-state transitions handler parent children)


(define (create-fst init transitions #!key (initial-state "initial") (message-key "name") (name ""))
  (let* ((fst_id (generate-guid))
         (fst (make-fst name fst_id init #f #f transitions #f #f #f)))
    (compute-handler fst message-key)
    (fst-initialize fst initial-state)
    fst))

(define (trivial-init)
  (lambda () (make-table)))

(define (trivial-fst)
  (create-fst (lambda () (make-table)) (list)))

(define (in-control-state? fst control-state)
  (equal? (fst-control-state fst) control-state))

;;;
;;;; transition
;;;


(define-type transition id: BB2A9F8D-F2EE-48CA-86FB-BC555CA8DABB event-name from to guard action)


(define (create-transition #!key (event "") (from "initial") (to "initial") (guard (lambda (fst evt) #t)) (action (lambda (fst event) #t)))
  (make-transition event from to guard action))


;;;
;;;; compute-handler 
;;;

(define display-events? #f)

(define (compute-handler fst key)
  (fst-handler-set! fst (let ((FST fst))
                          (lambda (event)
                            (let ((event-name (table-ref event key)))
                              (let ((matching-transition (find-if (lambda (t)
                                                                    ((transition-guard t) FST event))
                                                                  (collect-if (lambda (tt)
                                                                                (and (equal? (transition-event-name tt) event-name)
                                                                                     (or (equal? (transition-from tt) (fst-control-state FST))
                                                                                         (equal? (transition-from tt) "*"))))
                                                                              (fst-transitions FST)))))
                                (if matching-transition
                                    (begin
                                      (if display-events?
                                          (dart-display 
                                            (fst-id FST) 
                                            (table->list event) 
                                            (clearer-transition-locations fst (transition-from matching-transition)) 
                                            (clearer-transition-locations fst (transition-to matching-transition))))                            
                                      (if (not (equal? (transition-to matching-transition) "*"))
                                          (fst-control-state-set! FST (transition-to matching-transition)))
                                      ((transition-action matching-transition) FST event))
                                  (dart-display (string-append "DEBUG: Couldn't find matching transition for " (>string event-name) " in " (fst-id FST))))))))))


;;;
;;;; Debug
;;;

(define (clearer-transition-locations fst dest)
  (if (equal? dest "*") (string-append dest " (" (>string (fst-control-state fst)) ")") 
    dest))

;;;
;;;; fst-init
;;;


(define (fst-initialize fst initial-state)
  (fst-state-set! fst ((fst-init fst)))
  (fst-control-state-set! fst initial-state))

;;;
;;;; Child get
;;;

(define (fst-child-get fst name)
  (find-if (lambda (child)
             (equal? name (fst-name child))) 
           (fst-children fst)))

;;;
;;;; fst-handle-event
;;;


(define (fst-handle-event fst event)
  ((fst-handler fst) event))


(define (fst-id-handle-event fst_id event)
  (let ((found_fst (resolve-fst-by-id fst_id)))
    (and found_fst 
         (fst-handle-event found_fst event))))

(define (fst-child-handle-event fst name event)
  (fst-handle-event (fst-child-get fst name) event))


;;; Short names

(define (^ fst event)
  (fst-handle-event fst event))


(define (^^ fst event)
  (fst-handle-event (fst-parent fst) event))

(define (^#1 fst event)
  (fst-handle-event (fst-parent fst) event))

(define (^#2 fst event)
  (fst-handle-event (fst-parent (fst-parent fst)) event))

(define (^* fst name event)
  (fst-child-handle-event fst name event))


;;;
;;;; event-get
;;;


(define (event-get event arg-name)
  (table-ref event arg-name #f))

;;;
;;;; state-get
;;;


(define (get-state-by-fst-id fst-id)
  (table->list (fst-state (resolve-fst-by-id fst-id))))


(define (state-get fst address)
  (if (list? address)
      (list-reduce (lambda (acc v)
                     (if (table? acc)
                         (table-ref acc v)
                       v)) 
                   (fst-state fst)
                   address)
    (table-ref (fst-state fst) address #f)))


;;;
;;;; state-set
;;;


(define (state-set fst address value)
  (if (list? address) 
      (let loop ((state (fst-state fst))
                 (address address))
           (if (= (length address) 1) 
               (table-set! state (car address) value)
             (loop (table-ref state (car address)) (cdr address))))
    (table-set! (fst-state fst) address value)))

;;;
;;;; State manipulations
;;;


(define @r state-get)
(define @w state-set)


;;;
;;;; fst-union
;;; Create fst by doing union of a set of fst's 
;;;


(define (fst-union #!key (initial-state "initial") (message-key "name") #!rest rest)
  (let ((fst (create-fst (lambda ()
                           (reduce 
                             (lambda (f a)
                               (table-merge! a ((fst-init f))))
                             rest
                             (make-table)))
                         (apply append
                                (map fst-transitions rest))
                         message-key: message-key
                         initial-state: initial-state
                         )))
    ;;(for-each fst-cleanup rest)
    fst))

;;;
;;;; fst registry
;;;

#|

(define FST-Registry (make-table))


(define (register-fst fst_id fst)
  (dart-display (string-append "Registering fst: " fst_id))
  (table-set! FST-Registry fst_id fst))

(define (fst-register-all-children fst)
  (let ((children (fst-children fst)))
    (if (not (null? children))        
        (and 
          (for-each fst-register-all-children children)
          (for-each (lambda (child)
                      (register-fst (fst-id child) child)) 
                    children)))))
            

(define (resolve-fst-by-id fst_id)
  (table-ref FST-Registry fst_id #f))


(define (fst-cleanup fst)
  (dart-display (string-append  "Unregistering fst: " (fst-id fst)))
  (table-set! FST-Registry (fst-id fst)))

(define (fst-cleanup-all-children fst)
  (let ((children (fst-children fst)))
    (if (not (null? children))        
        (and 
          (for-each fst-cleanup-all-children children)
          (for-each fst-cleanup children)))))

|#

;;;
;;;; event creation
;;;


(define (>event name #!optional params)
  (let ((base-table (list->table `(("name" . ,name)))))
    (if params
        (table-merge! base-table
                      (if params (list->table params)))
      base-table)))