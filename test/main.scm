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
;; Project:
;;

(include "../loader.scm")

;; Change window size and title.
(systems-init
 "Gambit-Raylib"
 height: 480
 width: 300)

(define (close-program)
  ;; custom logic to execute before the program closes
  (close-window))

(define (initial-state)
  (list->table
   `((position . ,(make-vector2 240.0 150.0))
     (color . ,green))))

(let loop ((state (initial-state)))
  (if (window-should-close) (close-program)
      (begin
        (frame-definition
         ;; Test inputs in this list
         inputs: (list
                  (is-key-down KEY_DOWN)
                  (is-key-down KEY_UP)
                  (is-key-down KEY_LEFT)
                  (is-key-down KEY_RIGHT)
                  (is-mouse-button-down MOUSE_BUTTON_LEFT)
                  (is-mouse-button-down MOUSE_BUTTON_RIGHT))
         ;; Choose which state to use this frame
         state: state
         update: (lambda (state input)
                   ;; constant state manipulations pre-user-input
                   (let* ((position (table-ref state 'position))
                          (x (vector2-x position))
                          (y (vector2-y position)))
                     (case input
                       ;;state manipulations based on input
                       ((0) ;; Idle Case
                        (void))
                       ((1)
                        (table-set! state 'color (pick-random-element (list green yellow white blue red))))
                       ((2)
                        (table-set! state 'position (get-mouse-position)))
                       ((4 52)
                        (table-set! state 'position (vector2-x-set! position (+ x 2))))
                       ((8 56)
                        (table-set! state 'position (vector2-x-set! position (- x 2))))
                       ((16 28)
                        (table-set! state 'position (vector2-y-set! position (- y 2))))
                       ((20)
                        (table-set! state 'position (vector2-x-set! position (+ x 2)))
                        (table-set! state 'position (vector2-y-set! position (- y 2))))
                       ((24)
                        (table-set! state 'position (vector2-x-set! position (- x 2)))
                        (table-set! state 'position (vector2-y-set! position (- y 2))))
                       ((32 44)
                        (table-set! state 'position (vector2-y-set! position (+ y 2))))
                       ((36)
                        (table-set! state 'position (vector2-x-set! position (+ x 2)))
                        (table-set! state 'position (vector2-y-set! position (+ y 2))))
                       ((40)
                        (table-set! state 'position (vector2-x-set! position (- x 2)))
                        (table-set! state 'position (vector2-y-set! position (+ y 2))))
                       (else
                        (println "unknown case: " input)
                        (void)))
                     (lambda () (draw-circle-v position 20.0 (table-ref state 'color))))
                   ;; constant state manipulations post-user-input
                   ))
        (loop (let ((state (<! render-channel)))
                (if state state (<! render-channel)))))))
