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


;;;; Keys
(define-const KEY_UP)
(define-const KEY_DOWN)
(define-const KEY_LEFT)
(define-const KEY_RIGHT)

(define-c-lambda "IsKeyDown" (int) bool)
(define-c-lambda "IsKeyPressed" (int) bool)

;; Mouse
(define-const MOUSE_BUTTON_RIGHT)
(define-const MOUSE_BUTTON_LEFT)

(define-c-lambda "GetMousePosition" () vector2)
(define-c-lambda "IsMouseButtonPressed" (int) bool)
(define-c-lambda "IsMouseButtonDown" (int) bool)
