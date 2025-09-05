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

;; Color
(c-define-type color (struct "Color"))
(define make-color
  (c-lambda (unsigned-int8 unsigned-int8 unsigned-int8 unsigned-int8) color
            "Color col = (Color){___arg1, ___arg2, ___arg3, ___arg4};
            ___return(col);"))

;; Vector2
(c-define-type vector2 (struct "Vector2"))
(define make-vector2 (c-lambda (float float) vector2
                               "Vector2 vec =(Vector2){___arg1, ___arg2};
                               ___return(vec);"))
(define vector2-x (c-lambda (vector2) float "___return(___arg1.x);"))
(define vector2-y (c-lambda (vector2) float "___return(___arg1.y);"))
(define vector2-x-set! (c-lambda (vector2 float) vector2
                                 "___arg1.x = ___arg2;
                                  ___return(___arg1);"))
(define vector2-y-set! (c-lambda (vector2 float) vector2
                                 "___arg1.y = ___arg2;
                                  ___return(___arg1);"))


;; Camera2D
(c-define-type camera2D (struct "Camera2D"))
(define make-camera2D (c-lambda () camera2D
                                "Camera2D cam = { 0 };
                               ___return(cam);"))

(define camera2D-target (c-lambda (camera2D vector2) void
                                  "___arg1.target = ___arg2;"))

(define camera2D-zoom (c-lambda (camera2D float) void
                                "___arg1.zoom = ___arg2;"))
