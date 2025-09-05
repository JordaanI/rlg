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
;; Date: 2025-09-03
;; email: ivan@axoinvent.com
;; Project: Main systems file for rlg
;;

(define systems-channel (string))
(define render-channel (string))
(define SCREEN_WIDTH (string))
(define SCREEN_HEIGHT (string))
(define SCREEN_HEIGHTF (string))
(define SCREEN_WIDTHF (string))


(define (pack-controls bools)
  (letrec ((pack (lambda (bools l)
                   (if (null? bools) 0
                       (+ (if (car bools) (arithmetic-shift 1 l) 0)
                          (pack (cdr bools) (- l 1)))))))
    (pack bools (- (length bools) 1))))

(define (generate-rendering-functions l)
  (let ((update (first l))
        (state (second l))
        (inputs (third l))
        (ext (last l)))
    (list (update state (pack-controls inputs)) state ext)))

(define (frame-definition #!key inputs state update (render-ext (lambda () #t)))
  (>> systems-channel (list update state inputs render-ext)))

(define (render-frame l)
  (let ((draw-procedure (first l))
        (state (second l))
        (ext (third l)))
    (begin-drawing)
    (clear-background white)
    (draw-procedure)
    (ext)
    (end-drawing)
    state))

(define (systems-init title #!key width height (fps 60))

  ;; Environment Vars
  (set! SCREEN_WIDTH width)
  (set! SCREEN_HEIGHT height)
  (set! SCREEN_HEIGHTF (exact->inexact height))
  (set! SCREEN_WIDTHF (exact->inexact width))

  ;; Start Window
  (init-window height width title)
  (set-target-fps fps)

  ;; Set channels
  (set! systems-channel (create-channel transducer: ($map generate-rendering-functions)))
  (set! render-channel (create-channel
                        default: (list
                                  (lambda () #t)
                                  #f
                                  (lambda () #t))
                        transducer: ($map render-frame)))
  (pipeline systems-channel render-channel))
