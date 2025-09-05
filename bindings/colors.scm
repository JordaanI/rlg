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

(define-c-lambda "ColorFromHSV" (float float float) color)

(define (resolve-color color)
  (apply color-from-hsv (table-ref color-table color)))

(define color-table
  (list->table `((_red 0.0 1.0 1.0)
                 (_green 120.0 1.0 1.0)
                 (_blue  240.0 1.0 1.0)
                 (_yellow 60.0 1.0 1.0)
                 (_white 0.0 0.0 1.0))))

(define-macro (define-color color)
  `(define ,color (resolve-color (quote ,(string->symbol (string-append "_" (symbol->string color)))))))

(define-color red)
(define-color green)
(define-color yellow)
(define-color blue)
(define-color white)
