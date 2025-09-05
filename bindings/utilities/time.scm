;;;
;;;; constants
;;;


(define Hour-Seconds (* 60 60))
(define Day-Seconds (* 24 Hour-Seconds))
(define Time-Zone-Factor -5)
(define Week-Seconds (* 7 Day-Seconds))

;;;
;;;; get-hour
;;;


(define (get-hour uts)
  (fourth (seconds->time-list uts)))


;;;
;;;; get-minutes
;;;


(define (get-minutes uts)
  (fifth (seconds->time-list uts)))


;;;
;;;; get-date-midnight-uts
;;;


(define (get-midnight-uts)
  (let* ((now (inexact->exact (time->seconds (current-time))))
         (now-list (seconds->time-list now zone-factor: 0)))
    (time-list->seconds (list (car now-list) (cadr now-list) (caddr now-list) 0 0 0))
    ))


;;;
;;;; get-day-of-week
;;;


(define (get-day-of-week uts)
  (modulo (- (modulo (floor (/ uts Day-Seconds)) 7) 3) 7))

;;;
;;;; get-week-start-point
;;;


(define (get-week-start-point)
  (get-timestamp-week-start-point (get-current-time-round)))


(define (get-timestamp-week-start-point ts)
  (let ((midnight (+ (* -1 Time-Zone-Factor Hour-Seconds) (get-midnight-uts)))
        (day (get-day-of-week ts)))
    (- midnight (* Day-Seconds day))))


;;;
;;;; slot-start
;;;


(define (slot-start slot-index week-offset)
  (let ((ws (get-week-start-point)))
    (+ ws (* week-offset (* 7 Day-Seconds)) (* slot-index (/ Hour-Seconds 2)))))


;;;
;;;; current-slot
;;;


(define (get-current-slot)
  (floor (/ (- (get-current-time-round) (get-week-start-point)) 1800))) 


;;;
;;;; get-timestamp-week-offset
;;;


(define (get-timestamp-week-offset ts)
  (let ((now-weak-start (get-week-start-point)))
    (floor (/ (- ts now-weak-start) Week-Seconds))))

;;;
;;;; availability zones testing
;;;


(define (availability-point week-offset day hour mins)
  (let ((ws (get-week-start-point)))
    (+ ws (* week-offset (* 7 Day-Seconds)) (* day Day-Seconds) (* hour Hour-Seconds) (* mins 60))))


(define (is-in-availabilities? job-start job-end week-offset tech-availabilities)
  (if (null? tech-availabilities)
      #f
    (or (is-in-availability? job-start job-end week-offset (car tech-availabilities))
        (is-in-availabilities? job-start job-end week-offset (cdr tech-availabilities)))))


(define (is-in-availability? job-start job-end week-offset availability-instance)
  (let* ((day_inst (first (get-instance-property-values clipboard-ledger availability-instance 'AvailabilityDay)))
         (day_label (get-instance-name clipboard-ledger day_inst))
         (day_index (compute-day-index day_label))
         (avail_hour_start (get-hour (ui-ts-to-unix-ts (first (get-instance-property-values clipboard-ledger availability-instance 'AvailabilityStarttime)))))
         (avail_min_start (get-minutes (ui-ts-to-unix-ts (first (get-instance-property-values clipboard-ledger availability-instance 'AvailabilityStarttime)))))
         (avail_hour_end (get-hour (ui-ts-to-unix-ts (first (get-instance-property-values clipboard-ledger availability-instance 'AvailabilityEndtime)))))
         (avail_min_end (get-minutes (ui-ts-to-unix-ts (first (get-instance-property-values clipboard-ledger availability-instance 'AvailabilityEndtime)))))
         (avail_start (availability-point week-offset day_index avail_hour_start avail_min_start))
         (avail_end (availability-point week-offset day_index avail_hour_end avail_min_end)))
    ;(pp (list " is-in-availability?" job-start job-end avail_start avail_end (<= avail_start job-start avail_end) (<= avail_start job-end avail_end)))
    (and 
      (<= avail_start job-start avail_end)
      (<= avail_start job-end avail_end))))


(define (compute-day-index day_label)
  (case (string->symbol day_label)
    ('Sunday 0)
    ('Monday 1)
    ('Tuesday 2)
    ('Wednesday 3)
    ('Thursday 4)
    ('Friday 5)
    ('Saturday 6)))
    

(define (ui-ts-to-unix-ts ui_ts)
  (if (string? ui_ts)
      (string->number ui_ts)
    ui_ts))


(define (unix-ts-to-ui-ts t)
  (number->string t))


;;;
;;;; unavailability zones testing
;;;


(define (intersects-unavailability? job-start job-end tech-unavailabilities)
  (if (null? tech-unavailabilities)
      #f
    (or
      (intersects-specific-unavailability? job-start job-end (car tech-unavailabilities))
      (intersects-unavailability? job-start job-end (cdr tech-unavailabilities)))))


(define (intersects-specific-unavailability? job-start job-end unavailability)
  (let* ((unavailability-start (ui-ts-to-unix-ts (first (get-instance-property-values clipboard-ledger unavailability 'UnavailabilityStarttime))))
         (unavailability-end (ui-ts-to-unix-ts (first (get-instance-property-values clipboard-ledger unavailability 'UnavailabilityEndtime)))))
    ;(pp (list "intersects-specific-unavailability?" unavailability-start unavailability-end job-start job-end))
    (or (<= unavailability-start job-start unavailability-end)
        (<= unavailability-start job-end unavailability-end))))
         


;;;
;;;; miliseconds->time-list
;;;


(define (miliseconds->time-list ms #!key (zone-factor Time-Zone-Factor))
  (let* ((seconds (floor (/ ms 1000)))
         (milis (- ms (* 1000 seconds)))
         (stl (seconds->time-list seconds zone-factor: zone-factor)))
    (append 
      (butlast stl)
      (list milis))))


;;;
;;;; seconds->time-list
;;;


(define (seconds->time-list time-in-seconds #!key (zone-factor Time-Zone-Factor))
  (let ((ctis (+ time-in-seconds (* zone-factor 3600)))
        (y  1970)
        (mo 0)
        (d  0)
        (h  0)
        (mi 0)
        (s  0))
    (letrec ((cycle-months (lambda (time)
                             (let ((s-in-mo (* 60 60 24 (get-nb-days-in-month-of-year mo y))))
                               (if (>= time s-in-mo)
                                   (begin (set! mo (+ 1 mo))
                                          (cycle-months (- time s-in-mo)))
                                 (begin (set! d (inexact->exact (floor (/ time (* 24 60 60)))))
                                        (set! time (- time (* d 24 60 60)))
                                        (set! h (inexact->exact (floor (/ time (* 60 60)))))
                                        (set! time (- time (* h 60 60)))
                                        (set! mi (inexact->exact (floor (/ time 60))))
                                        (set! s (- time (* mi 60))))))))
             (cycle-years (lambda (time)
                            (let ((s-in-y (* 60 60 24 (if (bisextile? y) 366 365))))
                              (if (>= time s-in-y)
                                  (begin (set! y (+ 1 y))
                                         (cycle-years (- time s-in-y)))
                                (cycle-months time))))))
      (cycle-years (inexact->exact (floor ctis)))
      (list y mo d h mi s 0))))


(define (time-list->seconds time-list #!key (zone-factor Time-Zone-Factor))
  (let ((y  (first time-list))
        (mo (second time-list))
        (d  (third time-list))
        (h  (+ (fourth time-list) (* -1 zone-factor)))
        (mi (fifth time-list))
        (s  (sixth time-list)))
    (letrec ((cycle-months (lambda (mo)
                             (if (= 0 mo)
                                 0
                               (+ (get-nb-days-in-month-of-year (- mo 1) y)
                                  (cycle-months (- mo 1))))))
             (cycle-years (lambda (y)
                            (if (= 1970 y)
                                0
                              (+ (if (bisextile? (- y 1)) 366 365)
                                 (cycle-years (- y 1)))))))
      (+ s
         (* 60 mi)
         (* 60 60 h)
         (* 60 60 24 (+ d (cycle-months mo) (cycle-years y)))))))


(define (time-list->milliseconds time-list #!key (zone-factor Time-Zone-Factor))
  (+ (* 1000 (time-list->seconds time-list zone-factor: zone-factor)) (seventh time-list)))


;;;
;;;; time-interval->seconds
;;;


(define (time-interval->seconds base-time-in-seconds time-unit time-interval)
  (let* ((t1 (seconds->time-list base-time-in-seconds))
         (y  (first t1))
         (mo (second t1))
         (d  (third t1))
         (h  (fourth t1))
         (mi (fifth t1))
         (s  (sixth t1)))
    (letrec ((find-nb-days-in-months (lambda (nb-months)
                                       (if (= 0 nb-months)
                                           0
                                           (let ((nb-days (get-nb-days-in-month-of-year mo y)))
                                             (if (= mo 11)
                                                 (begin (set! mo 0)
                                                        (set! y (+ y 1)))
                                                 (set! mo (+ 1 mo)))
                                             (+ nb-days (find-nb-days-in-months (- nb-months 1)))))))
             (find-nb-days-in-years (lambda (nb-years)
                                       (if (= 0 nb-years)
                                           0
                                           (let ((nb-days (if (or (and (< mo 2) (bisextile? y))
                                                                  (and (> mo 1) (bisextile? (+ 1 y))))
                                                              366
                                                              365)))
                                             (set! y (+ y 1))
                                             (+ nb-days (find-nb-days-in-years (- nb-years 1))))))))
      (case time-unit
        ('seconds time-interval)
        ('minutes (* 60 time-interval))
        ('hours   (* 60 60 time-interval))
        ('days    (* 60 60 24 time-interval))
        ('weeks   (* 60 60 24 7 time-interval))
        ('months  (* 60 60 24 (find-nb-days-in-months time-interval)))
        ('years   (* 60 60 24 (find-nb-days-in-years time-interval)))))))


(define (get-nb-days-in-month-of-year month year)
  (cond ((member month '(0 2 4 6 7 9 11)) 31)
        ((member month '(3 5 8 10))       30)
        ((bisextile? year)                29)
        (else                             28)))


(define (bisextile? year)
  (and (= 0 (modulo year 4))              ; julian calendar
       (or (not (= 0 (modulo year 100)))  ; gregorian calendar
           (= 0 (modulo year 400)))))


;;;
;;;; formatted-time
;;;


(define (format-datetime t)
  (time-list->widget-datetime-string (seconds->time-list t)))

(define (format-time t)
  (time-list->widget-time-string (seconds->time-list t)))

(define (time-list->widget-datetime-string time-list)
  (let ((y (first time-list))
        (m (+ 1 (second time-list)))
        (d (+ 1 (third time-list)))
        (h  (fourth time-list))
        (mi (fifth time-list))
        (s  (sixth time-list)))
    (string-append 
      (number->string y)
      "-"
      (two-digits-integer-string m)
      "-"
      (two-digits-integer-string d)
      " "
      (two-digits-integer-string h)
      ":"
      (two-digits-integer-string mi))))


(define (time-list->iso-timestamp tlist)
  (let ((y (first tlist))
        (m (+ 1 (second tlist)))
        (d (+ 1 (third tlist)))
        (h  (fourth tlist))
        (mi (fifth tlist))
        (s  (sixth tlist))
        (ms (seventh tlist)))
    (string-append
      (number->string y)
      "-"
      (two-digits-integer-string m)
      "-"
      (two-digits-integer-string d)
      "T"
      (two-digits-integer-string h)
      ":"
      (two-digits-integer-string mi)
      ":"
      (two-digits-integer-string s)
      "."
      (three-digits-integer-string ms)
      "Z")))


(define (time-list->widget-time-string time-list)
  (let ((h  (+ 1 (fourth time-list)))
        (mi (fifth time-list))
        (s  (sixth time-list)))
    (string-append 
      (two-digits-integer-string h)
      ":"
      (two-digits-integer-string mi))))


(define (two-digits-integer-string n)
  (if (<= n 9)
     (string-append "0" (number->string n))
    (number->string n))) 


(define (three-digits-integer-string n)
  (cond
    ((<= n 9)
      (string-append "00" (number->string n)))
    ((<= 10 n 99)
     (string-append "0" (number->string n)))
    (#t
     (number->string n))))

