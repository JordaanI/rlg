(define-library (utilities time)
                (export 
                  Hour-Seconds
                  Day-Seconds
                  get-date
                  get-hour
                  get-minutes
                  get-date-midnight-uts
                  get-day-of-week
                  get-week-start-point
                  get-current-unix-timestamp
                  slot-start
                  availability-point)
                
                (import (gambit))
                
                (include "time.scm"))
                
                  