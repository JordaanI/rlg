(define-library (utilities srfi-95)
  
  (export 
    sort
    sort!
    sorted?
    merge
    sort:merge!
    merge!)
  
  (import (gambit))
  
  (include "srfi-95.scm"))
                
                  