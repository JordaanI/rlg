(define-library (utilities json)
                
                (export 
                  json-write-string
                  json-write
                  json-read
                  json-read-string)
                
                (import 
                  (gambit)
                  (utilities strings))
                
                (include "json.scm"))
                
                  