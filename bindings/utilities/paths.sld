(define-library (utilities paths)
                
                (export 
                  initialize-local-storage
                  path-resolve)
                
                (import 
                  (gambit))
                
                (include "paths.scm"))
                
                  