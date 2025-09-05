(define-library (utilities xml)
                (export 
                  ; accessors
                  match-child-sequence 
                  get-first-child 
                  set-children! 
                  get-children 
                  node-has-attributes? 
                  children-start-index 
                  find-child-tagged 
                  collect-children-tagged 
                  get-text-content 
                  get-tagname 
                  get-tagname-no-ns 
                  get-tagname-from-namespace 
                  get-attribute-name-no-ns 
                  strip-namespace 
                  get-node-attributes 
                  get-node-attributes-with-ns 
                  fix-attributes-names-no-namespace 
                  get-attribute-value 
                  get-subnode-by-address
                  ; utils
                  sxml-to-xml
                  ; parser
                  parse-xml-file
                  )
                
                (import 
                  (gambit))
                
                (include "lists.scm")
                (include "strings.scm")
                (include "xml-utilities.scm")
                (include "xml-accessors.scm")
                (include "xml-parser.scm"))
                
                  