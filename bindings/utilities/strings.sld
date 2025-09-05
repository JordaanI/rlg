(define-library (utilities strings)
                (export 
                  truncate-string
                  split-string
                  join-strings
                  join-strings-with-string
                  read-fields
                  char->string
                  8bitstring->utf8string
                  u8vector->utf8string
                  utf8-string->u8vector
                  8bitstring->u8vector
                  u8vector->utf8string-new
                  string-starts-with?
                  string-end-with?
                  read-from-string
                  read-all-from-string
                  lstring-append
                  convert-to-symbol
                  convert-to-string
                  escape-string-escapes
                  memstring
                  string-position
                  matching-initial-segment?
                  lowerString
                  upperString
                  string-replace-all
                  substitute-symbols
                  strip-all-spaces
                  generate-guid
                  get-current-time
                  get-current-timestamp
                  safe-getenv
                  >string)
                
                (import (gambit)
                        (utilities lists))
                
                (include "strings.scm"))
                
                  