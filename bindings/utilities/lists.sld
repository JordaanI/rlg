(define-library (utilities lists)
                (export 
                  butlast
                  remove-element-at
                  list-head
                  pick-random-element
                  pick-random-subset
                  union
                  intersection
                  intersection-m
                  difference
                  member2
                  andf
                  orf
                  mapcount
                  ormap
                  andmap
                  andmap_v2
                  special-map
                  multi-map
                  improper-map
                  improper-fold
                  improper-fold-right
                  improper-append
                  improper-last
                  improper-for-each
                  collect-if
                  every
                  some
                  prisme-true?
                  remove-index
                  find-if
                  find-it
                  find-it-sxml
                  find-it-anywhere
                  first-same
                  list-position
                  replace
                  decompose-list-in-pairs
                  repeat
                  for
                  catch
                  sublist
                  identities
                  flatten
                  make-set
                  set-minus
                  make-set2
                  $partition
                  log-message)
                ;(import (scheme base))
                (import gambit)
                (include "lists.scm"))
                
                  