;;;
;;;; maybe-create-directory
;;;


(define (maybe-create-directory dir)
  (if (not (file-exists? dir))
      (create-directory dir)))


;;;
;;;; initialize-local-storage
;;;


(define (initialize-local-storage)
  (let ((storage-dir (localstorage-base-dir)))
    (if (not (file-exists? storage-dir))
        (begin
          (maybe-create-directory storage-dir)
          (maybe-create-directory (string-append storage-dir "data"))
          (copy-models)))))


(define (copy-models)
  (let ((target_dir (string-append (localstorage-base-dir) "/models/")))
    (maybe-create-directory target_dir)
    (copy-file "../models/axonimo.gmot" (string-append target_dir "axonimo.gmot"))
    (copy-file "../models/axoterra.gmot" (string-append target_dir "axoterra.gmot"))
    (copy-file "../models/wave.gmot" (string-append target_dir "wave.gmot"))))


;;;
;;;; path-resolve
;;;


(define (localstorage-base-dir)
  (if embedded 
      (string-append (paths-ref 'documents) "/")
    (or 
      (get-environment-variable "AXO_HOME")
      (string-append (get-environment-variable "HOME") "/AXO/"))))


(define (path-resolve path)
  (initialize-local-storage)
  (string-append (localstorage-base-dir) path))