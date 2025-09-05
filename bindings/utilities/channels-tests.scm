
(include "lists.scm")
(include "strings.scm")
(include "json.scm")
(include "fixnum.scm")
(include "homovector.scm")
(include "base64.scm")
(include "case-lambda.scm")
(include "transducers.scm") 
(include "queues.scm")
(include "channels.scm")

#|

; simple channels tests


(define c (create-channel #f transducer: ($map string-length)))
(channel-consumer c %log)
(>> c "toto")

(define c1 (create-channel #f transducer: ($map string-length)))
(define c2 (create-channel #f))

(pipeline c1 c2)
(channel-consumer c2 %log)

; example with a %processor
(channel-consumer c2 (%processor (lambda (input) 
                                   (display (string-append "R: " (number->string input)))
                                   (newline)
                                   (force-output))))

|#


(define (line-writer port)
  (lambda (line)
    (display line port)
    (newline port)
    (force-output port)))


; simple tcp client
(define (start-sample-client)
  (let ((tcp-port (open-tcp-client  (list 
                                      address: (string-append "127.0.0.1" ":" (number->string 8080))
                                      eol-encoding: 'lf))))
    (create-ioport-pipes-channels
      tcp-port
      read-line
      (line-writer tcp-port)
      ($map json-read-string)
      ($map json-write-string))))


#|
(>> chan (list->table
           `(("command" . "valueChanged")
             ("address" . ()))))
|#



; simple tcp server
(define (start-sample-server)
  (thread
    (lambda ()
      (port-transduce
        $identity 
        (%processor (lambda (conn)
                      (pp (create-ioport-pipes-channels
                            conn
                            read-line
                            (line-writer conn) 
                            ($map json-read-string) 
                            ($compose
                              ($debounce 1000)
                              ($map json-write-string))))))
        read
        (open-tcp-server
          (list server-address: "*"
            port-number: 8080
            reuse-address: #t
            char-encoding: 'ISO-8859-1
            eol-encoding: 'lf))))))


(define (test-send ch)
  (repeat 15 (lambda ()
               (thread-sleep! 0.1)
               (>> ch (list->table `(("command" . "toto")))))))
   
   
   


#|

; test proper thread ending for consumers
(define ch (create-channel transducer: ($map table->list)))

(channel-consumer ch %log)

(close-channel ch)

(define ch (create-channel))
(channel-consumer ch %log)
(close-channel ch)




(define ch (create-channel transducer: ($debounce 1000)))
(channel-consumer ch %log)
(test-send ch)




|#
