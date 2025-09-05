;;;;
;;;; Buffered Channels
;;; A abstraction to decompose program architecture into multiple working parts (transducers)
;;; and have those parts talk to each other through channels via pipelines
;;; Each channel has a queue with possibly a fixed capacity.
;;; Subscribers can register to the channel to receive updates.
;;; Channel can have an optional transducer attached to it that is applied before any data that you consume out of the channel.

;;;
;;;

;;;
;;;; channel type
;;;


(define-type channel id: C4A87D43-F69A-4BB2-9BC5-E91E7F76CC35 size queue transducer)

;;;
;;;; create-channel
;;;


(define (create-channel #!key (size 0) (transducer $identity) (default #f))
  (if default (make-channel size (new-sticky-queue default: default) transducer)
      (make-channel size (new-fifo-queue size: size) transducer)))


;;;
;;;; channel-consumer
;;;


;;;
;;; This is a channel consuming thread that applies the channel input transducer to the inputs.
;;; You supply a reducer that will process those transduced values.
;;; All consumers should use this approach to consume a channel (no custom consuming in fancy ways for now).
;;;
;;; More advanced consuming strategies, for load balancing for example, could come later.


(define (channel-consumer channel reducer)
  (thread
   (lambda ()
     (channel-transduce (channel-transducer channel) reducer channel))))


;;;
;;;; channel-generator
;;;

                                        ; creates a generator from a channel
                                        ; this is low-level one that doesn't do prescribed transduction
                                        ; todo: create a high-level version that does apply it. Or NOT! (de we really want people to mess with that)


(define (channel-generator channel)
  (lambda ()
    (_<! channel)))


;;;
;;;; pipeline
;;;

;;;
;;;  This is a simple channel consumer that reduces by sending to another channel
;;;  The pipeline sends transduced values (transduced by channel transducer) to that c2 channel.
;;;


(define (pipeline c1 c2)
  (channel-consumer c1 (%pipeline-reducer c2)))



(define (chain-channels-with-pipelines . channels)
  (for-each
   (let ((prev-chan #f))
     (lambda (ch)
       (if prev-chan
           (pipeline prev-chan ch))
       (set! prev-chan ch)))
   channels))


;;;
;;;; >>
;;;


(define (>> channel message)
  (let ((queue (channel-queue channel)))
    ((if (fifo-queue? queue) fifo-queue-push sticky-queue-push) queue message)))

;; Note that channel transducer is not applied until there is a consuming process that starts.
;; This is a lazy approach, which is something we should favor.


;;;
;;;; <!
;;;


                                        ; low level
(define (_<! channel)
  (let ((queue (channel-queue channel)))
    ((if (fifo-queue? queue) fifo-queue-pull sticky-queue-pull) queue)))


                                        ; high level
(define (<! channel)
  (transduce-one-value (channel-transducer channel) (_<! channel)))

;;;
;;;; create-ioport-pipes-channels
;;;

                                        ; ***
                                        ; The base pattern to implement any client/server communication agents and more...
                                        ; ***
                                        ;
                                        ;
                                        ; Wraps a pair of transducers (in transducer, out transducer) around an io/port.
                                        ;
                                        ; Creates a pair of channels from a gambit i/o port.
                                        ; One "input" channel that is automatically fed by the i/o port input.
                                        ; One "output" channel that automatically feeds the i/o port output port.
                                        ;
                                        ; So basically one can read from input_channel (using given reader)  and write to output_channel (using the given writer), the rest is
                                        ; handled by the system to interact with the supplied port and apply the prescribed transducers.

                                        ; Reads from channel and processes them with input transducer before putting in the in_channel
                                        ; To send something to port. just push a message to the out_channel
                                        ; It will get transduced by the output transducer and written in the port.
                                        ;


(define (create-ioport-pipes-channels ioport _reader _writer in_transducer out_transducer #!key (in_cmd identity) (out_cmd identity))
  (let ((in-channel (create-channel transducer: in_transducer))
        (out-channel (create-channel)))
    (thread
     (lambda ()
       (port-transduce
        ($map (lambda (decoded-input)
                (>> in-channel decoded-input)
                decoded-input))
        (%processor in_cmd)
        _reader
        ioport)
       (close-channel in-channel)
       (close-channel out-channel)))
    (thread
     (lambda ()
       (channel-transduce
        ($compose
         ($map out_cmd)
         out_transducer)
        (%processor _writer)
        out-channel)))
    (cons in-channel out-channel)))


(define (consume-ioport-input-channel in_channel action #!key (transducer $identity))
  (thread
   (lambda ()
     (channel-transduce
      ($compose
       transducer
       (channel-transducer in_channel))
      (%processor action)
      in_channel))))



;;;
;;;; close-channel
;;;


(define (close-channel channel)
  (>> channel (eof-object)))

;;;
;;;; Channel empty?
;;;

(define (channel-empty? c)
  (fifo-queue-empty? (channel-queue c)))

;;;
;;;; Channel not empty
;;;

(define (channel-not-empty? c)
  (not (channel-empty? c)))


;;;
;;;; tests
;;;


#|

(define (eof-tests)
(let* ((c1 (create-channel))
(c2 (create-channel))
(c3 (create-channel))
(print-all (lambda () (for-each pp (list c1 c2 c3)))))
(>> c1 "otto")
(print-all)
(chain-pipelines c1 c2 c3)
(print-all)
(>> c2 "toot")
(print-all)
(channel-consumer c3 %log)
(print-all)
(close-channel c2)
(>> c1 "otot")
(print-all)))

|#
