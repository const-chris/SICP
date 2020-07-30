#lang sicp

#|
(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
        (stream-car stream)
        (delay (flatten-stream (stream-cdr stream))))))

flatten-stream uses delay explictly because without delaying the second argument to interleave-delayed here, we would essentially be converting from a stream to a list.
Before being passed to interleave-delayed, the inner call to flatten-stream would be evaluated, which in turn would evaluate the stream-car of the stream-cdr of the original stream and call flatten-stream again, which would evaluate the stream-car of the stream-cdr of the stream-cdr...

In other words, without explicit delay here, we completely throw away laziness.

The worst case is when our stream is infinite, as we've seen in 4.71 and 4.72. Instead of printing results as they are calculated, nothing would be printed as the system tries to eagerly evaluate the entire stream.
|#
