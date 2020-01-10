#lang racket
(require "coercion.rkt")
; REVISIT -- TEST AND INCLUDE COMPLETE IMPLEMENTATION (TYPED COMPONENTS OF COMPLEX NUMBERS) IN generic-arithmetic-tower.rkt

#|
To handle complex numbers whose components can be any type of number in our system,
we need only implement generic operations to replace the scheme operators we use inside the
rectangular, polar, and complex packages. This means, for instance, replacing (+) with add.
The call to atan would need to implement a call to div to turn its two arguments into one, then
a generic atan could dispatch to the correct procedure for the type of the result of that division.
We also need to implement some new operations (square, sqrt, sin, cos, and atan need to be replaced
with generic counterparts).

For most types, the easiest way to implement these operations is probably to coerce the arguments to
real numbers, then use the built-in operations.

Finally, the constructors for complex numbers should be updated to make sure the numbers are constructed
properly. If plain scheme numbers are passed as arguments to the constructors, they could be tagged as real
then passed to drop, for example.
;|#


; example, inside rational package
(define (real x)
  ((get-coercion 'rational 'real) x))
(define (square x) (mul x x))
(define (sqrt x)
  (cons 'real (sqrt (real x))))
(define (sin x)
  (cons 'real (sin (real x))))
(define (cos x)
  (cons 'real (cos (real x))))
(define (atan x)
  (cons 'real (atan (real x))))

