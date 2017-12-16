;;tomer
(define (func a b) 
  ;; Inner Function - sums the closed interval [x,y]
  ((define (sum-interval x y) (
    (if (x >= y) x (+ x (sum-interval (+ 1 x) y)))
  ))
  ;; the function's body
  
    let ((sum (sum-interval a b)))
    (if (< a b)
      (* sum sum)
      (* sum 2)
    )
  )
)

(define (fplus1 f)
  (define (fpinner a b) (+ 1 (f a b)))
  fpinner)