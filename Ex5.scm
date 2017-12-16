;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name:     Tomer Gill
;; I.D.:     318459450
;; Group:    89-310-05
;; Username: gilltom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PART 1

;; Help function - returns true if list e is the end of list l, other wise false
(define (list_end_with l e)
  (define (inner rl re)
    (cond
      ((null? re) #t)
      ((null? rl) #f)
      ((eqv? (car re) (car rl)) (inner (cdr rl) (cdr re)))
      (else #f)))
  (inner (reverse l) (reverse e))
  )

;; Returns true if str ends with suffix, otherwise false
(define (ends-with suffix str)
  (list_end_with (string->list str) (string->list suffix))
  )

;; load "test.scm"