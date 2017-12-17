;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name:     Tomer Gill
;; I.D.:     318459450
;; Group:    89-310-05
;; Username: gilltom
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Helping Functions - Filter & Reduce ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (filter p ls)
  (if (null? ls)
      ls
      (let
      (               
           (hd (car ls))
           (rest (cdr ls)))
                          
        (
         if (p hd)
            (cons hd (filter p rest))
            (filter p rest)
            )
       )))


(define (reduce binFunc u)
  (define (reduce-helper ls)
    (if (null? ls)
        u
        (binFunc (car ls) (reduce-helper (cdr ls)))))
  reduce-helper)

      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PART 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Help function - returns true if list e is the end of list l, other wise false
(define (list_end_with l e)
  (define (inner rl re)
    (cond
      ((null? re) #t)
      ((null? rl) #f)
      ((char=? (car re) (car rl)) (inner (cdr rl) (cdr re)))
      (else #f)))
  (inner (reverse l) (reverse e))
  )

;; 1.1
;; ends-with
;; Returns true if str ends with suffix, otherwise false
(define (ends-with suffix str)
  (list_end_with (string->list str) (string->list suffix))
  )


;; 1.2
;; mul-of-pairs
;; Returns the multiplication of the values of the strings that ends with suffix
(define (mul-of-pairs suffix ls)
  (
   ;; declerations
   let (
        ;; returns a list with only the pairs which the string ends with suffix
        (filtered (filter (lambda (str-num) (ends-with suffix (car str-num))) ls))
        ;; Multiplies the each pair's second item in list
        (multiply-pairs (reduce (lambda (pr num) (* (cdr pr) num)) 1))
        )
    ;; body
    (multiply-pairs filtered)
    )
  )


;; 1.3
;; merge
;; Merges 2 lists together, starting from l1
(define (merge ls1 ls2)
  (define (merge_helper l l1 l2)
    (cond
      ((null? l1) (if (null? l2) l (append l l2)))
      ((null? l2) (append l l1))
      (else (merge_helper (append (append l (list (car l1))) (list (car l2))) (cdr l1) (cdr l2)))
      )
    )
  (merge_helper '() ls1 ls2)
  )


;; Help function - modulo
(define (mod x n)
  (cond
    ((= n 0) 0) ;; For safety
    ((< x 0) (mod (abs x) n))
    ((< x n) x)
    (else (mod (- x n) n))
    )
  )


;; Help function - returns a sub-list of the list up to the n-th place
(define (get_sublist l n)
  (cond
    ((null? l) l)
    ((= n 0) '())
    ((< n 0) l)
    (else (cons (car l) (get_sublist (cdr l) (- n 1))))
    )
  )


;; 1.4
;; rotate
;; Returns the list, rotated n times
(define (rotate ls n)
  (cond
     ((null? ls) ls)
     ((= 0 n) ls)
     (else
      (let* (
         (len (length ls))
         (m (mod n len))
         )
      (append (reverse (get_sublist (reverse ls) m)) (get_sublist ls (- len m)))))
     )
    )


;; 1.5
;; quicksort
;; Returns a function to quicksort a list with the given comparing function
(define (quicksort comp)
  (define (qs-helper ls)
    (if (null? ls)
        ls
        (let* (
               (pivot (car ls))
               ;; ordered list of elements < pivot
               (lesser (qs-helper (filter (lambda (x) (< (comp x pivot) 0)) ls)))
               (eq (filter (lambda (x) (= (comp x pivot) 0)) ls))  ;; list of elements = pivot
               ;; ordered list of elements > pivot
               (greater (qs-helper (filter (lambda (x) (> (comp x pivot) 0)) ls)))
               )
          (append lesser (append eq greater))
          )
        )
    )
  qs-helper
  )


;; load "test.scm"