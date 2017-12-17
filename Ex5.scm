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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PART 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each Stream is a pair (head, func) where head is the first element in the stream, and func is a
;; function used to generate the next element(s) from the first one.


;; 2.0.1
;; hd 
;; Returns the first element of the stream
(define (hd s) (car s))


;; 2.0.2
;; tail
;; Returns a new stream from the input stream, without it's head
(define (tail s)
  (let (
        (func (cdr s))
        )
    (cons (func (hd s)) func)
    )
  )


;; 2.2 (I'm using seq-gen to create all streams)
;; seq-gen
;; Returns a stream made by g that starts with n
(define (seq-gen n g) (cons n g))


;; 2.1
;; seq
;; Returns a stream representing the sequence [n, Infinity) (aka {n, n+1, n+2, ...})
(define (seq n) (seq-gen n (lambda (x) (+ x 1))))


;; Help function - Gets the next element
(define (get_next x ls)
  (cond
    ((null? ls) ls)  ;; list is empty - return it
    ((null? (cdr ls)) ls)  ;; list has only one item - return non-empty list
    ((= x (car ls)) (car (cdr ls)))  ;; found item, return next
    (else (get_next x (cdr ls)))  ;; continue search
    )
  )


;; 2.3
;; cyclic-seq
;; Returns a stream that cycles the list
(define (cyclic-seq ls)
   (define (cyclic_get_next x)
     (let (
           (next (get_next x ls))  ;; find the next element in list after x
           )
       ;; if a list is returned, it means x is the last item and the head should be returned
       (if (or (null? next) (pair? next))  
           (car ls)
           next  ;; else - just return the found next
           )
       )
     )
  (seq-gen (car ls) cyclic_get_next)
  )
   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PART 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Helper function - Given a (key . value) list and pair, add the pair to the list or replace another
;; pair already in the list with the same key (key is string)
(define (list_add_key_val ls kv)
  (cond
    ((null? ls) (list kv))  ;; got to end of list, add the new value to list
    ((string=? (car kv) (car (car ls))) (cons kv (cdr ls)))  ;; found key, replace key & value in list
    (else (cons (car ls) (list_add_key_val (cdr ls) kv)))  ;; else add the list so far to the rest
    )
  )
      

;; Help function - Finds the value of key k in (key . value) list ls, or empty list if not found.
(define (find_val_by_key ls k)
  (cond
    ((null? ls) '())  ;; key doesn't exist - return empty list
    ((string=? k (car (car ls))) (cdr (car ls)))  ;; found key, return value
    (else (find_val_by_key (cdr ls) k))  ;; continue searching the rest of dictionary
    )
  )


;; 3
;; make-dictionary
;; Returns a "callable" dictionary (it's keys are strings), that can be called with:
;; a.  A (key . value) pair - Returns a new dictionary with the new pair, or if pair exists in dict 
;;      replace it's value by the new pair's value
;; b.  A key (string) - Returns the value by the key, or returns an empty list
;; c.  An empty list - Returns a list holding all the (key . value) pairs in the dictionary
(define (make-dictionary)
  (define (dict_helper lst)  ;; creates a new dictionary from lst
    (define (dictionary x)  ;; a dictionary function
      (define ls lst)  ;; the list holding the pairs
      (cond
        ((null? x) ls)  ;; empty list (c.)
        ((pair? x) (dict_helper (list_add_key_val ls x)))  ;; a pair (a.)
        (else (find_val_by_key ls x))  ;; a key (b.)
        )
      )
    dictionary  ;; returns the dictionary
    )
  (dict_helper '())  ;; creates a new empty dictionary
  )
    


;; load "test.scm"