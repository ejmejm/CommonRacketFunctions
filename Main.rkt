#lang racket

;List Functions

;#1 Append two lists
(define (my-append a b)
  (cond
     [(cons? a) (cons (first a) (my-append (rest a) b))]
     [(cons? b) (cons (first b) (my-append a (rest b)))]
     [else empty]))

;#2 Reverse a list
(define (my-reverse lst)
  (define (iter lst newlst)
  (cond
    [(empty? lst) newlst]
    [else (define nl (cons (first lst) newlst))
          (iter (rest lst) nl)]))
  (iter lst empty))

;#3 Map a function to every element in a list
(define (my-map func lst)
  (cond
     [(cons? lst) (cons (func (first lst)) (my-map func (rest lst)))]
     [else empty]))

;5 Fold left
(define (my-fold n op lst)
  (cond
    [(empty? lst) n]
    [else (my-fold (op n (first lst)) op (rest lst))]))

;6 Filter
(define (my-filter filt lst)
  (define (iter filt lst nlst)
  (cond
    [(empty? lst) (my-reverse nlst)]
    [(filt (first lst)) (iter filt (rest lst) (cons (first lst) nlst))]
    [else (iter filt (rest lst) nlst)]))
  (iter filt lst empty))

;Set Functions

;#1 Set membership
(define (my-member elem lst)
  (define result #f)
  (for ([i lst])
    (cond
      [(equal? i elem) (set! result #t)]))
  result)

;#2 Insert element into set
(define (my-insert elem lst)
  (cond
    [(equal? (my-member elem lst) #f) (cons elem lst)]
    [else lst]))

;#3 Set intersection
(define (my-intersection lst1 lst2)
  (define result empty)
  (for ([i lst1])
    (cond
      [(my-member i lst2) (set! result (cons i result))]))
  result)

;Set Functions

;#1 Absolute value
(define (my-abs n)
  (if (>= n 0)
      n
      (* -1 n)))

;#2 Factorial
(define (my-factorial n)
  (if (equal? n 0)
      1
      (* n (my-factorial (- n 1)))))

;#3 Right triangle hypoteneuse
(define (my-right-tri s1 s2 s3)
  (equal? (sqrt (+ (* s1 s1) (* s2 s2))) s3))

;Required Functions

;#1 Perfect numbers
(define (my-perfect? n)
  (define (iter n i sum)
    (cond
      [(equal? i 0) (equal? sum n)]
      [(equal? (modulo n i) 0) (iter n (- i 1) (+ sum i))]
      [else (iter n (- i 1) sum)]))
  (iter n (- n 1) 0))

;#2 Abundant Numbers
(define (my-abundant? n)
  (define (iter n i sum)
    (cond
      [(equal? i 0) (> sum n)]
      [(equal? (modulo n i) 0) (iter n (- i 1) (+ sum i))]
      [else (iter n (- i 1) sum)]))
  (iter n (- n 1) 0))

;#3 Deficient Numbers
(define (my-deficient? n)
  (define (iter n i sum)
    (cond
      [(equal? i 0) (< sum n)]
      [(equal? (modulo n i) 0) (iter n (- i 1) (+ sum i))]
      [else (iter n (- i 1) sum)]))
  (iter n (- n 1) 0))

(display "Examples\n")

(display "(Lists) 1. (my-append '(1 2 3) '(4 5 6)) Output:\n")
(my-append '(1 2 3) '(4 5 6))

(display "(Lists) 2. (my-reverse '(1 2 3 4 5 6)) Output:\n")
(my-reverse '(1 2 3 4 5 6))

(display "(Lists) 3. (my-map (lambda (d) (+ 3 d)) '(1 2 3 4 5 6)) Output:\n")
(my-map (lambda (d) (+ 3 d)) '(1 2 3 4 5 6))

(display "(Sets) 1. (my-member 'c '(a b c d e)) Output:\n")
(my-member 'c '(a b c d e))

(display "(Sets) 2 (Part 1). (my-insert 'a '(b c d)) Output:\n")
(my-insert 'a '(b c d))

(display "(Sets) 2 (Part 2). (my-insert 'a '(a b c d)) Output:\n")
(my-insert 'a '(a b c d))

(display "(Sets) 3. (my-intersection '(a b c) '(a c d)) Output:\n")
(my-intersection '(a b c) '(a c d))

(display "(Sets) 5. (my-fold 10 - '(1 3 2)) Output:\n")
(my-fold 10 - '(1 3 2))

(display "(Sets) 6. (define (lessthan3 x) (< x 3))\n(filter lessthan3 '(1 4 5 2 1 6)) Output:\n")
(define (lessthan3 x) (< x 3))
(my-filter lessthan3 '(1 4 5 2 1 6 0))

(display "(Math) 1 (Part 1). (my-abs 7) Output:\n")
(my-abs 7)

(display "(Math) 1 (Part 2). (my-abs -7) Output:\n")
(my-abs -7)

(display "(Math) 2. (my-factorial 5) Output:\n")
(my-factorial 5)

(display "(Math) 3 (Part 1). (my-right-tri 3 4 5) Output:\n")
(my-right-tri 3 4 5)

(display "(Math) 3 (Part 2). (my-right-tri 1 2 3) Output:\n")
(my-right-tri 1 2 3)

(display "(Required) 1 (Part 1). (my-perfect? 5) Output:\n")
(my-perfect? 5)

(display "(Required) 1 (Part 2). (my-perfect? 6) Output:\n")
(my-perfect? 6)

(display "(Required) 2 (Part 1). (my-abundant? 5) Output:\n")
(my-abundant? 5)

(display "(Required) 2 (Part 2). (my-abundant? 12) Output:\n")
(my-abundant? 12)

(display "(Required) 3 (Part 1). (my-deficient? 5) Output:\n")
(my-deficient? 5)

(display "(Required) 3 (Part 2). (my-deficient? 12) Output:\n")
(my-deficient? 12)