(define (sub1 n) (- n 1))

(define (add1 n) (+ n 1))

(define (atom? a)
  (not (or (null? a) (pair? a))))

(define (eqan? a1 a2) 
  (cond ((and (number? a1) (number? a2)) (= a1 a2))
        ((or (number? a1) (number? a2)) #f)
        (else (eq? a1 a2))))

(define (lat? lat)
  (cond ((null? lat) #t)
        ((atom? (car lat)) (lat? (cdr lat)))
        (else #f)))

(define (tup+ tup1 tup2)
  (cond ((null? tup1) tup2)
        ((null? tup2) tup1)
        (else
         (cons (+ (car tup1) (car tup2))
               (tup+ (cdr tup1) (cdr tup2))))))

(tup+ '(1 2) '(4 5))

(define (> n m)
  (cond ((zero? n) #f)
        ((zero? m) #t)
        (else (> (sub1 n) (sub1 m)))))

(define (= n m)
  (cond ((> n m) #f)
        ((< n m) #f)
        (else #t)))

(define (power n m)
  (cond ((zero? m) 1)
   (else (* n (power n (sub1 m))))))


(define (pick i lat)
  (cond ((null? lat) #f)
        ((= i 1) (car lat))
        (else (pick (sub1 i) (cdr lat)))))

(define (rember a lat)
  (cond ((null? lat) '())
        ((eq? a (car lat)) (rember a (cdr lat)))
        (else (cons (car lat) (rember a (cdr lat))))))

(define (rember* a lat)
  (cond ((null? lat) '())
        ((and (atom? lat) 
              (eq? lat a) '()))
        ((atom? lat) lat)
        ((lat? lat) (rember a lat))
        (else
         (cons (rember* a (car lat)) (rember* a (cdr lat))))))

(define (occur* a l)
  (cond ((null? l) 0)
        ((atom? (car l))
         (cond ((eq? a (car l)) (add1 (occur* a (cdr l))))
               (else (occur* a (cdr l)))))
        (else (+ (occur* a (car l))
                 (occur* a (cdr l))))))

(define (subst* new old l)
  (cond ((null? l) l)
        ((atom? (car l))
         (cond ((eq? old (car l))
                (cons new (subst* new old (cdr l))))
               (else (cons (car l) (subst* new old (cdr l))))))
        (else (cons (subst* new old (car l))
                    (subst* new old (cdr l))))))

(define (numbered? aexp)
  (cond ((atom? aexp) (number? aexp))
        (else (and (numbered? (car aexp))
                   (numbered? (car (cdr (cdr aexp))))))))

(define (value exp)
  (cond ((atom? exp) exp)
        ((eq? (quote +) (car (cdr exp)))
         (+ (value (car exp))
            (value (car (cdr (cdr exp))))))
        ((eq? (quote x) (car (cdr exp)))
         (* (value (car exp))
            (value (car (cdr (cdr exp))))))
        (else (power (value (car exp))
                      (value (car (cdr (cdr exp))))))))
(value '(1 + 2))

;(+ exp1 exp2)
(define (value2 exp)
  (cond ((atom? exp) exp)
        ((eq? (quote +) (car exp))
         (+ (value2 (car (cdr exp)))
            (value2 (car (cdr (cdr exp))))))
        ((eq? (quote x) (car exp))
         (* (value2 (car (cdr exp)))
            (value2 (car (cdr (cdr exp))))))
        ((eq? (quote -) (car exp))
         (- (value2 (car (cdr exp)))
            (value2 (car (cdr (cdr exp))))))
        ((eq? (quote /) (car exp))
         (/ (value2 (car (cdr exp)))
            (value2 (car (cdr (cdr exp))))))        
        (else (power (value2 (car (cdr exp)))
                     (value2 (car (cdr (cdr exp))))))))

(value2 '(+ 2 (/ 3 (x 9 4))))

(define (member? a l)
  (cond ((null? l) #f)
        ((eqv? a (car l)) #t)
        (else (member? a (cdr l)))))

(define (set? l)
  (cond ((null? l) #t)
        ((member? (car l) (cdr l)) #f)
        (else (set? (cdr l)))))
        
(define (makeset lat)
  (cond ((null? lat) lat)
        ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
        (else (cons (car lat) (makeset (cdr lat))))))

(define (subset? s1 s2)
  (cond ((null? s1) #t)
        (else (and (member? (car s1) s2)
                   (subset? (cdr s1) s2)))))

(define (eqset? s1 s2)
  (and (subset? s1 s2) (subset? s2 s1)))

(define (intersect? s1 s2)
  (cond ((null? s1) #f)
        (else (or (member? (car s1) s2)
                  (intersect? (cdr s1) s2)))))

(define (intersect s1 s2)
  (cond ((null? s1) '())
        ((member? (car s1) s2) (makeset (cons (car s1) (intersect (cdr s1) s2))))
        (else (makeset (intersect (cdr s1) s2)))))


(define (union s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (cons (car s1) (cons (car s2) (union (cdr s1) (cdr s2)))))))

(define (intersectall set-l)
  (cond ((null? (cdr set-l)) (car set-l))
        (else (intersect (car set-l)
                         (intersectall (cdr set-l))))))

(intersectall '((6 pears and)
                (3 peaches and 6 persons)
                (peoples 6 and)
                (and 6)))

(define (fun? rel)
  (set? (firsts rel)))

(define (firsts rel)
  (cond ((null? rel) '())
        (else (cons (car (car rel)) (firsts (cdr rel))))))

(fun? '((1 2) (3 4) (99999 6)))
                           
(define (rev l)
 (define (inner ll rev-l)
   (cond ((null? ll) rev-l)
         (else (inner (cdr ll) (cons (car ll) rev-l)))))
  (inner l '()))

(define (first pair)
  (car pair))

(define (second pair)
  (car (cdr pair)))

(define (build s1 s2) (cons s1 (cons s2 '())))

(define (revrel2 rel)
  (cond ((null? rel) '())
        (else (cons (build (second (car rel))
                          (first (car rel)))
                    (revrel2 (cdr rel))))))

(define (revrel rel)
  (map rev rel))

(rev '(1 2 3 4 5))

(revrel '((1 2) (3 4) (5 6 7 9 10)))

(revrel2 '((1 2) (3 4) (5 6 7)))



(define (eql-c? a)
  (lambda (x) (eq? a x)))

((eql-c? 'salad) 'salad)

(define (rember-f test?)
  (lambda (a l)
    (cond ((null? l) '())
          ((test? (car l) a) (cdr l))
          (else (cons (car l) ((rember-f test?) a (cdr l)))))))
((rember-f =) 5 '(1 2 3 4 5 6 6 5))
((rember-f equal?) '(pop corn) '(lemonade (pop corn) and (cake)))


;(define (insert-L el a l)
;  (cond ((null? l) '())
;        ((eq? a (car l))
;         (cons el (cons a (cdr l))))
;        (else (cons (car l) (insert-L el a (cdr l))))))

(define (insertL-f test?)
  (lambda (el a l)
    (cond ((null? l) '())
          ((test? a (car l))
           (cons el (cons a (cdr l))))
          (else (cons (car l) ((insertL-f test?) el a (cdr l)))))))
((insertL-f =) 5 20 '(10 20 30 40))

(define (insertR-f test?)
  (lambda (new old l)
    (cond ((null? l) '())
          ((test? (car l) old)
           (cons old (cons new (cdr l))))
          (else (cons (car l) ((insertR-f test?) new old (cdr l)))))))

((insertR-f =) 5 20 '(10 20 30 40))

(define (multirember-f test?)
  (lambda (a l)
    (cond ((null? l) '())
          ((test? (car l) a)
           ((multirember-f test?) a (cdr l)))
          (else (cons (car l) ((multirember-f test?) a (cdr l)))))))
((multirember-f =) 5 '(1 2 3 4 5 6 6 5))

(define (multirember&co a lat col)
  (cond ((null? lat) (col '() '()))
        ((eq? (car lat) a)
         (multirember&co a (cdr lat)
                         (lambda (newlat seen)
                           (col newlat (cons (car lat) seen)))))
        (else 
         (multirember&co a (cdr lat)
                         (lambda (newlat seen)
                           (col (cons (car lat) newlat) seen))))))

(multirember&co 'tuna '(strawberries banana tuna tuna swordfish) (lambda (x y) (null? y)))


(define (multiinsertLR new oldL oldR lat)
  (cond ((null? lat) '())
        ((eq? (car lat) oldL)
         (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
        ((eq? (car lat) oldR)
         (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
        (else
         (cons (car lat) (multiinsertLR new oldL oldR (cdr lat))))))

(multiinsertLR 5 10 20 '(10 20 30 40 50))

(define (multiinsertLR&co new oldL oldR lat col)
  (cond ((null? lat) (col '() 0 0))
        ((eq? (car lat) oldL)
         (multiinsertLR&co new oldL oldR (cdr lat)
                           (lambda (newlat L R)
                             (col (cons new (cons oldL newlat))
                                  (add1 L) R))))
        ((eq? (car lat) oldR)
         (multiinsertLR&co new oldL oldR (cdr lat)
                           (lambda (newlat L R)
                             (col (cons oldR (cons new newlat))
                                  L (add1 R)))))
        (else
         (multiinsertLR&co new oldL oldR (cdr lat)
                           (lambda (newlat L R)
                             (col newlat L R))))))

(multiinsertLR&co 5 10 20
                  '(10 20 20 20 20 20 30 40 10 50)
                  (lambda (newlat L R)
                    (begin
                      (display L)
                      (newline)
                      (display R)
                      (newline)
                      (display newlat)
                      (newline))))

(define (evens-only* lat)
  (cond ((null? lat) '())
        ((atom? (car lat))
         (if (even? (car lat))
             (cons (car lat) (evens-only* (cdr lat)))
             (evens-only* (cdr lat))))
        (else
         (cons (evens-only* (car lat))
               (evens-only* (cdr lat))))))

(evens-only* '( 1 2 3 4 (5 6 ((7 8)) ((((9)))) 10)))

(define (evens-only*&co lat col)
  (cond ((null? lat)
         (col '() 0 1))
        ((atom? (car lat))
         (if (even? (car lat))
             (evens-only*&co (cdr lat)
                             (lambda (newlat sum prod)
                               (col (cons (car lat) newlat)
                                    sum (* (car lat) prod))))
             (evens-only*&co (cdr lat)
                             (lambda (newlat sum prod)
                               (col newlat (+ (car lat) sum) prod)))))
        (else
         (evens-only*&co (car lat) (lambda (newlat sum prod)
                                     (col newlat sum prod)))
         (evens-only*&co (cdr lat) (lambda (newlat sum prod)
                                     (col newlat sum prod))))))
;(evens-only*&co '(1 2 3 4 (5 6 ((7 8)) ((((9)))) 10))
;                (lambda (newlat sum prod)
;                  (display newlat)
;                  (newline)
;                  (display sum)
;                  (newline)
;                  (display prod)
;                  (newline)))
 
;; CHAPTER 10
(define new-entry build)

(define (lookup-in-entry name entry entry-f)
  (lookup-in-entry-help name (first entry) (second entry) entry-f))

(define (lookup-in-entry-help name names values entry-f)
  (cond ((null? names)
         (entry-f name))
        ((eq? (car names) name)
         (car values))
        (else
         (lookup-in-entry-help name (cdr names) (cdr values) entry-f))))

(define extend-table cons)

(define (lookup-in-table name table table-f)
  (cond ((null? table)
         (table-f name))
        (else
         (lookup-in-entry name (car table) (lambda (name)
                                             (lookup-in-table name (cdr table) table-f))))))

(define (atom-to-action e)
  (cond ((number? e) *const)
        ((eq? e #t) *const)
        ((eq? e #f) *const)
        ((eq? e (quote cons)) *const)
        ((eq? e (quote car)) *const)
        ((eq? e (quote cdr)) *const)
        ((eq? e (quote null?)) *const)
        ((eq? e (quote eq?)) *const)
        ((eq? e (quote atom?)) *const)
        ((eq? e (quote zero?)) *const)
        ((eq? e (quote add1)) *const)
        ((eq? e (quote sub1)) *const)
        ((eq? e (quote number?)) *const)
        (else *identifier)))

(define (list-to-action e)
  (cond ((atom? (car e))
         (cond ((eq? (car e) (quote quote)) *quote)
               ((eq? (car e) (quote lambda)) *lambda)
               ((eq? (car e) (quote cond)) *cond)
               (else *application)))
        (else *application)))

(define (expression-to-action e)
  (cond ((atom? e) (atom-to-action e))
        (else (list-to-action e))))

(define (value e)
  (meaning e '()))

(define (meaning e table)
  ((expression-to-action e) e table))

(define (*const e table)
  (cond ((number? e) e)
        ((or (eq? #t e)
             (eq? #f e)) e)
        (else (build (quote primitive) e))))

(define (*quote e table)
  (text-of e))

(define text-of second)

(define (*identifier e table)
  (lookup-in-table e table initial-table))

(define (initial-table name)
  (car (quote ())))

(define (*lambda e table)
  (build (quote non-primitive)
         (cons table (cdr e))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define (evcon lines table)
  (cond ((else? (question-of lines))
         (meaning (answer-of lines) table))
        ((meaning (question-of (car lines)) table)
         (meaning (answer-of (car lines)) table))
        (else (evcon (cdr lines) table))))

(define (else? x)
  (cond ((atom? x) (eq? x (quote else)))
        (else #f)))

(define question-of first)
(define answer-of second)

(define (*cond e table)
  (evcon (cond-lines-of  e) table))

(define cond-lines-of cdr)

(define (evlis args table)
  (cond ((null? args) (quote ()))
        (else
         (cons (meaning (car args) table)
               (evlis (cdr args) table)))))
(define (*application e table)
  (apply (meaning (function-of e) table)
         (evlis (arguments-of e) table)))

(define function-of car)
(define arguments-of cdr)

(define (primitive? l)
  (eq? (first l) (quote primitive)))

(define (non-primitive? l)
  (eq? (first l) (quote non-primitive)))

(define (apply fun vals)
  (cond ((primitive? fun)
         (apply-primitive (second fun) vals))
        ((non-primitive? fun)
         (apply-closure (second fun) vals))))

(define (apply-primitive name vals)
  (cond ((eq? name (quote cons))
         (cons (first vals) (second vals)))
        ((eq? name (quote car))
         (car (first vals)))
        ((eq? name (quote cdr))
         (cdr (first vals)))
        ((eq? name (quote null?))
         (null? (first vals)))
        ((eq? name (quote eq?))
         (eq? (first vals) (second vals)))
        ((eq? name (quote atom?))
         (myatom? (first vals)))
        ((eq? name (quote zero?))
         (zero? (first vals)))
        ((eq? name (quote add1))
         (add1 (first vals)))
        ((eq? name (quote sub1))
         (sub1 (first vals)))
        ((eq? name (quote number?))
         (number? (first vals)))))

(define (myatom? x)
  (cond ((atom? x) #t)
        ((null? x) #f)
        ((or (eq? (car x) (quote primitive))
             (eq? (car x) (quote non-primitive)))
         #t)
        (else #f)))

(define (apply-closure closure vals)
  (meaning (body-of closure)
           (extend-table (new-entry (formals-of closure) vals)
                         (table-of closure))))
              
(meaning '(cons z x) '(((x y) ((a b c) (d e f)))
                     ((x y z) (4 5 6))))
                   













                
                                
                
