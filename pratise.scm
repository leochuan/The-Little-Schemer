(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
          (member? a (cdr lat)))))))

(define rember
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((equal? a (car l)) (cdr l))
      (else (cons (car l) (rember a (cdr l)))))))

(define firsts
  (lambda (l)
    (cond 
      ((null? l) (quote ()))
      (else (cons (car (car l)) (firsts (cdr l)))))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((eq? old (car l)) (seq new old (cdr l)))
        (else (cons? (car l) ((insert-g seq) new old l)))))))

(define insertR
  (insert-g
    (lambda (new old l)
      (cons old (cons new l)))))

(define insertL
  (insert-g
    (lambda (new old l)
      (cons new (cons old l)))))

(define subst
  (insert-g
    (lambda (new old l)
      (cons new l))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond 
      ((null? lat) (quote ()))
      ((or (eq? o1 (car lat)) (eq? o2 (car lat))) (cons new (cdr lat)))
      (else (cons (car lat) (subset2 new o1 o2 (cdr lat)))))))

(define multirember
  (lambda (a lat) 
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(define multirember-f
  (lambda (method)
    (lambda (a lat)
      (cond
        ((null? lat) (quote ()))
        ((method a (car lat)) (multirember a (cdr lat)))
        (else (cons (car lat) (multirember a (cdr lat))))))))

(define multiremberT
  (lambda (method lat)
    (cond
      ((null? lat) (quote ()))
      ((method (cdr lat)) (multiremberT method (cdr lat)))
      (else (cons (car lat) (multiremberT method (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) old) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col (quote ()) 0 0))
      ((eq? (car lat) oldL)
        (multiinsertLR&co new oldL oldR (cdr lat) 
          (lambda (newlat L R)
            (col (cons new (cons oldL newlat)) (add1 L) R))))
      ((eq? (car lat) oldR)
        (multiinsertLR&co new oldL oldR (cdr lat)
          (lambda (newlat L R)
            (col (cons oldR (cons new newlat)) L (add1 R)))))
      (else
        (multiinsertLR&co new oldL oldR (cdr lat)
          (lambda (newlat L R)
            (col (cons (car lat) newlat) L R)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))

(define add1 
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond
      ((not (zero? n)) (o+ (sub1 n) (add1 m)))
      (else m))))

(define o-
  (lambda (n m)
    (cond 
      ((zero? m) n)
      (else (o- (sub1 n) (sub1 m))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(define o*
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (o* n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond 
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define o>
  (lambda (m n)
    (cond 
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o> (sub1 m) (sub1 n))))))

(define o<
  (lambda (m n)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o< (sub1 m) (sub1 n))))))

(define o=
  (lambda (m n)
    (cond
      ((o< m n) #f)
      ((o> m n) #f)
      (else #t))))

(define power
  (lambda (m n)
    (cond
      ((zero? n) 1)
      (else (o* m (power m (sub1 n)))))))

(define o/
  (lambda (m n)
    (cond
      ((o< m n) 0)
      (else (add1 (o/ (o- m n) n))))))

(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (q1 q2)
    (cond
      ((number? q1) (and (number? q2) (o= q1 q2)))
      (else (and (not (number? q2)) (eq? q1 q2))))))

(define occur
  (lambda (n lat)
    (cond
      ((null? lat) 0)
      ((eqan? n (car lat)) (add1 (occur n (cdr lat))))
      (else (occur n (cdr lat))))))

(define one?
  (lambda (n)
    (= n 1)))

(define rember*
  (lambda (n l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) 
       (cond
         ((eq? n (car l)) (rember* n (cdr l)))
         (else (cons (car l) (rember* n (cdr l))))))
      (else (cons (rember* n (car l)) (rember* n (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
  (lambda (n l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? n (car l)) (add1 (occur* n (cdr l))))
         (else (occur* n (cdr l)))))
      (else (o+ (occur* n (car l)) (occur* n (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) 
       (cond
         ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (cond
         ((eq? a (car l)) #t)
         (else (member* a (cdr l)))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2) #f))
      (else (and (equal? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(define first-sub-exp
  (lambda (aexp) (car (cdr aexp))))

(define second-sub-exp
  (lambda (aexp) (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp) (car aexp)))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x (quote +)) o+)
      ((eq? x (quote *)) o*)
      (else power))))

(define value
  (lambda (aexp)
    (cond
      ((atom? aexp) aexp)
      (else
        ((atom-to-function (operator aexp))
          (value (first-sub-exp aexp))
          (value (second-sub-exp aexp)))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
      (else (cons (car lat) (makeset (cdr lat)))))))

(define makeset2
  (lambda (lat)
    (cond
      ((null? lat) (quote ()))
      (else (cons (car lat) (makeset2 (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      ((member? (car set1) set2) (subset? (cdr set1) set2))
      (else #f))))

(define subset2?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2) (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 (quote ())))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) (quote ()))
      (else (cons (revpair (car rel)) (revrel (cdr rel)))))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      (else (cons (car (cdr (car l))) (seconds (cdr l)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(define rember-f
  (lambda (method)
    (lambda (a l)
      (cond
        ((null? l) (quote ()))
        ((method a (car l)) (cdr l))
        (else (cons (car l) ((rember-f method) a (cdr l))))))))

(define insertL-f
  (lambda (method)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((method old (car l)) (cons new l))
        (else (cons (car l) ((insertL-f method) new old (cdr l))))))))

(define insertR-f
  (lambda (method)
    (lambda (new old l)
      (cond
        ((null? l) (quote ()))
        ((method old (car l)) (cons old (cons new (cdr l))))
        (else (cons (car l) ((insertR-f method) new old (cdr l))))))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
        (cond
          ((even? (car l)) (cons (car l) (evens-only* (cdr l))))
          (else (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col (quote ()) 1 0))
      ((atom? (car l))
        (cond
          ((even? (car l))
            (evens-only*&co (cdr l)
              (lambda (newlat p s)
                (col (cons (car l) newlat) (o* p (car l)) s))))
          (else
            (evens-only*&co (cdr l)
              (lambda (newlat p s)
                (col newlat p (o+ s (car l))))))))
      (else (evens-only*&co (car l)
        (lambda (al ap as)
          (evens-only*&co (cdr l)
            (lambda (dl dp ds)
              (col (cons al dl) (o* dp ap) (o+ ds as))))))))))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? a sorn)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(define shift
  (lambda (pair)
    (build (first (first pair)) (build (second (first pair)) (second pair)))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
        (align (shift pora)))
      (else (build (first pora) (align (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (o+ (o* (weight* (first pora)) 2) (weight* (second pora)))))))

(define new-entry
  (lambda (keySet valueSet)
    (cons keySet (cons valueSet (quote ())))))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
      (first entry)
      (second entry)
      entry-f)))

(define extend-table
  (lambda (entry table)
    (cons entry table)))

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else (lookup-in-entry-help name (car table) 
                (lambda (name)
                  (lookup-in-table name (cdr table) table-f)))))))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (second e)))

(define initial-table
  (lambda (name)
    (car (quote ()))))

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
      (cons table (cdr e)))))

(define table-of first)

(define formals-of second)

(define body-of
  (lambda (table)
    (car (cdr (cdr table)))))

(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines))))
      ((meaning (question-of (car lines))
         table)
       (meaning (answer-of (car lines))
         table))
     (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
      (else #f))))

(define question-of first)

(define question-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else
        (cons (meaning (car args) table)
          (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (apply
      (meaning (function-of e) table)
      (evlis (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun)
        (apply-primitive
          (second fun) vals))
      ((non-primitive? fun)
        (apply-closure
          (second fun) vals)))))

(define primitive?
  (lambda (fun)
    (eq? (quote primitive) (first fun))))

(define non-primitive?
  (lambda (fun)
    (eq? (quote non-primitive) (first fun))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
      (extend-table
        (new-entry
          (formals-of closure)
           vals)
        (table-of closure)))))

(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
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
        (atom? (first vals)))
      ((eq? name (quote zero?))
        (zero? (first vals)))
      ((eq? name (quote add1))
        (add1 (first vals)))
      ((eq? name (quote sub1))
        (sub1 (first vals)))
      ((eq? name (quote number?))
        (number? (first vals))))))

(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? #f e) *const)
      ((eq? #t e) *const)
      ((eq? (quote cons) e) *const)
      ((eq? (quote car) e) *const)
      ((eq? (quote cdr) e) *const)
      ((eq? (quote null?) e) *const)
      ((eq? (quote eq?) e) *const)
      ((eq? (quote atom?) e) *const)
      ((eq? (quote zero?) e) *const)
      ((eq? (quote add1) e) *const)
      ((eq? (quote sub1) e) *const)
      ((eq? (quote number?) e) *const)
      (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
        (cond
          ((eq? (car e) (quote quote)) *quote)
          ((eq? (car e) (quote lambda)) *lambda)
          ((eq? (car e) (quote cond)) *cond)
          (else *application)))
      (else *application))))

(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define meaning
  (lambda (e table)
      ((expression-to-action e) e table)))

(define value*
  (lambda (e)
    (meaning e (quote ()))))

