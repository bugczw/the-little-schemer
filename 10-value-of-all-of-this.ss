#lang scheme
; Chapter 10 of The Little Schemer:
; What Is the Value of All This?
;
; Code examples assemled by Peteris Krumins (peter@catonmat.net).
; His blog is at http://www.catonmat.net  --  good coders code, great reuse.
;
; Get yourself this wonderful book at Amazon: http://bit.ly/4GjWdP
;

; We'll need atom?
;
(define atom?
 (lambda (x)
    (and (not (pair? x)) (not (null? x)))))  
  
; An entry is a pair of lists whose first list is a set. The two lists must be
; of equal length.
; Here are some entry examples.
;
'((appetizer entree bevarage)
  (pate boeuf vin))
'((appetizer entree bevarage)
  (beer beer beer))
'((bevarage dessert)
  ((food is) (number one with us)))

; Let's build entries with build from chapter 7 (07-friends-and-relations.ss)
;
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define new-entry build)

; Test it out and build the example entries above
;
(build '(appetizer entree bevarage)
       '(pate boeuf vin))
(build '(appetizer entree bevarage)
       '(beer beer beer))
(build '(bevarage dessert)
       '((food is) (number one with us)))

; We'll need first and second functions from chapter 7
;
(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

; And also third, later.
;
(define third
  (lambda (l)
    (car (cdr (cdr l)))))

; The lookup-in-entry function looks in an entry to find the value by name
; entry-f will be invoked when name is not found in the first list of entry.
; entry-f can be use as transfer function and recursion.
;
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help
      name
      (first entry)
      (second entry)
      entry-f)))

; lookup-in-entry uses lookup-in-entry-help helper function
;
(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else
        (lookup-in-entry-help
          name
          (cdr names)
          (cdr values)
          entry-f)))))

; Let's try out lookup-in-entry
;
(lookup-in-entry
  'entree
  '((appetizer entree bevarage) (pate boeuf vin))
  (lambda (n) '()))
; ==> 'boeuf

(lookup-in-entry
  'no-such-item
  '((appetizer entree bevarage) (pate boeuf vin))
  (lambda (n) '()))
; ==> '()

; A table (also called an environment) is a list of entries. Here are some
; examples.
;
'()
'(((appetizer entree beverage) (pate boeuf vin))
  ((beverage dessert) ((food is) (number one with us))))

; The extend-table function takes an entry and a table and adds entry to the
; table
;
(define extend-table cons)

; lookup-in-table finds an entry in a table
;
(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
        (lookup-in-entry
          name
          (car table)
          (lambda (name)
             (lookup-in-table
              name
              (cdr table)
              table-f)))))))

; Let's try lookup-in-table
;
(lookup-in-table
  'beverage
  '(((entree dessert) (spaghetti spumoni))
    ((appetizer entree beverage) (food tastes good)))
  (lambda (n) '()))
; ==> 'good


(cons 'a
    (cons 'b
        (cons 'c
		    (quote()))))
'(a b c)
(quote(a b c))
; => (a b c)

; There are 6 types:
; *const   *quote   *identifier
; *lambda   *cond   *application
; Expressions to actions
;
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else
        (list-to-action e)))))

; Atom to action
;
(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))

; List to action
;
(define list-to-action
  (lambda (e)
    (cond
      ((atom? (car e))
       (cond
         ((eq? (car e) 'quote) *quote)
         ((eq? (car e) 'lambda) *lambda)
         ((eq? (car e) 'cond) *cond)
         (else *application)))
      (else *application))))

; The value function takes an expression and evaulates it
;
(define value
  (lambda (e)
    (meaning e '())))

; The meaning function translates an expression to its meaning
;
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

; Now the various actions. Let's start with *const
; We know what primitives do;non-primitives are defined
; by their arguments and their function body

; *const
;
(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else
        (build 'primitive e)))))

; *quote: (quote text)
;
(define *quote
  (lambda (e table)
    (text-of e)))

; text-of
;
(define text-of second)

; *identifier
;
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

; initial-table
;
(define initial-table
  (lambda (name)
    (car '())))    ; let's hope we don't take this path

; *lambda
;
(define *lambda
  (lambda (e table)
    (build 'non-primitive
           (cons table (cdr e)))))

; example of implement *lambda
;
(*lambda '(lambda(x)(cons x y)) '(((y z)((8) 9))))	   
; => (non-primitive ((((y z) ((8) 9))) (x) (cons x y)))		   

; Let's add helper functions
;
(define table-of first)
(define formals-of second)
(define body-of third)

; cond takes lines, and returns the value for the first true line
;
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else
        (evcon (cdr lines) table)))))    ; we don't ask null?, better one of cond lines be true!

; evcon needs else?, question-of and answer-of
;
(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x 'else))
      (else #f))))

(define question-of first)
(define answer-of second)

; Now we can 4write the real *cond
;
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

; evlis finds meaning of arguments
;
(define evlis
  (lambda (args table)
    (cond
      ((null? args) '())
      (else
        (cons (meaning (car args) table)
              (evlis (cdr args) table))))))

; Finally the *application
;
(define *application
  (lambda (e table)
    (applyz
      (meaning (function-of e) table)
      (evlis (arguments-of e) table))))
	  
	  
; The test of expression-to-action
;
(expression-to-action
    ((lambda(nothing)
	    (cond
		    (nothing (quote somthing))
			(else (quote nothing))))
	    #t))	  
; => #<procedure:*identifier>  
	  
(define function-of car)
(define arguments-of cdr)

; Is the function a primitive?
;
(define primitive?
  (lambda (l)
    (eq? (first l) 'primitive)))

; Is the function a non-primitive?
;
(define non-primitive?
  (lambda (l)
    (eq? (first l) 'non-primitive)))

; Apply!
;
(define applyz
  (lambda (fun vals)
    (cond
      ((primitive? fun)
       (apply-primitive (second fun) vals))
      ((non-primitive? fun)
       (apply-closure (second fun) vals)))))

; apply-primitive
;
(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name 'cons)
       (cons (first vals) (second vals)))
      ((eq? name 'car)
       (car (first vals)))
      ((eq? name 'cdr)
       (cdr (first vals)))
      ((eq? name 'null?)
       (null? (first vals)))
      ((eq? name 'eq?)
       (eq? (first vals) (second vals)))
      ((eq? name 'atom?)
       (:atom? (first vals)))
      ((eq? name 'zero?)
       (zero? (first vals)))
      ((eq? name 'add1)
       (+ 1 (first vals)))
      ((eq? name 'sub1)
       (- 1 (first vals)))
      ((eq? name 'number?)
       (number? (first vals))))))

; :atom?
;
(define :atom?
  (lambda (x)
    (cond
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) 'primitive) #t)
      ((eq? (car x) 'non-primitive) #t)
      (else #f))))

; apply-closure
;
(define apply-closure
  (lambda (closure vals)
    (meaning
      (body-of closure)
      (extend-table (new-entry
                      (formals-of closure)
                      vals)
                    (table-of closure)))))

; test apply-closure
;
(apply-closure
    '((((u v w) (1 2 3)) ((x y z) (4 5 6)))
	  (x y) (cons z x)) 
	'((a b c)(d e f)))
; '(cons z x)
; '(((x y) ((a b c) (d e f))) ((u v w)(1 2 3)) ((x y z) (4 5 6)))
; => (6 a b c)


					
					
;
; Let's try out our brand new Scheme interpreter!
;

(value '(add1 6))                           ; 7
(value '(quote (a b c)))                    ; '(a b c)
(value '(car (quote (a b c))))              ; 'a
(value '(cdr (quote (a b c))))              ; '(b c)
(value
  '((lambda (x)
      (cons x (quote ())))
    (quote (foo bar baz))))                 ; '((foo bar baz))
(value
  '((lambda (x)
      (cond
        (x (quote true))
        (else
          (quote false))))
    #t))                                    ; 'true


;
; Go get yourself this wonderful book and have fun with these examples!
;
; Shortened URL to the book at Amazon.com: http://bit.ly/4GjWdP
;
; Sincerely,
; Peteris Krumins
; http://www.catonmat.net
;

