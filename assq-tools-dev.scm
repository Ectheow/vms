(library (assq-tools)
  (export acons-recursive)
  (import (rnrs))

  ;; complex values:
  ;; keylist - a list-of-atoms
  ;; alol - a alist-of-alists
  ;;
  ;; keylist
  ;; 1. '()
  ;; 2. (cons atom keylist) where atom is an atom and keylist is a keylist.
  ;;
  ;; alist-of-alists 
  ;; 1. '()
  ;; 2. (cons alist alol) where alol is an alist-of-alists.
  ;;
  ;; alist
  ;; 1. '()
  ;; 2. (cons alist-elem alist) where alist-elem is an alist-element and alist is an alist.
  ;;
  ;; alist-elem
  ;; (cons key value) where key and value are anything.
n  ;;
  ;; possible combinations
  ;; |                | cons? keylist  |  empty? keylist  |
  ;; | cons?  alol    | and cons? keylist cons? alol | and empty? keylist cons? alol |
  ;; | empty? alol    | and cons? keylist empty? alol| and empty? keylist empty? alol|
  ;;
  ;; examples:
  ;; (acons-recursive '() '() (one . 2))
  ;; '((one . 2))
  ;; (acons-recursive '(one two) '() (three . 2))
  ;; (error)
  ;; (acons-recursive '(one two three)
  ;;                    '( (one . (two . ()))) 4)
  ;; '((one . (two . (three . 4))))
  ;; (acons-recursive '(one two three)
  ;;                  '( (one . (two . (three . 4)))) (4)
  ;; (error)
  ;; (acons-recursive '(one two three)
  ;;                  '( ( one . ( (two . ( (three . ())))))) (cons 'five 'six))
  ;; (cons (cons 'one 
  ;; (acons-recursive '(two three) '( (two . ( (three . ())))) (cons 'five 'six)))
  ;;

  #|
template:
;; assq-recursive : keylist alist-of-alists -> boolean
;; returns if a list of keys is in the alist-of-alists.
(define (assq-recursive keylist alol) ...)

data definitions
alist:
1. '()
2. (cons pair alist) where pair is a pair and alist is an alist.

alist-of-alists:
1. '()
2. (cons (cons key alist1) alist2) where alist1 and alist2 are alist-of-alists.
3. (cons (cons key noa) alist2) where noa is not an alist - i.e. it
doesn't match the form (cons (cons any any) alist).
( ( one . ( ( two . (1 2 3)))))


keylist:
1. (cons k '()) where k is some value
2. (cons k keylist) where k is some value and keylist is a keylist.

Template for something processing an alist-of-alists and a keylist:

|#
(define (alist? alist)
  (and (pair? alist)
       (pair? (car alist))))
#|
|      
| (null? alol)
| (and (pair? alol) (pair? (car alol))
|                   (pair? (cdr (car alol)))
|                   (pair? (car (cdr (car alol)))))
| (and (pair? alol) (pair? (car alol))
|                    
|                   (or (not (pair? (cdr (car alol))))
|                       (not (pair? (car (cdr (car alol))))))

in other words:
|                                                     | (null? (cdr keylist))                                                            | (not (null? (cdr keylist)))
| (null?  alol)                                       | (and (null? alol) (null? (cdr keylist)))                                         | (and (null? alol) (not (null? (cdr keylist))))
| (and (alist? alol) (alist? (cdr (car alol))))       | (and (and (alist? alol) (alist? (cdr (car alol)))) (null? (cdr keylist)))        | (and (and (alist? alol) (alist? (cdr (car alol)))) (not (null? (cdr keylist))))
| (and (alist? alol) (not (alist? (cdr (car alol))))) | (and (and (alist? alol) (not (alist? (cdr (car alol)))) (null? (cdr keylist))))  | (and (and (alist? alol) (not (alist? (cdr (car alol))))) (not (null? (cdr keylist))))


examples:
(equal? (assq-recursive '(one) '()) ;; null alol, first-clause keylist
        #f)
(equal? (assq-recursive '(one two) '()) ;; nul alol, second-clause keylist
	#f)
(equal?
 (assq-recursive '(one) '( (one . ( (one . two)))))
 '(( one . two )))
(equal?
 (assq-recursive '(one two) '( (one . ( (two . two)))))
 '(two . two))
(equal?
 (assq-recursive '(one two) '())
 #f)
(equal?
 (assq-recursive '(one) '( ( one . two)))
 '(one . two))


Template:
(define (assq-recursive keylist alol)
  (cond
   ((and (null? (cdr keylist))
	 (null? alol))
    ... (car keylist) ...)
   ((and (null? alol)
	 (not (null? (cdr keylist))))
    ... (car keylist) ...
    ... (cdr keylist) ...)
   ((and (alist? alol)
	 (alist? (cdr (car alol)))
	 (null? (cdr keylist)))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (assq-recursive keylist (cdr (car alol))) ...
    ... (assq-recursive keylist (cdr alol)) ...)
   ((and (alist? alol)
	 (alist? (cdr (car alol)))
	 (not (null? (cdr keylist))))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (cdr (car alol)) ...
    ... (assq-recursive keylist (cdr alol)) ...
    ... (assq-recursive (cdr keylist) (cdr alol)) ...
    ... (assq-recursive (cdr keylist) alol) ...
    ... (assq-recursive (cdr keylist) (cdr (car alol)))...
    ... (assq-recursive keylist (cdr (car alol))) ...)
   ((and (alist? alol)
	 (not (alist? (cdr (car alol))))
	 (null? (cdr keylist)))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (assq-recursive keylist (cdr alol)) ...)
   ((and (alist? alol)
	 (not (alist? (cdr (car alol))))
	 (not (null? (cdr keylist))))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (assq-recursive keylist (cdr alol)) ...
    ... (assq-recursive (cdr keylist) (cdr alol)) ...
    ... (assq-recursive (cdr keylist) alol) ...)))

The answers to the first two quetsions, when alol is null, should be falase.
(define (assq-recursive keylist alol)
  (cond
   ((and (null? (cdr keylist))
	 (null? alol)) #f)
   ((and (null? alol)
	 (not (null? (cdr keylist)))) #f)
   ((and (alist? alol)
	 (alist? (cdr (car alol)))
	 (null? (cdr keylist)))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (assq-recursive keylist (cdr (car alol))) ...
    ... (assq-recursive keylist (cdr alol)) ...)
   ((and (alist? alol)
	 (alist? (cdr (car alol)))
	 (not (null? (cdr keylist))))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (cdr (car alol)) ...
    ... (assq-recursive keylist (cdr alol)) ...
    ... (assq-recursive (cdr keylist) (cdr alol)) ...
    ... (assq-recursive (cdr keylist) alol) ...
    ... (assq-recursive (cdr keylist) (cdr (car alol)))...
    ... (assq-recursive keylist (cdr (car alol))) ...)
   ((and (alist? alol)
	 (not (alist? (cdr (car alol))))
	 (null? (cdr keylist)))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (assq-recursive keylist (cdr alol)) ...)
   ((and (alist? alol)
	 (not (alist? (cdr (car alol))))
	 (not (null? (cdr keylist))))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (assq-recursive keylist (cdr alol)) ...
    ... (assq-recursive (cdr keylist) (cdr alol)) ...
    ... (assq-recursive (cdr keylist) alol) ...)))

For the third, we examine some examples, one where the key in the
keylist is the same as the first element of the alist, and one where
it isn't.

(equal? (assq-recursive '(one) '( (one . ( (two . three)))))
	'( (two . three)))
(equal? (assq-recursive '(one) '( (two . ( (three . four ))) (one . (
								     (two
								      . three)))))
	'( (two . three)))
(equal? (assq-recursive '(one) '( (two . ( ()))))
	#f)
So, there are two conditions:
1. the car of the keylist is equal to the key of the first item in the
alist - if so, return that item.
2. it isn't, so we should search the rest of this alist, but not the
value of this alist since it's key doesn't match.

(define (assq-recursive keylist alol)
  (cond
   ((and (null? (cdr keylist))
	 (null? alol)) #f)
   ((and (null? alol)
	 (not (null? (cdr keylist)))) #f)
   ((and (alist? alol)
	 (alist? (cdr (car alol)))
	 (null? (cdr keylist)))
    (cond
     ((equal? (car keylist) (car (car alol)))
      (car alol))
     (else
      (assq-recursive keylist (cdr alol)))))
   ((and (alist? alol)
	 (alist? (cdr (car alol)))
	 (not (null? (cdr keylist))))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (cdr (car alol)) ...
    ... (assq-recursive keylist (cdr alol)) ...
    ... (assq-recursive (cdr keylist) (cdr alol)) ...
    ... (assq-recursive (cdr keylist) alol) ...
    ... (assq-recursive (cdr keylist) (cdr (car alol)))...
    ... (assq-recursive keylist (cdr (car alol))) ...)
   ((and (alist? alol)
	 (not (alist? (cdr (car alol))))
	 (null? (cdr keylist)))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (assq-recursive keylist (cdr alol)) ...)
   ((and (alist? alol)
	 (not (alist? (cdr (car alol))))
	 (not (null? (cdr keylist))))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (assq-recursive keylist (cdr alol)) ...
    ... (assq-recursive (cdr keylist) (cdr alol)) ...
    ... (assq-recursive (cdr keylist) alol) ...)))

Next, we have a case where
1. it is a true alist of alists.
2. we are not on the last element of the keylist.

(equal? (assq-recursive '(one two) '( (one . ( (two . three)))))
	'(two . three))
(equal? (assq-recursive '(one two) '( (one . ( (two . ( (three . (
								  (four
								   . ())))))))))
	'(two . ( (three . ( (four . ()))))))
(equal? (assq-recursive '(one two) '( (four . ( (five . ())))
				      (one . ( (two . ( (three
							 . 4)))))))
	'( two . ( (three . 4))))
(equal? (assq-recursive '(one two) '( (four . ( (five . ()))) (one
							       . two)))
	#f)
Cases:
1. The first element of the keylist matches the key of the first alist
item. In this case we should recurse into this list, and, if we get
something return that, if not, recurse onto the rest of the top-level
alist.
2. The first element of the keylist does not match the key of the
first alist item. We should recurse into the rest of the top level of
the alist, not taking away from the keylist.

(define (assq-recursive keylist alol)
  (cond
   ((and (null? (cdr keylist))
	 (null? alol)) #f)
   ((and (null? alol)
	 (not (null? (cdr keylist)))) #f)
   ((and (alist? alol)
	 (alist? (cdr (car alol)))
	 (null? (cdr keylist)))
    (cond
     ((equal? (car keylist) (car (car alol)))
      (car alol))
     (else
      (assq-recursive keylist (cdr alol)))))
   ((and (alist? alol)
	 (alist? (cdr (car alol)))
	 (not (null? (cdr keylist))))
    (cond
     ((equal? (car keylist)
	      (car (car alol)))
      (let ((result-from-value
	     (assq-recursive (cdr keylist)
			     (cdr (car alol)))))
	(cond
	 ((eq? #f result-from-value)
	  (assq-recursive keylist (cdr alol)))
	 (else
	  result-from-value))))
     (else
      (assq-recursive keylist (cdr alol)))))
   ((and (alist? alol)
	 (not (alist? (cdr (car alol))))
	 (null? (cdr keylist)))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (assq-recursive keylist (cdr alol)) ...)
   ((and (alist? alol)
	 (not (alist? (cdr (car alol))))
	 (not (null? (cdr keylist))))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (assq-recursive keylist (cdr alol)) ...
    ... (assq-recursive (cdr keylist) (cdr alol)) ...
    ... (assq-recursive (cdr keylist) alol) ...)))
 
In other words, we should 
Notice there are lots of selector expressions for the alol, which has
three items in it's data definition  when it's not null.

There are several possible natural recursions for the last two
clauses:

(assq-recursive keylist (cdr alol))
(assq-recursive keylist (cdr (car alol)))
(assq-recursive (cdr keylist) (cdr alol))
(assq-recursive (cdr keylist) (cdr (car alol)))
(assq-recursive (cdr keylist) alol)

examples -- there are two possible types of input besides their shape
- one where the keypath is  in the first alist-of-alists, the second
where it is in the rest of the list somewhere, and we ought to search
the rest of the list.
1. - the correct keypath is in the first.
2. - it is not, but it is in the rest
3. - a partial match of the keypath is in the first element.
4. - it is not anywhere


1. 
(assq-recursive '(one two) '( (one . ( (two . three)))))
1. (assq-recursive '(one two) '()) -- not right, assuming even the rest of the algorithm works.
2. (assq-recursive '(one two) '( (two . three) )) -- not right, because we should take the cdr of keylist, this will not return the right result.
3. (assq-recursive '(two) '()) -- will return the incorrect result, #f, since alol is null. 
4. (assq-recursive '(two) '( (two . three) )) -- right! -- in this
case we should do (assq-recursive (cdr keylist) (cdr (car alol))). 
5. (assq-recursive '(two) '( (one . ( (two . three))))) -- makes no sense and will return the incorrect result.


2. 
(assq-recursive '(one two) '( (four . ( (five . six))) (one . ( (two . three))))))
1. (assq-recursive '(one two) '( (one . ( (two . three))))) -- right, assuming the rest of the algorithm works.
2. (assq-recursive '(two) '( (one . ( two . three)))) -- not right
3. (assq-recursive '(two) '()) -- not right
4. (assq-recursive '(two) '( (five . six) ) -- not right
5. (assq-recursive '(two) '( (four . ( (five . six))) (one . ( (two
								. three)))))
-- not right - we'll return false.

3.
(assq-recursive '(one two) '( (one . ( (four . six))) (one . ( (two
								. three)))))
1. (assq-recursive '(one two) '( (one . ( (two . three))))) -- right,
but be suspicous about this one.
2. (assq-recursive '(one two) '( (four . six))) -- not right, will
return false.
3. (assq-recursive '(two) '( (one . ( (two . three))))) -- not right,
we cdr'ed the keylist too early.
4. (assq-recursive '(two) '( (four . six))) -- not right
5. (assq-recursive '(two) '( (one . ( ( four . six))) (one . ( (two
								. three)))))
-- wrong, we've lost the first element, which will keep us from
recursing properly.

So, since we know the data definition matches a particular shape but
not either 1. 2. 3. or 4., we need to try all possible correct
recursions, and return the first true recursion. Each has a possible
correct recursion:

1. (assq-recursive '(two) '( (two . three) )) -- (assq-recursive (cdr
								  keylist)
								 (cdr
								  (car alol)))
2. (assq-recursive '(one two) '( (one . ( (two . three))))) --
(assq-recursive keylist (cdr alol))
3. (assq-recursive '(one two) '( (one . ( (two . three))))) -- ditto

(cond
 ((equal? (car keylist) (car (car alol)))
  (let ((found-in-value (assq-recursive (cdr keylist) (cdr (car alol)))))
    (cond
     ((not (eq? #f found-in-value)) found-in-value)
     (else (assq-recursive keylist (cdr alol))))))
 (else (assq-recursive (cdr alol))))



(define (assq-recursive keylist alol)
  (cond
   ((and (null? (cdr keylist))
	 (null? alol)) #f)
   ((and (null? alol)
	 (not (null? (cdr keylist)))) #f)
   ((and (alist? alol)
	 (alist? (cdr (car alol)))
	 (null? (cdr keylist)))
    (cond
     ((equal? (car keylist) (car (car alol)))
      (car alol))
     (else
      (assq-recursive keylist (cdr alol)))))
   ((and (alist? alol)
	 (alist? (cdr (car alol)))
	 (not (null? (cdr keylist))))
    (cond
     ((equal? (car keylist)
	      (car (car alol)))
      (let ((result-from-value
	     (assq-recursive (cdr keylist)
			     (cdr (car alol)))))
	(cond
	 ((eq? #f result-from-value)
	  (assq-recursive keylist (cdr alol)))
	 (else
	  result-from-value))))
     (else
      (assq-recursive keylist (cdr alol)))))
   ((and (alist? alol)
	 (not (alist? (cdr (car alol))))
	 (null? (cdr keylist)))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (assq-recursive keylist (cdr alol)) ...)
   ((and (alist? alol)
	 (not (alist? (cdr (car alol))))
	 (not (null? (cdr keylist))))
    (cond
     ((equal? (car keylist) (car (car alol)))
      (let ((found-in-value (assq-recursive (cdr keylist) (cdr (car alol)))))
	(cond
	 ((not (eq? #f found-in-value)) found-in-value)
	 (else (assq-recursive keylist (cdr alol))))))
     (else (assq-recursive (cdr alol)))))))

In the final clause - or rather second-to-last - we gan see that it is
in fact a sort of sub-case of the last one where (cdr keylist) is not
a valid selector item so we can't recurse that way. It is also a
possbly terminal case since the car of keylist may equal to the car of
car of alol.

(cond
 ((equal? (car keylist) (car (car alol))) (car alol))
 (else
  ....))
For the else clause, we can just use the only valid recursion left
over from the more general case: (assq-recursive keylist (cdr
							  alol)).
The others are not valid if they contain cdr keylist.

(define (assq-recursive keylist alol)
  (cond
   ((and (null? (cdr keylist))
	 (null? alol)) #f)
   ((and (null? alol)
	 (not (null? (cdr keylist)))) #f)
   ((and (alist? alol)
	 (alist? (cdr (car alol)))
	 (null? (cdr keylist)))
    (cond
     ((equal? (car keylist) (car (car alol)))
      (car alol))
     (else
      (assq-recursive keylist (cdr alol)))))
   ((and (alist? alol)
	 (alist? (cdr (car alol)))
	 (not (null? (cdr keylist))))
    (cond
     ((equal? (car keylist)
	      (car (car alol)))
      (let ((result-from-value
	     (assq-recursive (cdr keylist)
			     (cdr (car alol)))))
	(cond
	 ((eq? #f result-from-value)
	  (assq-recursive keylist (cdr alol)))
	 (else
	  result-from-value))))
     (else
      (assq-recursive keylist (cdr alol)))))
   ((and (alist? alol)
	 (not (alist? (cdr (car alol))))
	 (null? (cdr keylist)))
    (cond
     ((equal? (car keylist) (car (car alol)))
      (car alol))
     (else
      (assq-recursive keylist (cdr alol)))))
   ((and (alist? alol)
	 (not (alist? (cdr (car alol))))
	 (not (null? (cdr keylist))))
    (cond
     ((equal? (car keylist) (car (car alol)))
      (let ((found-in-value (assq-recursive (cdr keylist) (cdr (car alol)))))
	(cond
	 ((not (eq? #f found-in-value)) found-in-value)
	 (else (assq-recursive keylist (cdr alol))))))
     (else (assq-recursive (cdr alol)))))))

Now the function is complete and can be tested, and then edited.

Edits:
For the first two clauses, we see they return the same value. Thus we
could have a single clause:
(or (and (null? (cdr keylist)) (null? alol))
    (and (not (null? (cdr keylist))) (null? alol)))

Since we can distribute with and and or, we can also factor out the
common factor:
(and (null? alol) (or (not (null? (cdr keylist))) (null? (cdr
							  keylist))))
which further reduces to
(null? alol).

Next we note that all of the other conditions all start with (equal?
							      (car
							       keylist)
							      (car
							       (car
								alol))).
They will all, if this is true, return (car alol). So we can create a
nested cond, flipping the order:
(cond
 ((equal? (car keylist) (car (car alol)))
  (car alol))
 (else
  ... others ...))
For the others, we note that they pretty much all, no matter what
keylist is, have an else clause that is (assq-recursive keylist (cdr
								 alol)).
Next, we note that two other clauses have the same answers to
different 'questions':
(and (alist? alol) (alist? (cdr (car alol))) (null? (cdr
							  keylist)))
(and (alist? alol) (not (alist? (cdr (car alol)))) (null? (cdr
							   keylist)))
it doesn't matter which we get, we have the answer
(cond
 ((equal? (car keylist) (car (car alol))) (car alol))
 (else (assq-recursive keylist (cdr alol))))

since we can't recurse into the value of the current key.

There remain the two other cond clauses which have similar 
We can answer false for the first clause, and false for the second
clause. On further examination we realize that one has an error: It
recurses into (cdr (car alol)) but that doesn't match the contract, so
that shouldn't be there. If the keylist is not null and the value of
the current key is not an alist, we can't recurse and the only
possible natural recursion is the rest of the list, so that actually
has the same answer as the top two:

(and (alist? alol) (not alist? (cdr (car alol))) (not (null? (cdr
							      keylist)))).
which will have the same value. So that leaves only a single clause
with a different way of doing things:

  (define (assq-recursive keylist alol)
    (cond
     ((null? alol) #false)
     ((or
       (and (alist? alol)
	   (alist? (cdr (car alol)))
	   (null? (cdr keylist)))
       (and (alist? alol)
	    (not (alist? (cdr (car alol))))
	    (not (null? (cdr keylist))))
       (and (alist? alol)
	    (not (alist? (cdr (car alol))))
	    (null? (cdr keylist)))))
      (cond
       ((equal? (car keylist) (car (car alol)))
	(car alol))
       (else
	(assq-recursive keylist (cdr alol)))))
     ((and (alist? alol)
	   (alist? (cdr (car alol)))
	   (not (null? (cdr keylist))))
      (cond
       ((equal? (car keylist)
		(car (car alol)))
	(let ((result-from-value
	       (assq-recursive (cdr keylist)
			       (cdr (car alol)))))
	  (cond
	   ((eq? #false result-from-value)
	    (assq-recursive keylist (cdr alol)))
	   (else
	    result-from-value))))
       (else
	(assq-recursive keylist (cdr alol)))))))

We see we may manipulate the second clause logic. We can factor out
alist? that is the first question.
(and (alist? alol)
     (or
      (and (alist? (cdr (car alol)))
	   (null? (cdr keylist)))
      (and (not (alist? (cdr (car alol))))
	   (null? (cdr keylist)))
      (and (not (alist? (cdr (car alol))))
	   (not (null? (cdr keylist))))))

(and (alist? alol)
     (or
      (and (not (alist? (cdr (car alol))))
	   (or (not (null? (cdr keylist)))
	       (null? (cdr keylist))))
      (and (alist? (cdr (car alol)))
	   (null? (cdr keylist)))))

(and (alist? alol)
     (or (not (alist? (cdr (car alol))))
	 (and (alist? (cdr (car alol)))
	      (null? (cdr keylist)))))
	  
We can also add an error in a final else clause, for if alol is not an
alist. We'll add a guarding condition to see if keylist is null, which
is not the data definition but somebody may nonetheless pass a null
keylist. 


  (define (assq-recursive keylist alol)
    (cond
     ((null? alol) #false)
     ((null? keylist)
      (error 'assq-recursive
	     "Null keylist"
	     keylist))
     ((and (alist? alol)
	   (or (not (alist? (cdr (car alol))))
	       (and (alist? (cdr (car alol)))
		    (null? (cdr keylist))))))
      (cond
       ((equal? (car keylist) (car (car alol)))
	(car alol))
       (else
	(assq-recursive keylist (cdr alol)))))
     ((and (alist? alol)
	   (alist? (cdr (car alol)))
	   (not (null? (cdr keylist))))
      (cond
       ((equal? (car keylist)
		(car (car alol)))
	(let ((result-from-value
	       (assq-recursive (cdr keylist)
			       (cdr (car alol)))))
	  (cond
	   ((eq? #false result-from-value)
	    (assq-recursive keylist (cdr alol)))
	   (else
	    result-from-value))))
       (else
	(assq-recursive keylist (cdr alol)))))
     (else (error 'assq-recursive
		       "Parameters do not meet data definition"
		       keylist alol)))

Now we want assq-recursive-cons, which takes a path/key:
(one two three four . five)
i.e.
(one . (two . (three . (four . five))))
or
(one two three . (four . five))
which, if the key path (one two three) exists in it's entirety, adds
the key/value (four . five) to the alist-of-alists at that point,
otherwise, it gives an error.

examples:
(aconsr
 '(one two three . (four . five))
 '( (one . ( (two . ( (three . ())))))))
'( (one . ( (two . ( (three . (four . five)))))))
(aconsr
 '(one . two)
 '())
'((one . two))
(aconsr
 '(one two three . (four . five))
 '( (doodle . ( (daddle . deedle)))
    (one . ( (two . (
		     (six . seven)
		     (three . ())))))))
'(
		  


|#
  (define (assq-recursive
	   keylist
	   alol)
    (cond
     ((and (null? (cdr keylist)) (pair? alol))
      (cond
       ((equal? (car keylist)
		(caar alol))
	(car alol))
       (else #f)))
     ((and (pair? keylist) (pair? alol))
      (cond
       ((equal? (caar alol) (car keylist))
	(let ((result-of-value (assq-recursive
				(cdr keylist)
				(cdar alol))))
	  (if result-of-value
	      result-of-value
	      (assq-recursive
	       keylist
	       (cdr alol)))))
       (else
	(assq-resursive keylist (cdr alol)))))
     ((and (pair? keylist) (null? alol)) #f)))
       
  (define (acons-recursive
	   keylist
	   alol
	   kv)
    "acons-recursive : list-of-keys alist-of-alists value
add a value to an alist-of-alists by the key list, e.g.
(equal?
 (acons-recursive '(one two four) '( (one . ( (two . ( (three . 4)))))) 5)
 '(one . ( (two . ( (three . 4) (four . 5))))))
(equal?
 (acons-recursive '(cat dog rat)
		  '( (cat . 
			  ( (dog . ( (mouse . dood))))))
		  rood)
 '( (cat . 
	 ( (dog . ( (mouse . dood)
		    (rat . rood)))))))"
(cond
 ((and (pair? keylist) (pair? alol))
  (cond
   ((and
     (equal? (car keylist) (caar alol))
     (list? (cdar alol)))
    (cons
     (cons (caar alol)
	   (acons-recursive (cdr keylist) (cdar alol) kv))
     (cdr alol)))
   (else
    (cons (car alol)
	  (acons-recursive keylist (cdr alol) kv)))))
 ((and (null? keylist) (pair? alol))
  (cons kv alol))
 ((and (pair? keylist) (null? alol))
  (error 'acons-recursive "key not found" keylist))
 ((and (null? alol) (null? keylist))
  (cons kv alol)))))



	   
	   

	  
