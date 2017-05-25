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
;; a path-and-key is:

1. (cons a b) where a and b are any atomic values
2. (cons pc pk) where pc is a symbol and pk is a path-and-key

;; assq-cons : alist-of-alists path-and-key -> alist-of-alists 
;; add the key/value pair to the keypath specified.

examples:
(equal? 
  (aconsr
   '(one two three four) 
   'five
   '( (one . ( (two . ( (three . ())))))))

  '( (one . ( (two . ( (three . (four . five))))))))

(equal?
 (aconsr
  '(one)
  'two
  '())
 '( (one . two) ))

(equal? 
 (aconsr
  '(one two three)
  'four
  '( (one . ( (two . ()) ) ) (four . six)))
 '( (one . ( (two . (three . four)) ) ( four . six))))

Possible combinations:
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


1. (cons a '()) where a is a key value
2. (cons s kl) where s is a key value and kl is a keylist

key values are atoms.

We are not 'processing' value,  just putting it in.

cases:
|                           |(null? alol)                             |  (and (alist? lol) (alist? (cdar alol)))                           | (and (alist? alol) (not (alist? (cdar alol))))|
|(null? (cdr keylist))      |(and (null? (cdr keylist)) (null? alol)) |(and (null? (cdr keylist)) (and (alist? lol) (alist? (cdar alol)))) | (and (null? (cdr keylist)) (and (alist? alol) (not (alist? (cdar alol)))))
|(not (null? (cdr keylist)))|(and (not (null? (cdr keylist0))) (null? alol)) | (and (not (null? (cdr keylist))) (and (alist? alol) (alist? (cdar alol)))) | (and (not (null? (cdr keylist))) (and (alist? alol) (not (alist? (cdar alol))))) |

We can fill these out:
|#
(define (aconsr keylist value alol)
  (cond
   ((and (null? (cdr keylist))
	 (null? alol))
    ...(car keylist) ...)
   ((and (null? (cdr keylist))
	(and (alist? alol)
	     (alist? (cdar alol))))
    ... (car keylist) ...
    ... (cdr alist) ...
    ... (cdr (car alist)) ...
    ... (car (car alist)) ...)
   ((and (null? (cdr keylist))
	 (and (alist? alol)
	      (not (alist? (cdar alol)))))
    ... (car keylist) ...
    ... (cdr alol) ...
    ... (car (car alol)) ...
    ... (cdr (car alol)) ...)
   ((and (not (null? (cdr keylist)))
	 (null? alol))
    ... (car keylist) ...
    ... (cdr keylist) ...)
   ((and (not (null? (cdr keylist)))
	 (and (alist? alol)
	      (alist? (cdar alol))))
    ... (car keylist) ...
    ... (cdr keylist) ...
    ... (cdr alol) ...
    ... (car (car alol)) ...
    ... (cdr (car alol)) ...)
   ((and (not (null? (cdr keylist)))
	 (and (alist? alol)
	      (not (alist? (cdar alol)))))
    ... (car keylist) ...
    ... (cdr keylist) ...
    ... (cdr alist) ...
    ... (car (car alist)) ...
    ... (cdr (car alist)) ...)))
#|
Next we should annotate with possible recursions. We can see that for
the most complex case, where all the values of the alist are alists
except for the key and the keylist is a full list, we have:

(aconsr (cdr keylist) value alol)
(aconsr (cdr keylist) vlaue (cdr alol))
(aconsr keylist value (cdr alol))
(aconsr keylist value (cdr (car alol)))
(aconsr (cdr keylist) value (cdr (car alol)))

Only some of these will make sense.
|#
    
(define (aconsr keylist value alol)
  (cond
   ((and (null? (cdr keylist))
	 (null? alol))
    ...(car keylist) ...)
   ((and (null? (cdr keylist))
	(and (alist? alol)
	     (alist? (cdar alol))))
    ... (car keylist) ...
    ... (car (car alist)) ...
    ;; cdr keylist doesn't make sense since it is null.
    (aconsr keylist value (cdr alist))
    (aconsr keylist value (cdr (car alist))))
   ((and (null? (cdr keylist))
	 (and (alist? alol)
	      (not (alist? (cdar alol)))))
    ... (car keylist) ...
    ... (car (car alol)) ...
    ... (cdr (car alol)) ...
    (aconsr keylist value (cdr alol)))
   ((and (not (null? (cdr keylist)))
	 (null? alol))
    ;; no possible recursions with null alol.
    ... (car keylist) ...
    (aconsr (cdr keylist) value alol)
    ... (cdr keylist) ...)
   ((and (not (null? (cdr keylist)))
	 (and (alist? alol)
	      (alist? (cdar alol))))
    ... (car keylist) ...
    ... (car (car alol)) ...
    (aconsr keylist value (cdr alol))
    (aconsr keylist value (cdr (car alol)))
    (aconsr (cdr keylist) value (cdr alol))
    (aconsr (cdr keylist) value (cdr (car alol)))
    (aconsr (cdr keylist) value alol))
   ((and (not (null? (cdr keylist)))
	 (and (alist? alol)
	      (not (alist? (cdar alol)))))
    ... (car keylist) ...
    ... (cdr (car alol)) ...
    ... (car (car alol)) ...
    (aconsr keylist (cdr alol))
    (aconsr (cdr keylist) (cdr alol))
    (aconsr (cdr keylist) alol))))
   
#|
Now we can begin filling it in, by looking at examples. 

For example, we know that we said to make an application of error if
we get a keylist that is more than one item (non-null cdr) with a null
alol. This covers the first two cases, where there is an item in
keylist but nothing in alol.

If alol is empty and the keylist is a single value, we just return
(list (cons key value)).

This leaves us with more complex cases. 

If the keylist has one item:

(equal? (aconsr '(one) 'two '())
'( (one . two) ))
(equal? (aconsr '(one) 'two '( (three . four)))
         '( (one . two) (three . four)))
(equal? (aconsr '(a) 2 '( (a . one) ))
        '( (a . 2) (a . one)))
(equal? (aconsr '(six) 'seven '( (one . two) (three . four)))
	'( (six . seven) (one . two) (three . four)))
(equal? (aconsr '(six) 'seven '( (one . ( (two . three) ))))
        '( (six . seven) ( (one . ( (two . three))))))

No matter what, we just return 
(cons (cons key value) alol)

very simple. This covers fully half of the cases. Note there is a
possibility for bizarre behavior that we don't plan for: a key is
already in the list, but we hit the end early, i.e.

(aconsr '(six) 'seven '( (six . ten)))
would return
'( (six . seven) (six . ten)).

This should be checked for first, our function doesn't handle that.

Next, we worry about a keylist with more than one element. If the
keylist has multiple elements, i.e. non-null cdr, but the alist is
empty, this is an error because the first part of teh path is not in
the alist.

(aconsr '(a b c) 2 '())
;; error.

Next, we have a non-null-cdr keylist, with a non-null alol and also an
alol whose cdr-car is itself an alist, and a non-null-cdr keylist
whose alol's cdr-car is not an alist.

1. (and (not (null? (cdr keylist))) 
        (and (alist? alol)
             (alist? (cdr (car alol)))))
   
   example call (aconsr '(one two) 'six '( (five . ( (two . three)))))
   correct return value is an error.

   example call (aconsr '(one two) 'six '( (one . ( (three . four)))))
. five)))))))
   correct value '( (one . ( (two . six ) (three . four))))

   possible recursions

   1. (aconsr (cdr keylist) value alol)
      (aconsr '(two) 'six '( (five . ( (two . three)))))
    
       This defers to a previous case where we decided we would just
       cons the first of the keylist and the value onto the result
       and would return - 

       '( (two . six) (five . ( (two . three))))
 
       We cannot make correct output from this, it did not detect the
       error. 

   2. (aconsr (cdr keylist) value (cdr alol))
      (aconsr '(two) 'six '())
      
      Again this hits on the unconditional cons clause and returns

      '( (two . six)).

      we cannot make the correct output.

   3. (aconsr keylist value (cdr alol))
 
      (aconsr '(one two) 'six '())

      This will signal an error because of a multiple-value keylist
      and an empty alol. This is correct. If the value was there in
      the cdr

      (aconsr '(one two) 'six '( (four . five) (one . ( (three . four )))))
      we would call
      (aconsr '(one two) 'six '( one . ( (three . four))))
      which assuming it is a correct function is
      '( (one . ( (two . six) (three . four))))
      which isn't our entire output for our function call. We were
     called with

      '( (four . five) ...)
      and should produce
      '( (four . five) (one . ( (two . six) (three . four ))))
      we have '( (one . ( (two . six) (three . four))))
      so we need to do
      (cons (car alol) 
            (aconsr keylist value (cdr alol)))
       

   4. (aconsr (cdr keylist) value (cdr (car alol)))
     
      (aconsr '(two) 'six '( (two . three) ))

      This is for this case not correct because 'five does not match
      'one. However if it had (and it might):

      input:
      (aconsr '(one two) 'six '( (one . ())))
      expected:
      '( (one . ( (two . six ))))
      
      if we used this recursion:
      (aconsr '(two) 'six '())
      which will return (correctly)
      '(two . six)

      We have car alol -- caar alol and cdar alol --  and cdr alol.
      To create correct output:
      (cons (cons (caar alol) (aconsr (cdr keylist) value 
                                                    (cdar alol)))     
             (cdr alol))

       This creates the correct output for the case where the path matches.

      

     

      
     (cond ((equal? (caar alol) (car keylist))
            (cons (cons (caar alol)
                        (aconsr 
                         (cdr keylist) 
                         value (cdr (car alol))))
                  (cdr alol)))
           (else (cons (car alol)
                       (aconsr keylist value (cdr alol)))))

     Note that there is a possibility  -- our data definition has not
     outlawed it -- that an alist-of-alists has multiple keys and that
     a latter could have a full path and still be compliant with the
     data definition. For now we will specify that this type of
     alist-of-alists is illegal and no sane person would use it.

     Finally, we need to make sure we return the right value after our
     recursive call. We need to reconstruct the entire list, so we
     need to add back on what we deconstructed.

     We deconstructed the value of the current alist cell, so we need
     to use the new one provided.
   
     (cons (caar alol) 
           (aconsr (cdr keylist) value (cdar alol)))

     Then we also need to reconstruct the rest of the alist
 
     (cons (cons (caar alol) (aconsr (cdr keylist) alue (cdar alol)))
           (cdr alol))


2. (and (not (null? (cdr keylist)))
        (and (alist? alol)
             (not (alist? (cdar alol)))))

   In this case we simply don't need to worry about the cdr car of
   alol being a possible alist. Since we can't recurse into it, there
   are fewer possible recursions:

   (aconsr (cdr keylist) vlaue (cdr alol))
   (aconsr keylist value (cdr alol))

   However, we may want to signal an error if the car-car of the alol
   matches the car of the keylist, this is undesirable. The user
   should not have more than two keys in the same level of the list
   but validating this is currently beyond our scope.

   Example:
   (aconsr '(one two) 'five '( (one . two)))
   ;; error
   (aconsr '(one two) 'five '( (seven . eight) (nine . (
                              (ten.eleven))))) 
   ;; error
   (aconsr '(one two) 'five '( (seven . eight) (one . ())))
   '( (seven . eight) (one . ( (two . five))))
   ;; In this case

   (aconsr (cdr keylist) value (cdr alol)) is
   (aconsr '(two) 'five '( (one . ())))
   which would be a single-element-keylist clause, which would give us
   '( (two . five) (one . ()))
   not correct.

   The other alternative is
   (aconsr keylist value (cdr alol))
   which is
   (aconsr '(one two) 'five '( (one . ())))
   which would see the 'one is a first element of the keylist, and
   give us a value we can make the correcr result out of.

   so:

   (cond ((equal? (caar alol) (car keylist))
          (error 'aconsr "keypath not in alist"
                         alol keylist))
         (else (cons (car alol)
                     (aconsr keylist value (cdr alol)))))

   This gives us all cases.
|#

(define (aconsr keylist value alol)
  (cond
   ((and (null? (cdr keylist))
	 (null? alol))
    (cons (cons (car keylist) value) '()))
   ((and (null? (cdr keylist))
	(and (alist? alol)
	     (alist? (cdar alol))))
    (cons (cons (car keylist) value) alol))
   ((and (null? (cdr keylist))
	 (and (alist? alol)
	      (not (alist? (cdar alol)))))
    (cons (cons (car keylist) value) alol))
   ((and (not (null? (cdr keylist)))
	 (null? alol))
    ;; no possible recursions with null alol.
    (error 'aconsr "this key path isn't in the alist-of-alists"
	   keylist
	   alol))
   ((and (not (null? (cdr keylist)))
	 (and (alist? alol)
	      (alist? (cdar alol))))
    (cond
     ((equal? (caar alol) (car keylist))
      (aconsr (cdr keylist) value (cdr (car alol))))
     (else (aconsr (cdr keylist) value (cdr alol)))))
   ((and (not (null? (cdr keylist)))
	 (and (alist? alol)
	      (not (alist? (cdar alol)))))
    (cond ((equal? (caar alol) (car keylist))
	   (error 'aconsr "keypath not in alist"
		  alol keylist))
	  (else (aconsr keylist value (cdr alol)))))))

#|
Reduction:

our aconsr returns the same results - 
(cons (cons (car keylist) value) alol)
in three cases, the first three. We can consolidate these into one
case:

(or (and (null? (cdr keylist))
         (null? alol))
    (and (null? (cdr keylist))
         (and (alist? alol)
              (alist? (cdar alol))))
    (and (null? (cdr keylist))
         (and (alist? alol)
              (not (alist? (cdar alol))))))

to which our answer is
(cons (cons (car keylist) value) alol)

we can pull out a common case:

(and (null? (cdr keylist))
     (or (null? alol)
         (and (alist? alol)
              (alist? (cdar alol)))
         (and (alist? alol)
              (not (alist? (cdar alol))))))

As for the second part of the and, this actually covers every possible
value for alist, since it covers all clauses of the data
definition. Our condition becomes:

(null? (cdr keylist))
|#

  (define (aconsr keylist value alol)
    (cond
     ((null? (cdr keylist))
      (cons (cons (car keylist) value) alol))
     ((and (not (null? (cdr keylist)))
	   (null? alol))
      ;; no possible recursions with null alol.
      (error 'aconsr "this key path isn't in the alist-of-alists"
	     keylist
	     alol))
     ((and (not (null? (cdr keylist)))
	   (and (alist? alol)
		(alist? (cdar alol))))
      (cond
       ((equal? (caar alol) (car keylist))
	(cons 
	 (cons
	  (caar alol)
	  (aconsr (cdr keylist) value (cdr (car alol))))
	 (cdr alol)))
       (else
	(cons (car alol)
	      (aconsr keylist value (cdr alol))))))
     ((and (not (null? (cdr keylist)))
	   (and (alist? alol)
		(not (alist? (cdar alol)))))
      (cond ((equal? (caar alol) (car keylist))
	     (error 'aconsr "keypath not in alist"
		    alol keylist))
	    (else (cons (car alol)
			(aconsr keylist value (cdr alol)))))))) 

    
             
	       



       




	   
	   

	  
