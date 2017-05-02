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
  ;;
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
data definitions
alist:
1. '()
2. (cons pair alist) where pair is a pair and alist is an alist.

alist-of-alists:
1. '()
2. (cons (cons key alist1) alist2) where alist1 and alist2 are alists.

keylist:
1. (cons k '()) where k is some value
2. (cons k keylist) where k is some value and keylist is a keylist.


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



	   
	   

	  
