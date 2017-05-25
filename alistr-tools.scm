(library (assq-tools)
  (export assqr aconsr)
  (import (rnrs))
  
  (define (alist? alist)
    (or (null? alist)
	(and (pair? alist)
	     (pair? (car alist)))))
  ;; (define (assqr keylist alol)
  ;;   (cond
  ;;    ((and (null? (cdr keylist))
  ;; 	   (null? alol)) #false)
  ;;    ((and (null? alol)
  ;; 	   (not (null? (cdr keylist)))) #false)
  ;;    ((and (alist? alol)
  ;; 	   (alist? (cdr (car alol)))
  ;; 	   (null? (cdr keylist)))
  ;;     (cond
  ;;      ((equal? (car keylist) (car (car alol)))
  ;; 	(car alol))
  ;;      (else
  ;; 	(assqr keylist (cdr alol)))))
  ;;    ((and (alist? alol)
  ;; 	   (alist? (cdr (car alol)))
  ;; 	   (not (null? (cdr keylist))))
  ;;     (cond
  ;;      ((equal? (car keylist)
  ;; 		(car (car alol)))
  ;; 	(let ((result-from-value
  ;; 	       (assqr (cdr keylist)
  ;; 			       (cdr (car alol)))))
  ;; 	  (cond
  ;; 	   ((eq? #false result-from-value)
  ;; 	    (assqr keylist (cdr alol)))
  ;; 	   (else
  ;; 	    result-from-value))))
  ;;      (else
  ;; 	(assqr keylist (cdr alol)))))
  ;;    ((and (alist? alol)
  ;; 	   (not (alist? (cdr (car alol))))
  ;; 	   (null? (cdr keylist)))
  ;;     (cond
  ;;      ((equal? (car keylist) (car (car alol)))
  ;; 	(car alol))
  ;;      (else
  ;; 	(assqr keylist (cdr alol)))))
  ;;    ((and (alist? alol)
  ;; 	   (not (alist? (cdr (car alol))))
  ;; 	   (not (null? (cdr keylist))))
  ;;     (cond
  ;;      ((equal? (car keylist) (car (car alol)))
  ;; 	(let ((found-in-value (assqr (cdr keylist) (cdr (car alol)))))
  ;; 	  (cond
  ;; 	   ((not (eq? #false found-in-value)) found-in-value)
  ;; 	   (else (assqr keylist (cdr alol))))))
  ;;      (else (assqr keylist (cdr alol))))))))

  (define (assqr keylist alol)
    (cond
     ((null? alol) #false)
     ((null? keylist)
      (error 'assqr
	     "Null keylist"
	     keylist))
     ((and (alist? alol)
	   (or (not (alist? (cdr (car alol))))
	       (and (alist? (cdr (car alol)))
		    (null? (cdr keylist)))))
      (cond
       ((equal? (car keylist) (car (car alol)))
	(car alol))
       (else
	(assqr keylist (cdr alol)))))
     ((and (alist? alol)
	   (alist? (cdr (car alol)))
	   (not (null? (cdr keylist))))
      (cond
       ((equal? (car keylist)
		(car (car alol)))
	(let ((result-from-value
	       (assqr (cdr keylist)
			       (cdr (car alol)))))
	  (cond
	   ((eq? #false result-from-value)
	    (assqr keylist (cdr alol)))
	   (else
	    result-from-value))))
       (else
	(assqr keylist (cdr alol)))))
     (else (error 'assqr
		  "Parameters do not meet data definition"
		  keylist alol))))

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
  )
