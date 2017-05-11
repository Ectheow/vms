(library (assq-tools)
  (export assq-recursive)
  (import (rnrs))
  
  (define (alist? alist)
    (and (pair? alist)
	 (pair? (car alist))))
  ;; (define (assq-recursive keylist alol)
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
  ;; 	(assq-recursive keylist (cdr alol)))))
  ;;    ((and (alist? alol)
  ;; 	   (alist? (cdr (car alol)))
  ;; 	   (not (null? (cdr keylist))))
  ;;     (cond
  ;;      ((equal? (car keylist)
  ;; 		(car (car alol)))
  ;; 	(let ((result-from-value
  ;; 	       (assq-recursive (cdr keylist)
  ;; 			       (cdr (car alol)))))
  ;; 	  (cond
  ;; 	   ((eq? #false result-from-value)
  ;; 	    (assq-recursive keylist (cdr alol)))
  ;; 	   (else
  ;; 	    result-from-value))))
  ;;      (else
  ;; 	(assq-recursive keylist (cdr alol)))))
  ;;    ((and (alist? alol)
  ;; 	   (not (alist? (cdr (car alol))))
  ;; 	   (null? (cdr keylist)))
  ;;     (cond
  ;;      ((equal? (car keylist) (car (car alol)))
  ;; 	(car alol))
  ;;      (else
  ;; 	(assq-recursive keylist (cdr alol)))))
  ;;    ((and (alist? alol)
  ;; 	   (not (alist? (cdr (car alol))))
  ;; 	   (not (null? (cdr keylist))))
  ;;     (cond
  ;;      ((equal? (car keylist) (car (car alol)))
  ;; 	(let ((found-in-value (assq-recursive (cdr keylist) (cdr (car alol)))))
  ;; 	  (cond
  ;; 	   ((not (eq? #false found-in-value)) found-in-value)
  ;; 	   (else (assq-recursive keylist (cdr alol))))))
  ;;      (else (assq-recursive keylist (cdr alol))))))))

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
	(assq-recursive keylist (cdr alol)))))
     (else (error 'assq-recursive
		       "Parameters do not meet data definition"
		       keylist alol)))))
