(import (alistr-tools)
	(srfi srfi-64)
	(rnrs))


(test-begin "assqr")
(test-equal
    '(four . ( (five . six )))
    (assqr '(one two three four)
		'( (two . ( ( five . ( (six . seven)))))
		   (one . ( (five . ( (doodle . 8)))
			    (eight . ( (twenty . cats)
				       (fifty . dogs)))
			    (two . ( (six . seven)
				     (three . ( (four . ( (five . six)))))))
			    (five . eight))))))
(test-equal
    #false
  (assqr '(one two)
		  '()))
(test-error
 &error
 (assqr '()
		 '((one . two))))
(test-error
 &error
 (assqr '(one two)
		 '(one two three)))
(test-equal
    #false 
  (assqr '(one two)
		  '( (one . ( ( three . four)))
		     (two . five))))
(test-equal
    '(one . two)
  (assqr
   '(one)
   '( (one . two))))
(test-equal
    '(four . five)
  (assqr
   '(one two four)
   '( (one . ( (two . ( ( four . five))))))))


(test-end "assqr")

(test-begin "aconsr")
(test-equal
    '( (one . two))
  (aconsr '(one) 'two '()))
(test-equal
    '( (a . 2) (b . one) (a . two))
  (aconsr '(a) 2 '( (b . one) (a . two))))

(test-equal
    '( (one . ( (two . four)))
       (two . ( (three . ( (four . ( (five . six)
				     (seven . eight)
				     (nine . ten)
				     (eleven . ( (twelve . thirteen))))))))))
    (aconsr '(two three four eleven twelve)
	    'thirteen
	    '( (one . ( (two . four)))
	       (two . ( (three . ( (four . ( (five . six)
					     (seven . eight)
					     (nine . ten)
					     (eleven . ()))))))))))

(test-error
 &error
 (aconsr '(one two) '()))

(test-error
 &error
 (aconsr '(one two)
	 '( (one . two))))

(test-error
 &error
 (aconsr '(one two three)
	 '( (one . ( (two . three))))))

(test-equal
    '( (one . ( ( two . three)
		( four . five)
		( six . seven)))
       (two . ( ( three . ( (four . five)))
		( five . ( ( six . seven))))))
  (aconsr '(two five six) 'seven
    '( (one . ( ( two . three)
		( four . five)
		( six . seven)))
       (two . ( ( three . ( (four . five)))
		( five . ()))))))

(test-end "aconsr")
