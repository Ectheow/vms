(import (assq-tools)
	(srfi srfi-64)
	(rnrs))


(test-begin "assq-recursive")
(test-equal
    '(four . ( (five . six )))
    (assq-recursive '(one two three four)
		'( (two . ( ( five . ( (six . seven)))))
		   (one . ( (five . ( (doodle . 8)))
			    (eight . ( (twenty . cats)
				       (fifty . dogs)))
			    (two . ( (six . seven)
				     (three . ( (four . ( (five . six)))))))
			    (five . eight))))))
(test-equal
    #false
  (assq-recursive '(one two)
		  '()))
(test-error
 &error
 (assq-recursive '()
		 '((one . two))))
(test-error
 &error
 (assq-recursive '(one two)
		 '(one two three)))
(test-equal
    #false 
  (assq-recursive '(one two)
		  '( (one . ( ( three . four)))
		     (two . five))))
(test-equal
    '(one . two)
  (assq-recursive
   '(one)
   '( (one . two))))
(test-equal
    '(four . five)
  (assq-recursive
   '(one two four)
   '( (one . ( (two . ( ( four . five))))))))


(test-end "assq-recursive")

