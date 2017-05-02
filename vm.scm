(library (vm)
  
  (export vm-command
	  master-vm-settings
	  boot-vm
	  vm-settings)
  
  (import
    (rnrs)
    (ice-9 list)
;;    (ice-9 r4rs)
    (ice-9 r5rs)
    (custom processes)
    (srfi srfi-8))

  (define (->string atom)
    (cond
     ((string? atom) atom)
     ((number? atom) (number->string atom))
     ((symbol? atom) (symbol->string atom))
     ((boolean? atom) (boolean->string atom))
     (else (error "bad atom" atom))))
  
  
  (define *master-settings* "./master.sexp")
  
  (define (build-vm-filename vm)
    (string-append "./" vm ".sexp"))

  (define (master-vm-settings)
    (with-input-from-file *master-settings*
      (lambda ()
	(read))))

  (define (get-vm-details vm)
    (let ((master-settings (master-vm-settings)))
      (cdr (assq vm
	    (cdr (assq 'vms master-settings))))))

  (define (get-vm-setting setting vm-settings)
    (cdr (assq setting vm-settings)))

  (define (update-vnc-port vm-settings
			   master-settings
			   vnc-port)
    (+ 1 vnc-port))


  (define (update-drive vm-settings
			master-settings
			drive)
    (let ((new-drive (string-append
		      (->string drive)
		      ".copy")))
      new-drive))

  (define (update-tap-id vm-settings
			 master-settings
			 tap)
    (let ((tap-num (string->number
		    (substring
		    (->string tap)
		    3))))
      (string-append
       "tap"
       (->string (+ 1 tap-num)))))
  
  (define *clone-settings-handlers*
    `(
      (vnc-port . ,update-vnc-port)
      (drive . ,update-drive)
      (tap-id . ,update-tap-id)))
  (define (clone-vm-settings vm)
    (let ((master-settings (master-vm-settings))
	  (vm-settings (get-vm-details  vm)))
      (map
       (lambda (key-value)
	 (let* ((assqcons
		(assq (car key-value)
		      *clone-settings-handlers*))
	       (transformer
		(if assqcons (cdr assqcons) #f)))

	   (if transformer
	       (cons
		(car key-value)
		(transformer vm-settings
			    master-settings
			    (cdr key-value)))
	       key-value)))
       vm-settings)))
  
  (define (clone-vm vm)
    (let* ((new-vm-settings (clone-vm-settings vm))
	   (new-master-settings
	    (cons (cons
		   (string->symbol
		    (string-append
		     (symbol->string vm)
		     "-copy"))
		    new-vm-settings)
		  (master-vm-settings))))
      (write-vm-settings new-master-settings)))
  
      
  (define (write-vm-settings settings)
    (let ((port (open-file-output-port *master-settings*
				  (file-options no-fail))))
      (write settings port)
      (close-output-port port)))


  (define (vm-command vm)
    (let ((vm-settings (get-vm-details (string->symbol vm))))
      (cond
       ((eq? #f vm-settings)  (error "no vm settings"))
       (else
	(list 'sudo
	      'kvm
	      '-boot 'cd
	      '-vnc (get-vm-setting 'vnc-port vm-settings)
	      '-net 'nic,model=virtio
	      '-net (string-append
		     "tap,id="
		     (symbol->string (get-vm-setting 'tap-id vm-settings)))
	      '-cdrom (get-vm-setting 'cdrom vm-settings)
	      '-m (get-vm-setting 'memory vm-settings)
	      '-smp (string-append
		     "cpus="
		     (number->string
		      (get-vm-setting 'cpus vm-settings))))))))
  
  (define (boot-vm vm)
    (receive (pid output ignore-input)
  	(run+ ((> (1 2) (#:port)) 
  	       (< (0) (#:string "")))
  	      (begin (vm-command vm)))
      (let ((result (wait pid)))
  	(cond
  	 ((eq? (status:exit-val (cdr result)) 0)
  	  (display "OK\n"))
  	 (else (begin
  		 (display "ERROR\n")
  		 (display (vm-command vm))
  		 (newline)
  		 (drain-port->port output (current-output-port)))))))))

  ;; (if (not (= 2 (length (command-line))))
  ;;     (error "You need a command and a VM name")
  ;;     (let ((command (list-ref (command-line) 1))
  ;; 	    (vm-name (list-ref (command-line) 2)))
  ;; 	(case command
  ;; 	  ((boot) (boot-vm vm-name))
  ;; 	  ((clone) (clone-vm vm-name))
  ;; 	  (else (error "No such command" command))))))
