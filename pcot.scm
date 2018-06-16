(use alist-lib
     posix)
(define moodc 0)
(define mood 0)
(define argc 0)
(define filec 0)
(define dat-file "/full/path/to/pcot.dat")
(define params `((--show-folder . 0)
		 (--iteration-count . 0)
		 (--absolute-dir .
   "/full/path/to/Characters/Red/")
		 (--moodc . 0)
		 (--greeting-threshold . 0)
		 (--rand-threshold . 1)
		 (--mad-threshold . 3)))
(define folders '("0" "1" "2"))
(define arg #f)
(define used-file-names '())
(define avail-file-names '())
(define current-file-count 0)
(define (usage)
  (print "..")
  (exit))

(define (clean-data)
  (process-run
   (string-append "rm -r " dat-file "  &> /dev/null")))


(define (get-available-files)
  (let ((file-list (directory
		    (string-append
		     (alist-ref params '--absolute-dir)
		     (number->string mood)))))
    (for-each
     (lambda (x)
       (and-let*
	   ((g (not (null? x)))
	    (x (string-split x "."))
	    (x (if (equal? (cadr x) "mp3")
		   (car x)
		   #f))
	    (g (not (member x used-file-names))))
	 (set! filec (add1 filec))
	 (set! avail-file-names
	   (append avail-file-names `(,x)))))
     file-list)))

(define (check-mood)
  (let ((i 0)
	(cnt 0))
    (for-each
     (lambda (x)
       (when (and
	      (> cnt 0)
	      (>= moodc (alist-ref params x)))
	 (set! i (add1 i)))
       (when (> i (length (cddddr params)))
	 (set! i 0))
       (set! cnt (add1 cnt)))
     (cddddr (alist-keys params)))
    (set! mood i)))


(define (iterate)
  (when (file-exists? dat-file)
    (read-config-file))
  (get-available-files)
  (if (> (length avail-file-names) 0)
      (let* ((picked-ind
	      (random  (sub1 (length avail-file-names))))
	     (file-name
	      (list-ref avail-file-names picked-ind))
	     (it-count (alist-ref params '--iteration-count)))
	(set! moodc (add1 (alist-ref params '--moodc)))
	(alist-update! params
		       '--moodc
		       (lambda (k) moodc ))
	(check-mood)
	(set! used-file-names
	  (append used-file-names `(,file-name)))
	(printf "~A-~A" (list-ref folders mood) file-name)
	(alist-update! params
		       '--iteration-count
		       (lambda (k) (add1 it-count) ))
	(write-config-file))
      (begin (clean-data)
	     (set! avail-file-names '())
	     (set! used-file-names '())
	     (iterate))))

(define (write-config-file)
  (let ((fp (file-open dat-file
		       (+ open/wronly
			  open/creat
			  open/trunc))))
    (for-each
     (lambda(x)
       (let* ((v (cdr x))
	      (v (if (number? v)
		     (number->string v)
		      v)))
	 (file-write
	  fp
	  (sprintf
	   "~A~%"
	   (string-append (symbol->string (car x))
			 "="
			 v)))))
     params)
    (for-each
     (lambda(x)
       (file-write fp
		   (sprintf
		    "~A~%"
		    x)))
     used-file-names)
    (file-close fp)))
    

(define (read-config-file)
  (let* ((fp (open-input-file dat-file))
	 (lines (read-lines fp)))
    (for-each
     (lambda (x)
       (let* ((is-param (memq #\= (string->list x)))
	      (param-val
	       (if is-param
		   (list->string (cdr is-param))
		   #f))
	      (param-name
	       (string->symbol
		(car (string-split x "=")))))
	 (if is-param
	     (begin
	       (alist-update! params
			      param-name
			      (lambda(k)
				(let ((x (string->number param-val)))
				  (if x x param-val)))))
	     (set! used-file-names
	       (append used-file-names
		       `(,x))))))
     lines)
    (close-input-port fp)))

(for-each
 (lambda (x)
   (if (= (remainder argc 2) 0)
       (let ((a (string->symbol x)))
	 (if (memq a
		   (alist-keys params))
	     (set! arg a)
	     (usage)))
       (begin (alist-set! params arg x)
	      (set! arg #f)))
   (set! argc (add1 argc)))
 (command-line-arguments))
(iterate)
(exit)



  





