;;;第三章 模块化、对象和状态

(defun show (m)
  (format t "~%the show is ~A~%" m))

(defun withdraw ()
  (let ((balance 100))
    #'(lambda (amount)
	(if (>= balance amount)
	    (progn
	      (setq balance (- balance amount))
	      balance)
	    (format t "~% not enough balance.~%")))))


(defun make-withdraw (balance)
  #'(lambda (amount)
      (if (>= balance amount)
	  (progn
	    (setq balance (- balance amount))
	    balance)
	  (format t "~%not enough balance~%"))))



(defun make-account (balance)
  (labels ((withdraw (amount)
	     (if (>= balance amount)
		 (progn (setq balance (- balance amount))
			balance)
		 "insufficient funds"))
	   (deposit (amount)
	     (setq balance (+ balance amount))
	     balance)
	   (dispatch (m)
	     (cond ((eq m 'withdraw) #'withdraw)
		   ((eq m 'deposit) #'deposit)
		   (t (error "unkown request --make-account ~A" m)))))
    #'dispatch))
	     
(let ((acc (make-account 100)))
  (show (funcall (funcall acc 'withdraw)
		 50))
  (show (funcall (funcall acc 'withdraw)
		 60))
  (show (funcall (funcall acc 'deposit)
		 40))
  (show (funcall (funcall acc 'withdraw)
		 60))
  (show "end!"))

;;;;3.1
(defun make-accumulator (s)
  #'(lambda (n)
      (setq s (+ s n))))

(let ((a (make-accumulator 5)))
  (show (funcall a 10))
  (show (funcall a 10))
  (show "end"))

;;;;3.2
(defun make-monitored (f)
  (let ((counter 0))
    (labels ((how-many-calls? () counter)
	     (reset-count () (setq counter 0))
	     (dispatch (mf)
	       (cond ((eq mf 'how-many-calls?) (how-many-calls?))
		     ((eq mf 'reset-count) (reset-count))
		     (t 
		      (setq counter (+ counter 1))
		      (funcall f mf)))))
		      
      #'dispatch)))

(let((s (make-monitored #'sqrt)))
  (show (funcall s 100))
  (show (funcall s 100))
  (show (funcall s 'how-many-calls?))
  (show "end"))
	       
;;;;3.3 3.4

(defun make-account (balance psw)
  (let ((counter 1))
    (labels ((withdraw (amount)
	       (if (>= balance amount)
		   (progn (setq balance (- balance amount))
			  balance)
		   "insuffcient funds"))
	     (deposit (amount)
	       (setq balance (+ balance amount)))
	     (reset-count () (setq counter 1))
	     
	     (dispatch (mp mf)
	       (if (eq mp psw)
		   (progn 
		     (reset-count)
		     (cond ((eq mf 'withdraw) #'withdraw)
			   ((eq mf 'deposit) #'deposit)
			   (t (error "unkown request -- make-account ~A" mf))))
		   (cond ((>= counter 7)
			  #'(lambda (m) "call-the-cops"))
			 (t

			  (setq counter (+ counter 1))
			  #'(lambda (m) "incorrect password"))))))
      #'dispatch)))

(let ((acc (make-account 100 'aaa)))
  (show (funcall (funcall acc 'aaa 'withdraw)
		 40))
  (show (funcall (funcall acc 'a 'deposit)
		 50))
  (show (funcall (funcall acc 'a 'deposit)
		 50))
  (show (funcall (funcall acc 'a 'deposit)
		 50))
  (show (funcall (funcall acc 'a 'deposit)
		 50))
  (show (funcall (funcall acc 'a 'deposit)
		 50))
  (show (funcall (funcall acc 'a 'deposit)
		 50))
  (show (funcall (funcall acc 'a 'deposit)
		 50))
  (show (funcall (funcall acc 'aaa 'deposit)
		 50))
  (show (funcall (funcall acc 'a 'deposit)
		 50))
  (show "end"))


;;;;3.1.2

(defun estimate-pi (trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(defun cesaro-test ()
  (= (gcd (rand) (rand)) 1))
(defun monte-carlo (trials experiment)
  (labels ((iter (trials-remaining trials-passed)
	     (cond ((= trials-ramaining 0)
		    (/ trials-passed trials))
		   ((experiment)
		    (iter (- trials-remaining 1) (+ trials-passed 1)))
		   (t
		    (iter (- trials-remaining 1) trials-passed)))))
    (iter trials 0)))
