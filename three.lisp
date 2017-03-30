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
	       (cond ((eq mp psw)
		      (progn
			(reset-count)
			(cond ((eq mf 'withdraw) #'withdraw)
			     ((eq mf 'deposit) #'deposit)
			   
			     (t (error "unkown request -- make-account ~A" mf)))))
		     ((eq mp 'check)
		      (if (eq mf psw)
			  t
			  nil))
		     (t
		      (cond ((>= counter 7)
			     #'(lambda (m) "call-the-cops"))
			    (t
			     
			     (setq counter (+ counter 1))
			     #'(lambda (m) "incorrect password")))))))
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
  (sqrt (/ 6 (monte-carlo trials #'cesaro-test))))

(defun cesaro-test ()
  (= (gcd (funcall *rand*) (funcall *rand*)) 1))
(defun monte-carlo (trials experiment)
  (labels ((iter (trials-remaining trials-passed)
	     (cond ((= trials-remaining 0)
		    (/ trials-passed trials))
		   ((funcall experiment)
		    (iter (- trials-remaining 1) (+ trials-passed 1)))
		   (t
		    (iter (- trials-remaining 1) trials-passed)))))
    (iter trials 0)))

(defun rand-update (x)
  (mod (+ (* x 1)
	  3)
       10))

(defun make-rand ()
  (let ((x 4))
    #'(lambda ()
	(setq x (rand-update x))
	x)))
(defvar *rand* (make-rand))


;;;;3.5

(defun random-in-range (low hig)
  (let ((range (- hig low)))
    (+ low (random (* 1.0 range)))))

(defun estimate-integral (fp x1 y1 x2 y2 trials)
  (* (- x2 x1)
     (- y2 y1)
     (monte-carlo trials #'(lambda ()
			     (let ((r (/ (- x2 x1) 2)))
			       (funcall fp
					(random-in-range x1 x2)
					(random-in-range y1 y2)
					r))))))

(defun square (x) (* x x))

(defun get-pi (trials)
  (estimate-integral #'(lambda (x y r)
			 (<= (+ (square x)
				(square y))
			     r))
		     -1.0
		     -1.0
		     1.0
		     1.0
		     trials))

;;;;3.6

(defun make-rand (random-init)
  (let ((x random-init))
    #'(lambda (m)
	(cond ((eq m 'generate)
	       (progn (setq x (rand-update x))
		      x))
	      ((eq m 'reset)
	       #'(lambda (new-value)
		   (setq x new-value)))))))

(let ((rand (make-rand 0)))
  (show (funcall rand 'generate))
  (show (funcall (funcall rand 'reset) 4))
  (show "end"))


;;;;3.7

(defun make-joint (acc pwd n-pwd)
  (if (funcall acc 'check pwd)
      #'(lambda (mp mf)
	  (if (eq mp n-pwd)
	      (funcall acc pwd mf)
	      "inccorect password"))
      "inccorect password"))

(let ((peter-acc (make-account 100 'aaa)))
  (let ((paul-acc (make-joint peter-acc 'aaa 'bbb))
	(paul2-acc (make-joint peter-acc 'a 'bbb)))
    (show (funcall (funcall peter-acc 'aaa 'withdraw)
		   10))
    (show (funcall peter-acc 'check 'aaa))
    (show (funcall (funcall paul-acc 'bbb 'withdraw)
		   10))
    
    (show paul2-acc)
    (show "3.7 end")))

;;;;3.8

(defun make-f ()
  (let ((x nil))
    #'(lambda (n)
	(if x
	    0
	    (progn (setq x t)
		   n)))))
		  


;;;;3.11

;;每次执行(make-account 100)都会创建一个新的环境。局部状态分别保存在不同的环境中。
;;acc和acc2共享的部分只有make-account的外部环境。



(let ((x (cons 'a 'b))
      (y (cons 'c 'd)))
  (show x)
  (setf (cdr x) y)
  (show x)
  (show "end"))
