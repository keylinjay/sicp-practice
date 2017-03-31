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


;;所谓的setf，只不过是一个宏。它调用lisp内部的表操作函数rplaca和rplacd函数。

(defun set-car! (c v)
  (rplaca c v))

(defun set-cdr! (c v)
  (rplacd c v))


(let ((x (cons 'a 'b))
      (y (cons 'c 'd)))
  (show x)
  (show (set-car! x y))
  (show x)
  
  (show "end"))

;;;;3.12
;;(b)
;;(b c d)

;;;;3.13
;;死循环

;;;;3.14
;;reverse
;;v is (a), w is (c b a)

;;;;3.16

;;因为函数没有判断同一，所以同一个序对多引用一次就会多一次。环形结构就会死循环。

;;;;3.17

(defun count-pairs (lst)
  (labels ((s-lst (lst res)
	     (if (and (consp lst) (not (mem-eq lst res)))
		 (s-lst (cdr lst) 
			(s-lst (car lst) (cons lst res)))
		 res))
		    

	   (mem-eq (item lst)
	     (cond ((null lst) nil)
		   ((eq item (car lst))
		    (cdr lst))
		   (t
		    (mem-eq item (cdr lst))))))
    (length (s-lst lst '()))))
	   
	 

(let ((x (list 'a 'b))
      (y (list 'c 'd)))
  (show (count-pairs (cons x x)))
  (show (count-pairs (list x y x y)))
  (show "end"))

;;;;3.18 3.19
;;只需要判断lst的cdr部分是否和原lst是同一就行了。

(defun cyclep (lst)
  (labels ((test (l1)
	     (cond ((null (cdr l1)) nil)
		   ((eq (cdr l1) lst)
		    t)
		   (t
		    (test (cdr l1))))))
    (test lst)))

(defun last-pair (lst)
  (if (null (cdr lst))
      lst
      (last-pair (cdr lst))))

(defun append! (lst1 lst2)
  (set-cdr! (last-pair lst1) lst2)
  lst1)

(defun make-cycle (x)
  (set-cdr! (last-pair x) x)
  x)

(let ((x (list 'a 'b 'c)))
  (show (cyclep x))
  (show (cyclep (make-cycle x)))
  (show "3.19 end"))


;;;;3.22

(defun make-queue ()
  (let ((front-ptr '())
	(rear-ptr '()))
    (labels ((set-front-ptr! (item)
	       (setq front-ptr item))
	     (set-rear-ptr! (item)
	       (setq rear-ptr item))
	     
	     (dispatch (m)
	       (cond ((eq m 'front-ptr) front-ptr)
		     ((eq m 'rear-ptr) rear-ptr)
		     ((eq m 'set-front-ptr!) #'set-front-ptr!)
		     ((eq m 'set-rear-ptr!) #'set-rear-ptr!)
		     (t
		      (error "not this inner method -- make-queue~A" m)))))
      #'dispatch)))

(defun front-ptr (queue) (funcall queue 'front-ptr))
(defun rear-ptr (queue) (funcall queue 'rear-ptr))
(defun set-front-ptr! (queue item)
  (funcall (funcall queue 'set-front-ptr!)
	   item))
(defun set-rear-ptr! (queue item)
  (funcall (funcall queue 'set-rear-ptr!)
	   item))

(defun empty-queue? (queue)
  (null (front-ptr queue)))

(defun front-queue (queue)
  (if (empty-queue? queue)
      (error "front called with an empty queue ~A" queue)
      (car (front-ptr queue))))

(defun insert-queue! (queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
	   (set-front-ptr! queue new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue)
	  (t
	   (set-cdr! (rear-ptr queue) new-pair)
	   (set-rear-ptr! queue new-pair)
	   queue))))
(defun delete-queue! (queue)
  (cond ((empty-queue? queue)
	 (error "delete! called with an empty queue~A" queue))
	(t
	 (set-front-ptr! queue (cdr (front-ptr queue)))
	 queue)))
	   
;;;;3.23


