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
;;思路是将数据部分做成双向链表。这样通过一个数据就能在一步之内找到下一个或者上一个。

(defun make-dlink (val) (cons (cons val 'start) 'end))
(defun pre-dlink (x) (cdar x))
(defun nxt-dlink (x) (cdr x))
(defun val-dlink (x) (caar x))
(defun start-dlink? (x) (eq (pre-dlink x) 'start))
(defun end-dlink? (x) (eq (nxt-dlink x) 'end))
(defun set-pre! (x item) (set-cdr! (car x) item))
(defun set-nxt! (x item) (set-cdr! x item))
(defun pre-insert-dlink! (x1 x2)
  (set-nxt! x2 x1)
  (set-pre! x1 x2))
(defun nxt-insert-dlink! (x1 x2)
  (set-nxt! x1 x2)
  (set-pre! x2 x1))
(defun separate-dlink (x1 x2) 
  (set-nxt! x1 'end)
  (set-pre! x2 'start))
(defun print-dlink (x)
  (cond ((eq x 'end)
	 nil)
	(t
	 (cons (val-dlink x)
	       (print-dlink (nxt-dlink x))))))

(defun make-deque () (cons '() '()))
(defun front-ptr-deque (queue) (car queue))
(defun rear-ptr-deque (queue) (cdr queue))
(defun set-front-deque-ptr! (queue item) (set-car! queue item))
(defun set-rear-deque-ptr! (queue item) (set-cdr! queue item))
(defun empty-deque? (queue) (null (front-ptr-deque queue)))
(defun front-deque (queue)
  (if (empty-deque? queue)
      (error "front called empty deque/~A" queue)
      (front-ptr-deque queue)))
(defun rear-deque (queue) 
  (if (empty-deque? queue)
      (error "rear called empty deque~A" queue)
      (rear-ptr-deque queue)))
(defun front-insert-deque! (queue item)
  (let ((new-dlink (make-dlink item)))
    (cond ((empty-deque? queue)
	   (set-front-deque-ptr! queue new-dlink)
	   (set-rear-deque-ptr! queue new-dlink)
	   queue)
	  (t
	   (pre-insert-dlink! (front-deque queue) new-dlink)
	   (set-front-deque-ptr! queue new-dlink)
	   queue))))

(defun rear-insert-deque! (queue item)
  (let ((new-dlink (make-dlink item)))
    (cond ((empty-deque? queue)
	   (set-front-deque-ptr! queue new-dlink)
	   (set-rear-deque-ptr! queue new-dlink)
	   queue)
	  (t
	   (nxt-insert-dlink! (rear-deque queue) new-dlink)
	   (set-rear-deque-ptr! queue new-dlink)
	   queue))))
(defun front-delete-deque! (queue)
  (cond ((empty-deque? queue)
	 (error "delete empty deque ~A" queue))
	(t
	 (let ((old-front (front-deque queue))
	       (new-front (nxt-dlink (front-deque queue))))
	   (cond ((end-dlink? old-front)
		  (set-front-deque-ptr! queue nil)
		  (set-rear-deque-ptr! queue nil))
		 (t
		  (set-front-deque-ptr! queue new-front)
		  (separate-dlink old-front new-front)
		  queue))))))
(defun rear-delete-deque! (queue)
  (if (empty-deque? queue)
      (error "delete empty deque ~A" queue)
      (let ((old-rear (rear-deque queue))
	    (new-rear (pre-dlink (rear-deque queue))))
	(cond ((start-dlink? old-rear)
	       (set-rear-deque-ptr! queue nil)
	       (set-front-deque-ptr! queue nil))
	      (t
	       (set-rear-deque-ptr! queue new-rear)
	       (separate-dlink new-rear old-rear)
	       queue)))))
(defun print-deque (queue)
  (cond ((empty-deque? queue)
	 nil)
	(t
	 (print-dlink (front-deque queue)))))

(let ((dq (make-deque)))
  (show (print-deque dq))
  (show (print-deque (front-insert-deque! dq 'a)))
  (show (print-deque (front-insert-deque! dq 'b)))
  (show (print-deque (front-insert-deque! dq 'b)))
  (show (print-deque (rear-insert-deque! dq 'b)))
  (show (print-deque (rear-insert-deque! dq 'b)))
  (show (print-deque (front-delete-deque! dq)))
  (show (print-deque (rear-delete-deque! dq)))
  (show (print-deque (rear-delete-deque! dq)))
  (show (print-deque (rear-delete-deque! dq)))
  (show (print-deque (rear-delete-deque! dq)))
 
  (show "3.23 end"))



(defun lookup (key table)
  (let ((record (my-assoc key (cdr table))))
    (if record
	(cdr record)
	nil)))

(defun my-assoc (key records)
  (cond ((null records) nil)
	((equal key (caar records))
	 (car records))
	(t
	 (my-assoc key (cdr records)))))

(defun insert! (key value table)
  (let ((record (my-assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) (cdr table))))
    'ok))

;;ps 无赋值的版本可以这样
(defun my-insert (key value table)
  (let ((record (my-assoc key (cdr table))))
    (if record
	(cons (car table)
	      (mapcar #'(lambda (r)
			  (if (equal (car r) key)
			      (cons key value)))
		      (cdr table)))
	(cons (car table)
	      (cons (cons key value) (cdr table))))))

(defun make-table () (list '*table*))
(let ((table (make-table)))
  (show (insert! 'a 1 table))
  (show table)
  (show (my-insert 'b 2 table))
  (show table)
  (show "test end"))

;;无赋值的版本最后还是要赋值才能更新table的。但是把赋值操作减少到了一个。


(defun lookup (key-1 key-2 table)
  (let ((subtable (my-assoc key-1 (cdr table))))
    (if subtable
	(let ((record (my-assoc key-2 (cdr subtable))))
	  (if record
	      (cdr record)
	      nil))
	nil)))

(defun insert! (key-1 key-2 value table)
  (let ((subtable (my-assoc key-1 (cdr table))))
    (if subtable
	(let ((record (my-assoc key-2 (cdr subtable))))
	  (if record
	   (set-cdr! record value)
	   (set-cdr! subtable (cons (cons key-2 value)
				    (cdr subtable)))))
	(set-cdr! table (cons (list key-1 
				    (cons key-2 value))
			      (cdr table))))
    'ok))

(defun replace-value (table key fval)
  (cons (car table)
	(mapcar #'(lambda (st)
		    (if (equal (car st) key)
			(funcall fval st)
			st))
		(cdr table))))
(defun my-insert (key-1 key-2 value table)
  (let ((subtable (my-assoc key-1 (cdr table))))
    (if subtable
	(let ((record (my-assoc key-2 (cdr subtable))))
	  (if record
	      (replace-value table
			     key-1 
			     #'(lambda (st)
			       (replace-value st key-2 #'(lambda (r)
							   (cons key-2 value)))))
	      (replace-value table
			     key-1
			     #'(lambda (st)
				 (cons (car st)
				       (cons (cons key-2 value)
					     (cdr st)))))))
	(cons (car table)
	      (cons (list key-1 (cons key-2 value))
		    (cdr table))))))

(let ((table (make-table)))
  (show (insert! 'a 'a 1 table))
  (show (insert! 'a 'c 3 table))
  (show table)
  (show (my-insert 'b 'b 2 table))
  (show (my-insert 'a 'a 2 table))
  (show (my-insert 'a 'b 2 table))
  (show "end"))


(defun make-table ()
  (let ((local-table (list '*table*)))
    (labels ((lookup (key-1 key-2)
	       (let ((subtable (my-assoc key-1 (cdr local-table))))
		 (if subtable
		     (let ((record (my-assoc key-2 (cdr subtable))))
		       (if record
			   (cdr record)
			   nil))
		     nil)))
	     (insert! (key-1 key-2 value)
	       (let ((subtable (my-assoc key-1 (cdr local-table))))
		 (if subtable
		     (let ((record (my-assoc key-2 (cdr subtable))))
		       (if record
			   (set-cdr! record value)
			   (set-cdr! subtable (cons (cons key-2 value)
						    (cdr subtable)))))
		     (set-cdr! local-table
			       (cons (list key-1 (cons key-2 value))
				     (cdr local-table))))
		 'ok))
	     (dispatch (m)
	       (cond ((eq m 'lookup-proc) #'lookup)
		     ((eq m 'insert-proc!) #'insert!)
		     (t
		      (error "unkown operation -- table~A" m)))))
      #'dispatch)))

(let ((operation-table (make-table)))
  (let ((get (funcall operation-table 'lookup-proc))
	(put (funcall operation-table 'insert-proc!)))
    (funcall put 'a 'a 1)
    (funcall put 'a 'b 2)
    (show (funcall get 'a 'b))
    (show "end")))

;;;;3.24
(defun make-table (same-key?)
  (let ((table (list '*table*)))
    (labels ((my-assoc (key records)
	       (cond ((null records) nil)
		     ((funcall same-key? key (caar records))
		      (car records))
		     (t
		      (my-assoc key (cdr records)))))
	     (lookup (key-1 key-2)
	       (let ((subtable (my-assoc key-1 (cdr table))))
		 (if subtable 
		     (let ((record (my-assoc key-2 (cdr subtable))))
		       (if record
			   (cdr record)
			   nil))
		     nil)))
	     (insert! (key-1 key-2 value)
	       (let ((subtable (my-assoc key-1 (cdr table))))
		 (if subtable
		     (let ((record (my-assoc key-2 (cdr subtable))))
		       (if record
			   (set-cdr! record value)
			   (set-cdr! subtable (cons (cons key-2 value)
						    (cdr subtable)))))
		     (set-cdr! table (cons (list key-1 (cons key-2 value))
					   (cdr table))))))
	     (dispatch (m)
	       (cond ((eq m 'lookup-proc) #'lookup)
		     ((eq m 'insert-proc!) #'insert!)
		     (t
		      (error "unkown operation --table~A" m)))))
      #'dispatch)))
	       
;;;;3.25
(defun lookup (keys table)
  (let ((record (my-assoc (car keys) (cdr table))))
    (if record
	(cond ((null (cdr keys))
	       (cdr record))
	      (t
	       (lookup (cdr keys) record)))
	nil)))
(defun insert! (keys value table)
  (let ((record (my-assoc (car keys) (cdr table))))
    (if record
	;;有记录
	(cond ((null (cdr keys))
	       ;;最后一个key
	       (set-cdr! record value))
	      (t
	       ;;不是最后一个key
	       (insert! (cdr keys) value record)))
	;;无记录
	(cond ((null (cdr keys))
	       ;;最后一个key
	       (set-cdr! table (cons (cons (car keys) value)
				     (cdr table))))
	      (t
	       ;;不是最后一个key,(cadr table)指的是新加入key的表的位置。
	       (set-cdr! table (cons (list (car keys)) 
				     (cdr table)))
	       (insert! (cdr keys) value (cadr table)))))
    'ok))
(let ((table (list '*table*))
      (keys1 '(a a a a a a a a a))
      (keys2 '(a a a a a b ab ab a))
      (keys3 '(j a jfkdf j asdk fjsl fla f))
      (keys4 '(i jf jalj fo jlaj fo j oai fo)))
  (insert! keys1 1 table)
  (show table)
  (insert! keys2 2 table)
  (show table)
  (insert! keys3 3 table)
  (show table)
  (insert! keys4 4 table)
  (show table)
  (show (lookup keys4 table))
  (show "3.25 end"))


;;;;3.26
;;先定义二叉树
(defun entry-tree (tree)
  (car tree))
(defun left-branch-tree (tree)
  (cadr tree))
(defun right-branch-tree (tree)
  (caddr tree))
(defun key (entry) (car entry))
(defun value (entry) (cdr entry))
(defun search-tree (key tree)
  (cond ((null tree) nil)
	((= key (key (entry tree)))
	 (entry tree))
	((< key (key (entry tree)))
	 (search-tree key (left-branch-tree tree)))
	((> key (key (entry tree)))
	 (search-tree key (right-branch-tree tree)))))
(defun adjoin-tree (entry tree)
  (cond ((null tree) (make-tree entry '() '()))
	((< (key entry) (key (entry-tree tree)))
	 (make-tree (entry-tree tree)
		    (adjoin-tree entry (left-branch-tree tree))
		    (right-branch-tree tree)))
	((> (key entry) (key (entry-tree tree)))
	 (make-tree (entry-tree tree)
		    (left-branch-tree tree)
		    (adjoin-tree entry (right-branch-tree tree))))
	((= (key entry) (key (entry-tree tree)))
	 (make-tree entry
		    (left-branch-tree tree)
		    (right-branch-tree tree)))))

(defun make-table ()
  (let ((table (list '*table*)))
    (labels ((my-assoc (key records)
	       (search-tree key records))
	     (lookup (key)
	       (let ((record (my-assoc key (cdr table))))
		 (if record
		     (value record)
		     nil)))
	     (insert! (key value)
	       (let ((record (my-assoc key (cdr table))))
		 (if record
		     (set-cdr! record value)
		     (set-cdr! table (cons (car table)
					   (adjoin-tree (cons key value)
							(cdr table)))))))
	     (dispatch (m)
	       (cond ((eq m 'get) #'lookup)
		     ((eq m 'insert!) #'insert!)
		     (t
		      (error "not this method ~A" m)))))
      #'dispatch)))
		   
	
;;;;3.27

(defun memo-fib ()
  (memoize #'(lambda (n)
	       (cond ((= n 0) 0)
		     ((= n 1) 1)
		     (t
		      (+ (funcall (memo-fib) (- n 1))
			 (funcall (memo-fib) (- n 2))))))))
(defun memoize (f)
  (let ((table (make-table)))
    #'(lambda (x)
	(let ((previously-computed-result (lookup x table)))
	  (or previously-computed-result
	      (let ((result (funcall f x)))
		(insert! x result table)
		result))))))

;;换成(memoize fib)能工作但是就没有记忆优化了。

;;;;3.3.4数字电路模拟器

(defun half-adder (a b s c)
  (let ((d (make-wire))
	(e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))
(defun full-adder (a b c-in sum c-out)
  (let ((s (make-wire))
	(c1 (make-wire))
	(c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))
(defun inverter (input output)
  (labels ((invert-input ()
	     (let ((new-value (logical-not (get-signal input))))
	       (after-delay *inverter-delay*
			    #'(lambda ()
				(set-signal! output new-value))))))
    (add-action! input #'invert-input)
    'ok))
(defun logical-not (s)
  (cond ((= s 0) 1)
	((= s 1) 0)
	(t
	 (error "invalid signal ~A" s))))
(defun and-gate (a1 a2 output)
  (labels ((and-action-procedure ()
	     (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
	       (after-delay *and-gate-delay*
			    #'(lambda ()
				(set-signal! output new-value))))))
    (add-action! a1 #'and-action-procedure)
    (add-action! a2 #'and-action-procedure)
    'ok))
(defun logical-and (s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
	(t 0)))
;;;;3.28
(defun or-gate (a1 a2 output)
  (labels ((or-action-procedure ()
	     (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
	       (after-delay *or-gate-delay*
			    #'(lambda ()
				(set-signal! output new-value))))))
    (add-action! a1 #'or-action-procedure)
    (add-action! a2 #'or-action-procedure)
    'ok))
(defun logical-or (s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
	(t 0)))
;;;;3.29
(defun or-gate-2 (a1 a2 output)
  (let ((na1 (make-wire))
	(na2 (make-wire))
	(na1a2 (make-wire)))
    (inverter a1 na1)
    (inverter a2 na2)
    (and-gate na1 na2 na1a2)
    (inverter na1a2 output)
    'ok))
;;;;3.30
(defun ripple-carry-adder (ak bk c-in sk c-out)
  (cond ((null ak) (list c-out sk))
	(t
	 (let ((c (make-wire)))
	   (full-adder (car ak) (car bk) c-in (car sk) c-out)
	   (adder (cdr ak) (cdr bk) c-out (cdr sk) c)))))

;;;((2*and+not+or)*2+or)*n=4n*and+2n*not+3n*or


;;;;线路的表示
(defun make-wire ()
  (let ((signal-value 0)
	(action-procedures '()))
    (labels ((set-my-signal! (new-value)
	       (if (not (= signal-value new-value))
		   (progn (setq signal-value new-value)
			  (call-each action-procedures))
		   'done))
	     (accept-action-procedure! (proc)
	       (setq action-procedures (cons proc action-procedures))
	       (funcall proc))
	     (dispatch (m)
	       (cond ((eq m 'get-signal) signal-value)
		     ((eq m 'set-signal!) #'set-my-signal!)
		     ((eq m 'add-action!) #'accept-action-procedure!)
		     (t
		      (error "unkown operation -- wire~A" m)))))
      #'dispatch)))

(defun call-each (procedures)
  (if (null procedures)
      'done
      (progn 
	(funcall (car procedures))
	(call-each (cdr procedures)))))

(defun get-signal (wire)
  (funcall wire 'get-signal))
(defun set-signal! (wire new-value) 
  (funcall (funcall wire 'set-signal!) new-value))
(defun add-action! (wire action-procedure)
  (funcall (funcall wire 'add-action!) action-procedure))


(defun after-delay (delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
		  action
		  the-agenda))
(defun propagate ()
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
	(funcall first-item)
	(remove-first-agenda-item! the-agenda)
	(propagate))))
;;;;3.31
;;如果不增加这个语句。那么只有在信号发生变化时才会在待处理表中添加待处理函数。初始定义功能模块时不会添加待处理函数。这就会造成定义完成线路后执行(propagate)驱动函数不会执行该线路。因为待处理表此时为空。

(defun make-time-segment (time queue)
  (cons time queue))
(defun segment-time (s) (car s))
(defun segment-queue (s) (cdr s))

(defun make-agenda () (list 0))
(defun current-time (agenda) (car agenda))
(defun set-current-time! (agenda time)
  (set-car! agenda time))
(defun segments (agenda) (cdr agenda))
(defun set-segments! (agenda segments)
  (set-cdr! agenda segments))
(defun first-segment (agenda)
  (car (segments agenda)))
(defun rest-segments (agenda)
  (cdr (segments agenda)))
(defun empty-agenda? (agenda)
  (null (segments agenda)))

(defun add-to-agenda! (time action agenda)
  (labels ((belongs-before? (segments)
	     (or (null segments)
		 (< time (segment-time (car segments)))))
	   (make-new-time-segment (time action)
	     (let ((q (make-queue)))
	       (insert-queue! q action)
	       (make-time-segment time q)))
	   (add-to-segments! (segments)
	     (if (= (segment-time (car segments)) time)
		 (insert-queue! (segment-queue (car segments))
				action)
		 (let ((rest (cdr segments)))
		   (if (belongs-before? rest)
		       (set-cdr! segments
				 (cons (make-new-time-segment time action)
				       (cdr segments)))
		       (add-to-segments! rest))))))
    (let ((segments (segments agenda)))
      (if (belongs-before? segments)
	  (set-segments! agenda
			 (cons (make-new-time-segment time action)
			       segments))
	  (add-to-segments! segments)))))


(defun remove-first-agenda-item! (agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
	(set-segments! agenda (rest-segments agenda)))))
(defun first-agenda-item (agenda)
  (if (empty-agenda? agenda)
      (error "agenda is empty --first-agenda-item")
      (let ((first-seg (first-segment agenda)))
	(set-current-time! agenda (segment-time first-seg))
	(front-queue (segment-queue first-seg)))))

;;;;3.32

;;假设线路a1和a2,a1信号首先变化，状态从0 1 变 1 0，a1信号首先变化时在待处理表添加函数(lambda () (set-signal! output 1)),a2变化时则添加 (lambda () (set-signal! output 0)),先进先出最后a2的函数为最后状态。先进后出a1的待处理函数为最后状态。是不同的结果。

(defparameter *inverter-delay* 2)
(defparameter *and-gate-delay* 3)
(defparameter *or-gate-delay* 5)
(defvar the-agenda (make-agenda))
(setq the-agenda (make-agenda))
;;;定义一个监测器
(defun probe (name wire)
  (add-action! wire
	       #'(lambda ()
		  (format t "~%~A  ~A  new-value = ~A~%" name (current-time the-agenda) (get-signal wire)))))

(let ((input-1 (make-wire))
      (input-2 (make-wire))
      (sum (make-wire))
      (carry (make-wire)))
  (probe 'sum sum)
  (probe 'carry carry)
  (half-adder input-1 input-2 sum carry)
  (set-signal! input-1 1)
  (propagate)
  (set-signal! input-2 1)
  (propagate))

;;;电路设计只是一个例子，其中的设计思路是值得思考的。好好想一想，在大脑里模拟整个程序的运行过程。

;;;;约束的传播

(defvar *C* (make-connector))
(defvar *F* (make-connector))
(celsius-fahrenheit-converter *C* *F*)

(defun celsisus-fahrenheit-converter (*C* *F*)
  (let ((u (make-connector))
	(v (make-connector))
	(w (make-connector))
	(x (make-connector))
	(y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(defun adder (a1 a2 sum)
  (labels ((process-new-value ()
	     (cond ((and (has-value? a1) (has-value? a2))
		    (set-value! sum
				(+ (get-value a1) (get-value a2))
				me))
		   ((and (has-value? a1) (has-value? sum))
		    (set-value! a2
				(- (get-value sum) (get-value a1))
				me))
		   ((and (has-value? a2) (has-value? sum))
		    (set-value! a1
				(- (get-value sum) (get-value a2))
				me))))
	   (process-forget-value ()
	     (forget-value! sum me)
	     (forget-value! a1 me)
	     (forget-value! a2 me)
	     (process-new-value))
	   (me (request)
	     (cond ((eq request 'i-have-a-value)
		    (process-new-value))
		   ((eq request 'i-lost-my-value)
		    (process-forget-value))
		   (t
		    (error "unkown request --adder~A" request)))))
    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me))
(defun inform-about-value (constraint)
  (funcall constraint 'i-have-a-value))
(defun inform-about-no-value (constraint)
  (funcall constraint 'i-lost-my-value))

(defun multiplier (m1 m2 product)
  (labels ((process-new-value ()
	     (cond ((or (and (has-value? m1) (= (get-value m1) 0))
			(and (has-value? m2) (= (get-value m2) 0)))
		    (set-value! product 0 me))
		   ((and (has-value? m1) (has-value? m2))
		    (set-value! product
				(* (get-value m1) (get-value m2))
				me))
		   ((and (has-value? product) (has-value? m1))
		    (set-value! m2
				(/ (get-value product) (get-value m1))
				me))
		   ((and (has-value? product) (has-value? m2))
		    (set-value! m1
				(/ (get-value product) (get-value m2))
				me))))
	   (process-forget-value ()
	     (forget-value! product me)
	     (forget-value! m1 me)
	     (forget-value! m2 me)
	     (process-new-value))
	   (me (request)
	     (cond ((eq request 'i-have-a-value)
		    (process-new-value))
		   ((eq request 'i-lost-my-value)
		    (process-forget-value))
		   (t
		    (error "unkown request -- multiplier~A" request)))))
    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me))
(defun constant (value connector)
  (labels ((me (request)
	     (error "unkown request -- constant~A" request)))
    (connect connector me)
    (set-value! connector value me)
    me))
(defun probe (name connector)
  (labels ((print-probe (value)
	     (format t "~%probe: ~A = ~A~%" name value))
	   (process-new-value ()
	     (print-probe (get-value connector)))
	   (process-forget-value ()
	     (print-probe "?"))
	   (me (request)
	     (cond ((eq request 'i-have-a-value)
		    (process-new-value))
		   ((eq request 'i-lost-my-value)
		    (process-forget-value))
		   (t
		    (error "unkown request -- probe~A" request)))))
    (connect connector me)
    me))

(defun make-connector ()
  (let ((value nil)
	(informant nil)
	(constraints '()))
    (labels ((set-my-value (newval setter)
	       (cond ((not (has-value? me))
		      (setq value newval)
		      (setq informant setter)
		      (for-each-except setter
				       infor-about-value
				       constraints))
		     ((not (= value newval))
		      (error "constradiction ~A" (list value newval)))
		     (t
		      'ignored)))
	     (forget-my-value (retractor)
	       (if (eq retractor informant)
		   (progn (setq informant nil)
			  (for-each-except retractor
					   inform-about-no-value
					   constraints))
		   'ignored))
	     (connect (new-constraint)
	       (if (not (memq new-constraint constraints))
		   (setq constraints
			 (cons new-constraint constraints)))
	       (if (has-value? me)
		   (inform-about-value new-constraint))
	       'done)
	     (me (request)
	       (cond ((eq request 'has-value?)
		      (if informant t nil))
		     ((eq request 'value) value)
		     ((eq request 'set-value!) #'set-my-value)
		     ((eq request 'forget) #'forget-my-value)
		     ((eq request 'connect) #'connect)
		     (t
		      (error "unkown operation -- connector~A" request)))))
      #'me)))
