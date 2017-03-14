(defun square (x)
  (* x x))
;;;;2.1
(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))
(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer x) (denom y)))
	    (* (denom x) (denom y))))
(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))
(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))
(defun equal-ratp (x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))
(defun numer (x)
  (car x))
(defun denom (x)
  (cdr x))

(defun print-rat (x)
  (format t "~%~a/~a" (numer x) (denom x)))

(defun make-rat (x y)
 
  (let ((g (gcd x y)))
    (let ((x (/ x g))
	  (y (/ y g)))
      (if (< y 0)
	  (cons (- x) (- y))
	  (cons x y)))))

(let ((one-half (make-rat 1 2))
      (one-third (make-rat 1 3)))
  (print-rat (add-rat one-third one-third))
  (print-rat (mul-rat one-half one-third)))

;;;;2.2

(defun midpoint-segment (x)
  (div-point (add-point (start-segment x)
			(end-segment x))
	     2))

(defun make-segment (x y)
  (cons x y))

(defun start-segment (x)
  (car x))

(defun end-segment (x)
  (cdr x))

(defun add-point (x y)
  (make-point (+ (x-point x) (x-point y))
	      (+ (y-point x) (y-point y))))

(defun sub-point (x y)
  (make-point (- (x-point x) (x-point y))
	      (- (y-point x) (y-point y))))

(defun mul-point (x n)
  (make-point (* n (x-point x))
	      (* n (y-point x))))

(defun div-point (x n)
  (make-point (/ (x-point x) n)
	      (/ (y-point x) n)))

(defun equal-point (x y)
  (and (= (x-point x) (x-point y))
       (= (y-point x) (y-point y))))

(defun make-point (x y)
  (cons x y))

(defun x-point (x)
  (car x))

(defun y-point (x)
  (cdr x))

(defun print-point (x)
  (format t "(~a,~a)~%" (x-point x) (y-point x)))

(let ((d1 (make-point 2 2))
      (d2 (make-point 4 6))
      (d3 (make-point 5 13)))
  (let ((l1 (make-segment d1 d2))
	(l2 (make-segment d1 d3))
	(l3 (make-segment d2 d3)))
    (print-point (midpoint-segment l1))
    (print-point (midpoint-segment l2))
    (print-point (midpoint-segment l3))))

;;;;2.3
(defun area-rec (x)
  (* (width-rec x)
     (height-rec x)))

(defun perimeter-rec (x)
  (* (+ (width x)
	(height x))
     2))

(defun make-rec (width height)
  (cons width height))

(defun width-rec (rec)
  (car rec))

(defun height-rec (rec)
  (cdr rec))

;;;计算线段长度
(defun length-segment(l)
  (let ((dl (sub-point (start-segment l) (end-segment l))))
    (let ((dx (x-point dl))
	  (dy (y-point dl)))
      (sqrt (+ (square dx)
	       (square dy))))))

;;;rec另一种表示方法
(defun make-rec-2 (p1 p2)
  (cons (abs (- (x-point p1) (x-point p2)))
	(abs (- (y-point p1) (y-point p2)))))

;;;test
(let ((p1 (make-point 1 1))
      (p2 (make-point 1 4))
      (p3 (make-point 4 4))
      (p4 (make-point 4 1)))
  (let ((s1 (make-segment p1 p2))
	(s2 (make-segment p1 p4))
	(s3 (make-segment p2 p3))
	(s4 (make-segment p3 p4)))
    (let ((rec1 (make-rec (length-segment s1)
			  (length-segment s2)))
	  (rec2 (make-rec-2 p1 p3)))
      (format t "~a~%" (area-rec rec1))
      (format t "~a~%" (area-rec rec2)))))


;;;;2.4
(labels ((my-cons (x y)
	   #'(lambda (m) (funcall m x y)))
	 (my-car (z)
	   (funcall z #'(lambda (p q) p)))
	 (my-cdr (z)
	   (funcall z #'(lambda (p q) q))))
  (format t "~a" (my-car (my-cons 1 2)))
  (format t "~%~a" (my-cdr (my-cons 1 2))))

;;;;2.5

;;;根据幂函数，a,b取负无穷时表达式的值也不会为0

(labels ((cons-1 (a b)
	   (* (expt 2 a) (expt 3 b)))
	 (car-1 (z)
	   (cond ((= (mod z 2) 0)
		  (+ 1 (car-1 (/ z 2))))
		 (t 0)))
	 (cdr-1 (z)
	   (labels ((iter (res)
		      (if (/= (mod z (expt 3 res)) 0)
			  (- res 1)
			  (iter (+ res 1)))))
	     (iter 0))))
  (format t "~%~A~%" (cons-1 3 4))
  (format t "~A~%" (car-1 (cons-1 3 4)))
  (format t "~A~%" (cdr-1 (cons-1 3 4))))
	      
;;;;2.6

(labels ((zero ()
	   #'(lambda (f)
	       #'(lambda (x) x)))
	 ;;相加之后返回的是一个数字，数字定义时是没有形参的。所以返回一个没有形参的匿名函数来指代返回的数字值。
	 (add-1 (n)
	   #'(lambda ()
	       #'(lambda (f)
		   #'(lambda (x)
		       (funcall f
				(funcall (funcall (funcall n) f)
					 x))))))
	 (one ()
	   #'(lambda (f)
	       #'(lambda (x)
		   (funcall f x))))
	 ;;同add-1
	 (add (n m)
	   #'(lambda ()
	       #'(lambda (f)
		   #'(lambda (x)
		       (funcall (funcall (funcall n) f)
				(funcall (funcall (funcall m) f)
					 x))))))
	 (print-num (n)
	   (format t "~%~A" (funcall (funcall (funcall n)
					      #'inc)
				     0)))
	 ;;仅仅为了打印值方便阅读。
	 (inc (x)
	   (+ x 1)))
  (format t "~%~A~%" (funcall (funcall (funcall (add-1 #'zero)) #'inc) 0))
  (print-num #'zero)
  (print-num (add-1 #'zero))
  (print-num #'one)
  (print-num (add #'one #'one))
  )


;;;;算数区间

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

;;;;2.7

(defun make-interval (a b)
  (cons a b))

(defun lower-bound (iter)
  (car iter))

(defun upper-bound (iter)
  (cdr iter))

;;;;2.8

(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (lower-bound y))
		 (- (upper-bound x) (upper-bound y))))

;;;;2.9

;;简单，答案略。

;;;;2.10

(defun div-interval (x y)
  (if (< (* (lower-bound y)
	    (upper-bound y))
	 0)
      (format t "error, div 0")
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
				     (/ 1.0 (lower-bound y))))))

;;;;2.11

(defun mul-interval (x y)
  (let ((x1 (lower-bound x))
	(x2 (upper-bound x))
	(y1 (lower-bound y))
	(y2 (upper-bound y)))
    
    (cond ((>= x1 0) (cond ((>= y1 0)
			    (make-interval (* x1 y1) (* x2 y2)))
			   ((< y2 0)
			    (make-interval (* x2 y2) (* x1 y1)))
			   (t (make-interval (* x2 y1) (* x2 y2)))))
	  ((< x2 0) (cond ((>= y1 0)
			   (make-interval (* x2 y2) (* x1 y1)))
			  ((< y2 0)
			   (make-interval (* x1 y1) (* x2 y2)))
			  (t (make-interval (* x2 y2) (* x2 y1)))))
	  (t (cond ((>= y1 0)
		    (make-interval (* x1 y2) (* x2 y2)))
		   ((< y2 0)
		    (make-interval (* x2 y2) (* x1 y2)))
		   (t (make-interval (min (* x1 y2) (* x2 y1))
				     (max (* x1 y1) (* x2 y2)))))))))


;;;;2.12

(defun make-center-percent (c p)
  (make-interval (- c (* c p 0.01)) (+ c (* c p 0.01))))

(defun center (i)
  (let ((x (lower-bound i))
	(y (upper-bound i)))
    (/ (+ x y) 2)))

(defun percent (i)
  (let ((x (lower-bound i))
	(y (upper-bound i)))
    (* 100
       (/ (/ (- y x)
	     2)
	  (center i)))))

;;;;2.13

;;;((x*(1-p1)),x*(1+p1)) * ((y*(1-p2)),(y*(1+p2))) 假定都为正可以推导出
;;;(x*y*(1-p1)*(1-p2)),(x*y*(1+p1)*(1+p2))化简
;;;xy*(1-(p1+p2-p1p2)),xy*(1+(p1+p2+p1p2)) 在p很小时，p1p2可以忽略为0
;;;得新乘积得百分比为(p1+p2)。

;;;;2.14

(let ((r1 (make-center-percent 4 0.1))
      (r2 (make-center-percent 2 0.1))
      (one (make-interval 1 1)))
  (labels ((par1 (r1 r2)
	     (div-interval (mul-interval r1 r2)
			   (add-interval r1 r2)))
	   (par2 (r1 r2)
	     (div-interval one
			   (add-interval (div-interval one r1)
					 (div-interval one r2)))))
    (format t "~%")
    (princ (par1 r1 r2))
    (format t "~%")
    (princ (par2 r1 r2))
    (format t "~%")
    (princ (div-interval r1 r1))
    (format t "~%")
    (princ (div-interval r1 r2))))
		      
	  
;;;;2.15

;;;那个人说的是对的。确定得范围量比如（1 1）不会影响结果，但是不确定的量算会增加误差范围。

;;;;2.16

;;;以我现在的知识储备来答，不可能。除非有个程序能自动把运算先化简为最优形式（不确定量最少）。然后再计算。

;;;;2.17

(defun last-pair (lst)
  (if (null (cdr lst))
      lst
      (last-pair (cdr lst))))

;;;;2.18

;;;recurse
(defun my-reverse (lst)
  (if (null lst)
      nil
      (append (my-reverse (cdr lst)) 
	      (cons (car lst) nil))))

;;;iter
(defun my-reverse (lst)
  (labels ((iter (lst res)
	     (if (null lst)
		 res
		 (iter (cdr lst)
		       (cons (car lst) res)))))
    (iter lst nil)))

;;;;2.19

(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-morep coin-values)) 0)
	(t 
	 (+ (cc amount 
		(except-first-denomination coin-values))
	    (cc (- amount 
		   (first-denomination coin-values))
		coin-values)))))

(defun no-morep (lst)
  (null lst))

(defun except-first-denomination (lst)
  (cdr lst))

(defun first-denomination (lst)
  (car lst))

(let ((us-coins (list 50 25 10 5 1))
      (uk-coins (list 100 50 20 10 5 2 1 0.5)))
  (format t "~%~A" (cc 100 us-coins))
  (format t "~%~A" (cc 100 uk-coins)))

;;;;2.20

;;;recu
(defun same-parity (x &rest rest)
  (labels ((filter (f lst)
	     (if (null lst)
		 '()
		 (if (funcall f (car lst))
		     (cons (car lst) (filter f (cdr lst)))
		     (filter f (cdr lst))))))
    (if (evenp x)
	(cons x (filter #'evenp rest))
	(cons x (filter #'oddp rest)))))
					  
;;;iter
(defun same-parity (x &rest rest)
  (let ((m (mod x 2)))
    (labels ((iter (lst res)
	       (if (null lst)
		   (reverse res)
		   (iter (cdr lst)
			 (if (= (mod (car lst) 2) m)
			     (cons (car lst) res)
			     res)))))
      (iter rest (list x)))))

;;;;2.21

(defun square-list (lst)
  (mapcar #'(lambda (x) (* x x)) lst))

(defun square-list (items)
  (if (null items)
      nil
      (cons (square (car items))
	    (square-list (cdr items)))))

;;;;2.22

(defun square-list (items)
  (labels ((iter (lst res)
	     (if (null lst)
		 (reverse res)
		 (iter (cdr lst)
		       (cons (square (car lst))
			    res)))))
    (iter items '())))

;;Louis第一个程序会倒过来是因为cons 参数的顺序问题
;;第二个还是不行是因为表的结尾标识符没有即cons的最后一个参数要是nil结尾标识符。

;;;;2.23

(defun for-each (f lst)
  (if (null lst)
      nil
      (let ((x (car lst)))
	(funcall f x)
	(for-each f (cdr lst)))))

;;;;2.24
;;;(1 (2 (3 4)))

;;;;2.25

(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))

(car (car '((7))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;;;;2.26

;;(1 2 3 4 5 6)
;;((1 2 3) 4 5 6)
;;((1 2 3) (4 5 6))

;;;;2.27

;;正常版本
(defun deep-reverse (lst)
  (cond ((null lst) nil)
	((not (consp (car lst)))
	 (append (deep-reverse (cdr lst))
		 (cons (car lst) nil)))
	(t (append (deep-reverse (cdr lst))
		   (list (deep-reverse (car lst)))))))

;;文艺版本
(defun deep-reverse (lst)
  (if (consp lst)
      (append (deep-reverse (cdr lst))
	      (list (deep-reverse (car lst))))
      lst))

;;;;2.28

(defun fringe (lst)
  (cond ((null lst) nil)
	((not (consp (car lst)))
	 (cons (car lst) (fringe (cdr lst))))
	(t (append (fringe (car lst))
		   (fringe (cdr lst))))))

;;;;2.29

;;;a

(defun make-mobile (l r)
  (list l r))

(defun make-branch (l s)
  (list l s))

(defun left-branch (m)
  (car m))

(defun right-branch (m)
  (car (cdr m)))

(defun branch-length (b)
  (car b))

(defun branch-structure (b)
  (car (cdr  b)))

;;;b

(defun total-weight (m)
  (labels ((branch-weight (b)
	     (let ((s (branch-structure b)))
	       (cond ((not (consp s)) s)
		     (t (total-weight s))))))
    (let ((lb (left-branch m))
	  (rb (right-branch m)))
      (+ (branch-weight lb)
	 (branch-weight rb)))))


;;;c

(defun mobile-balance (m)
  (labels ((moment-of-force (b)
	     (let ((l (branch-length b))
		   (s (branch-structure b)))
	       (cond ((not (consp s)) (* l s))
		     (t (* l (total-weight s))))))
	   (branch-balance (b)
	     (cond ((not (consp (branch-structure b))) t)
		   (t (mobile-balance (branch-structure b))))))
    (let ((lb (left-branch m))
	  (rb (right-branch m)))
      (and (= (moment-of-force lb)
	      (moment-of-force rb))
	   (branch-balance lb)
	   (branch-balance rb)))))


;;;d

(defun make-mobile (l r)
  (cons l r))

(defun make-branch (l s)
  (cons l s))

(defun left-branch (m)
  (car m))

(defun right-branch (m)
  (cdr m))

(defun branch-length (b)
  (car b))

(defun branch-structure (b)
  (cdr b))


;;;;2.30

(defun square-tree (tree)
  (cond ((null tree) nil)
	((not (consp tree)) (square tree))
	(t (cons (square-tree (car tree))
		 (square-tree (cdr tree))))))

(defun square-tree (tree)
  (mapcar #'(lambda (x)
	   (if (consp x)
	       (square-tree x)
	       (square x)))
       tree))

;;;;2.31

(defun tree-map (f tree)
  (mapcar #'(lambda (x)
	   (if (consp x)
	       (tree-map f x)
	       (funcall f x)))
       tree))

;;;;2.32

(defun subsets (s)
  (if (null s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (mapcar #'(lambda (x)
				 (cons (car s) x))
			     rest)))))

;;;;序列作为一种约定的界面

(defun fib (n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(t (+ (fib (- n 1))
	      (fib (- n 2))))))

(defun fib (n)
  (labels ((next (res nex k)
	     (if (> k n)
		 res
		 (next nex (+ res nex) (+ k 1)))))
    (next 0 1 0)))


(defun sum-odd-squares (tree)
  (cond ((null tree) 0)
	((not (consp tree))
	 (if (oddp tree) tree 0))
	(t (+ (sum-odd-squares (car tree))
	      (sum-odd-squares (cdr tree))))))

(defun even-fibs (n)
  (labels ((next (k)
	     (if (> k n)
		 nil
		 (let ((f (fib k)))
		   (if (evenp f)
		       (cons f (next (+ k 1)))
		       (next (+ k 1)))))))
    (next 0)))

(defun filter (fpredicate sequence)
  (cond ((null sequence) nil)
	((funcall fpredicate (car sequence))
	 (cons (car sequence) (filter fpredicate (cdr sequence))))
	(t (filter fpredicate (cdr sequence)))))

(defun accumulate (fop initial sequence)
  (if (null sequence)
      initial
      (funcall fop
	       (car sequence)
	       (accumulate fop initial (cdr sequence)))))

;;;;2.33

(defun my-map (p sequence)
  (accumulate #'(lambda (x y)
		  (cons (funcall p x) y))
	      nil
	      sequence))
				 
(defun my-append (seq1 seq2)
  (accumulate #'cos seq2 seq1))

(defun my-length (sequence)
  (accumulate #'(lambda (x y)
		  (+ 1 y))
	      0 
	      sequence))

;;;;2.34

(defun horner-eval (x coefficient-sequence)
  (accumulate #'(lambda (this-coeff higher-terms)
		  (+ this-coeff
		     (* x higher-terms)))
	      0
	      coefficient-sequence))
;;;;2.35

(defun count-leaves (tree)
  (accumulate #'+
	      0
	      (mapcar #'(lambda (x)
			  (if (consp x)
			      (count-leaves x)
			      1))
		      tree)))

;;;;2.36

(defun accumulate-n (fop init seqs)
  (if (null (car seqs))
      nil
      (cons (accumulate fop init (mapcar #'(lambda (x) (car x)) seqs))
	    (accumulate-n fop init (mapcar #'(lambda (x) (cdr x)) seqs)))))

;;;;2.37

(defun dot-product (v w)
  (accumulate #'+ 0 (mapcar #'* v w)))

(defun matrix-*-vector (m v)
  (mapcar #'(lambda (x)
	      (dot-product v x))
	  m))

(defun transpose (mat)
  (accumulate-n #'(lambda (x y)
		    (cons x y))
		nil
		mat))

(defun matrix-*-matrix (m n)
  (let ((cols (transpose n)))
    (mapcar #'(lambda (x)
	     (matrix-*-vector cols x))
	 m)))
	
;;;;2.38

(defun fold-left (fop initial sequence)
  (labels ((iter (result rest)
	     (if (null rest)
		 result
		 (iter (funcall fop result (car rest))
		       (cdr rest)))))
    (iter initial sequence)))

;;需要满足参数交换不影响结果
;;;;2.39

(defun fold-right (fop initial sequence)
  (if (null sequence)
      initial
      (funcall fop
	       (car sequence)
	       (fold-right fop initial sequence))))

(defun fold-left (fop initial sequence)
  (labels ((iter (result rest)
	     (if (null rest)
		 result
		 (iter (funcall fop
				result
				(car rest))
		       (cdr rest)))))
    (iter initial sequence)))


(defun my-reverse (sequence)
  (fold-right #'(lambda (x y) (append y (list x)))
	      nil
	      sequence))

(defun my-reverse (sequence)
  (fold-left #'(lambda (x y) (cons y x))
	     nil
	     sequence))


;;;;嵌套映射

(defun enumerate-iterval (s e)
  (if (> s e)
      nil
      (cons s (enumerate-iterval (+ s 1) e))))

(defun flatmap (proc seq)
  (accumulate #'append nil (mapcar proc seq)))

(defun prime-sump (pair)
  (primep (+ (car pair) (cadr pair))))

(defun primep (n)
  (labels ((prime-test (k)
	     (cond ((> (square k) n) t)
		   ((= (mod n k) 0) nil)
		   (t (prime-test (+ k 1))))))
    (prime-test 2)))

(defun make-pair-sum (pair)
  (list (car pair)
	(cadr pair)
	(+ (car pair)
	   (cadr pair))))

(defun prime-sum-pairs (n)
  (mapcar #'make-pair-sum
       (filter #'prime-sump
	       (flatmap #'(lambda (i)
			    (mapcar #'(lambda (j)
				     (list i j))
				 (enumerate-iterval 1 (- i 1))))
			(enumerate-iterval 1 n)))))

;;;;2.40

(defun unique-pairs (n)
  (flatmap #'(lambda (i)
	       (mapcar #'(lambda (j) (list i j))
		       (enumerate-iterval 1 (- i 1))))
	   (enumerate-iterval 1 n)))

(defun prime-sum-pairs (n)
  (mapcar #'make-pair-sum
	  (filter #'prime-sump
		  (unique-pairs n))))
	       

;;;;2.41

(defun equal-s-sum (n s)
  (labels ((unique-pairs (n)
	     (flatmap #'(lambda (i)
			  (flatmap #'(lambda (j)
				       (mapcar #'(lambda (k) (list i j k))
					       (enumerate-iterval 1 (- j 1))))
				   (enumerate-iterval 1 (- i 1))))
		      (enumerate-iterval 1 n)))
	   (equal-sp (pair)
	     (= s (accumulate #'+ 0 pair))))
    (filter #'equal-sp
	    (unique-pairs n))))
					       
;;;;2.42

(defun queens (board-size)
  (labels ((queen-cols (k)
	     (if (= k 0)
		 (list '())
		 (filter
		  #'(lambda (positions) (safe? k positions))
		  (flatmap
		   #'(lambda (rest-of-queens)
		       (mapcar #'(lambda (new-row)
				   (adjoin-position new-row k rest-of-queens))
			       (enumerate-iterval 1 board-size)))
		   (queen-cols (- k 1)))))))
    (queen-cols board-size)))

(defun adjoin-position (row col seq)
  (cons (list col row)
	seq))

(defun safe? (k positions)
  (labels ((get-row ()
	     (cadar (filter #'(lambda (p) (= (car p) k))
			    positions)))
	   (test (test-row seq)
	     (let ((col (caar seq))
		   (row (cadar seq)))
	       (cond ((null seq) t)
		     ((= test-row row) nil)
		     ((= (abs (- test-row row))
			 (abs (- k col))) 
		      nil)
		     (t (test test-row (cdr seq))))))
	   (get-positions ()
	     (filter #'(lambda (p)
			 (< (car p) k))
		     positions)))

    ;(format t "~%~A~%" (get-row))
    ;(format t "~%~A~%" (get-positions))
    (test (get-row)
	  (get-positions))))


;;;;2.43

;;;对于map的每个元素，都对应一次时间为T的递归操作。所以时间按大约为T*T的操作。


;;;;2.44

(defun up-split (painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
	(below painter (beside smaller smaller)))))

;;;;2.45

(defun split (combine smaller-combine)
  (labels ((split-n (painter n)
	     (if (= n 0)
		 painter
		 (let ((smallter (split-n painter (- n 1))))
		   (funcall combine
			    painter
			    (funcall smaller-combine
				     smaller
				     smaller))))))
    #'split-n))
      

;;;;2.46

(defun make-vect (x y)
  (cons x y))
(defun xcor-vect (v)
  (car v))
(defun ycor-vect (v)
  (cdr v))
(defun add-vect (v m)
  (let ((x1 (xcor-vect v))
	(y1 (ycor-vect v))
	(x2 (xcor-vect m))
	(y2 (ycor-vect m)))
    (make-vect (+ x1 x2)
	       (+ y1 y2))))

(defun sub-vect (v m)
  (let ((x1 (xcor-vect v))
	(y1 (ycor-vect v))
	(x2 (xcor-vect m))
	(y2 (ycor-vect m)))
    (make-vect (- x1 x2)
	       (- y1 y2))))

(defun scale-vect (v s)
  (let ((x (xcor-vect v))
	(y (xcor-vect v)))
    (make-vect (* s x)
	       (* s y))))



(defun frame-coord-map (frame)
  #'(lambda (v)
      (add-vect (origin-frame frame)
		(add-vect (scale-vect (xcor-vect v)
				      (edge1-frame frame))
			  (scale-vect (ycor-vect v)
				      (edge2-frame frame))))))

;;;;2.47

(defun make-frame (origin edge1 edge2)
  (list origin edge1 edge2))

(defun origin-frame (frame)
  (car frame))

(defun edge1-frame (frame)
  (car (cdr frame)))

(defun edge2-frame (frame)
  (car (cdr (cdr frame))))

(defun make-frame (origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(defun origin-frame (frame)
  (car frame))

(defun edge1-frame (frame)
  (car (cdr frame)))

(defun edge2-frame (frame)
  (cdr (cdr frame)))

(defun segments->painter (segment-list)
  #'(lambda (frame)
      (for-each 
       #'(lambda (segment)
	   (draw-line
	    (funcall (frame-coord-map frame)
		     (start-segment segment))
	    (funcall (frame-coord-map frame)
		     (end-segment segment))))
       segment-list)))

;;;;2.48

(defun make-segment (v1 v2)
  (cons v1 v2))
(defun start-segment (segment)
  (car segment))
(defun end-segment (segment)
  (cdr segment))

;;;;2.49

;;;a
(defun border->painter ()
  (let ((v1 (make-vect 0 0))
	(v2 (make-vect 1 0))
	(v3 (make-vect 1 1))
	(v4 (make-vect 0 1)))
    (let ((s1 (make-segment v1 v2))
	  (s2 (make-segment v2 v3))
	  (s3 (make-segment v3 v4))
	  (s4 (make-segment v4 v1)))
      (segments->painter (list s1 s2 s3 s4)))))

;;;b
(defun corner->painter ()
  (let ((v1 (make-vect 0 0))
	(v2 (make-vect 1 0))
	(v3 (make-vect 1 1))
	(v4 (make-vect 0 1)))
    (let ((s1 (make-segment v1 v3))
	  (s2 (make-segment v2 v4)))
      (segments->painter (list s1 s2)))))

;;;c
(defun rhombus->painter ()
  (let ((v1 (make-vect 0.5 0))
	(v2 (make-vect 1 0.5))
	(v3 (make-vect 0.5 1))
	(v4 (make-vect 0 0.5)))
    (let ((s1 (make-segment v1 v2))
	  (s2 (make-segment v2 v3))
	  (s3 (make-segment v3 v4))
	  (s4 (make-segment v4 v1)))
      (segments->painter (list s1 s2 s3 s4)))))

;;;d


;;;;2.50

(defun transform-painter (painter origin corner1 corner2)
  #'(lambda (frame)
      (let ((m (frame-coord-map frame)))
	(let ((new-origin (funcall m origin)))
	  (funcall painter
		   (make-frame new-origin
			       (sub-vect (funcall m corner1) new-origin)
			       (sub-vect (funcall m corner2) new-origin)))))))

(defun flip-vert (painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(defun shrink-to-upper-right (painter)
  (transform-painter painter
		     (make-vect 0.5 0.5)
		     (make-vect 1.0 0.5)
		     (make-vect 0.5 1.0)))
(defun rotate90 (painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))
(defun squash-inwards (painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))
(defun beside (painter1 painter2)
  (let ((split-point (make-vect 0.5 0)))
    (let ((paint-left
	   (transform-painter painter1 
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0)
			      (make-vect 0.5 1.0))))
      #'(lambda (frame)
	  (funcall paint-left frame)
	  (funcall paint-right frame)))))

(defun flip-horiz (painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(defun rotate180 (painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))
(defun rotate270 (painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))
;;;;2.51
(defun below (painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-upper 
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0)))
	  (paint-down 
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point)))
      #'(lambda (frame)
	  (funcall paint-upper frame)
	  (funcall paint-down frame)))))
(defun below (painter1 painter2)
  (rotate270
   (beside (rotate90 painter2)
	   (rotate90 painter1))))


;;;;2.52
;;;a

;;;b
(defun corner-split (painter n)
  (if (= n 0)
      painter
      (blow (beside painter (right-split painter (- n 1)))
	    (beside (up-split painter (- n 1)) (corner-split painter (- n 1))))))

;;;c



;;;;2.53

(defun my-memq (item lst)
  (cond ((null lst) nil)
	((eq item (car lst)) lst)
	(t (my-memq item (cdr lst)))))

;(a b c)
;((george))
;((y1 y2))
;(y1 y2)
;nil
;nil
;(red shoes blue socks)

;;;;2.54

(defun equal? (lst1 lst2)
  (cond ((and (not (consp lst1))
	      (not (consp lst2))
	      (eq lst1 lst2))
	 t)
	((and (consp lst1)
	      (consp lst2)
	      (equal? (car lst1) (car lst2))
	      (equal? (cdr lst1) (cdr lst2)))
	 t)
	(t nil)))

;;;;2.55

;;等价于(car '(quote (abracadabra))),返回quote


;;;;符号求导

(defun deriv (exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (multiplicand exp)
			(deriv (multiplier exp) var))))
	(t (princ "unkown expression type -- DERIV"))))
(defun number? (n)
  (numberp n))
(defun variable? (x)
  (symbolp x))
(defun same-variable? (v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq v1 v2)))
(defun sum? (x)
  (and (consp x)
       (eq (car x) '+)))
(defun addend (x)
  (cadr x))
(defun augend (x)
  (caddr x))
(defun make-sum (x y)
  (list '+ x y))
(defun product? (x)
  (and (consp x)
       (eq (car x) '*)))
(defun multiplier (p)
  (cadr p))
(defun multiplicand (p)
  (caddr p))
(defun make-product (x y)
  (list '* x y))

(defun make-sum (x y)
  (cond ((=number? x 0) y)
	((=number? y 0) x)
	((and (number? x) (number? y))
	 
	 (+ x y))
	(t (list '+ x y))))

(defun =number? (v n)
  (and (number? v) (= v n))) 

(defun make-product (x y)
  (cond ((or (=number? x 0) (=number? y 0)) 0)
	((=number? x 1) y)
	((=number? y 1) x)
	((and (number? x) (number? y)) (* x y))
	(t (list '* x y))))

;;;;2.56
(defun deriv (exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (multiplicand exp)
			(deriv (multiplier exp) var))))
	((exponentiation? exp)
	 (make-product 
	  (make-product (exponent exp)
			(make-exponentiation (base exp) (- (exponent exp) 1)))
	  (deriv (base exp) var)))
	(t "do not support or error")))

(defun exponentiation? (x)
  (and (consp x) (eq (car x) '**)))
(defun base (e)
  (cadr e))
(defun exponent (e)
  (caddr e))
(defun make-exponentiation (b e)
  (cond ((=number? e 0) 1)
	((=number? e 1) b)
	(t (list '** b e))))

;;;;2.57

(defun my-memq (item lst)
  (cond ((null lst) nil)
	((eq item (car lst)) lst)
	(t (my-memq item (cdr lst)))))
(defun my-remove (item lst)
  (cond ((null lst) nil)
	((eq item (car lst)) (my-remove item (cdr lst)))
	(t (cons (car lst) (my-remove item (cdr lst))))))

(defun make-sum (&rest lst)
  (let ((new-lst (my-remove 0 lst)))
    (cond ((null new-lst) 0)
	  ((null (cdr new-lst)) (car new-lst))
	  
	  (t (cons '+ new-lst)))))

(defun make-product (&rest lst)
  (let ((new-lst (my-remove 1 lst)))
    (cond ((null new-lst) 1)
	  ((my-memq 0 new-lst) 0)
	  ((null (cdr new-lst)) (car new-lst))
	  (t (cons '* new-lst)))))

(defun addend (s)
  (cadr s))
(defun augend (s)
  (if (null (cdddr s))
      (caddr s)
      (apply #'make-sum (cddr s))))

(defun multiplier (m)
  (cadr m))
(defun multiplicand (m)
  (if (null (cdddr m))
      (caddr m)
      (apply #'make-product (cddr m))))



;;;;2.58

;;;a

(defun deriv (exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum
	  (make-product (multiplier exp)
			(deriv (multiplicand exp) var))
	  (make-product (multiplicand exp)
			(deriv (multiplier exp) var))))
	(t (format t "not surpport this exp"))))

(defun number? (n)
  (numberp n))
(defun variable? (n)
  (symbolp n))
(defun same-variable? (v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq v1 v2)))
(defun sum? (exp)
  (and (consp exp)
       (eq (cadr exp) '+)))
(defun product? (exp)
  (and (consp exp)
       (eq (cadr exp) '*)))
(defun make-sum (x y)
  (list x '+ y))
(defun make-product (x y)
  (list x '* y))
(defun addend (exp)
  (car exp))
(defun augend (exp)
  (caddr exp))
(defun multiplier (exp)
  (car exp))
(defun multiplicand (exp)
  (caddr exp))

;;;b

(defun make-sum (&rest lst)
  (cond ((null (cdr lst)) lst)
	(t (cons (car lst) (cons '+ (apply #'make-sum (cdr lst)))))))

(defun make-product (&rest lst)
  (cond ((null (cdr lst)) lst)
	(t (cons (car lst) (cons '* (apply #'make-product (cdr lst)))))))

(defun sum? (exp)
  (my-memq '+ exp))
(defun product? (exp)
  (eq (cadr exp) '*))

(defun addend (exp)
  (labels ((get-addend (exp)
	     (cond ((eq (car exp) '+) nil)
		   (t (cons (car exp) (get-addend (cdr exp)))))))
    (let ((res (get-addend exp)))
      (if (null (cdr res))
	  (car res)
	  res))))

(defun augend (exp)
  (cond ((null (cdddr exp)) (caddr exp))
	((eq (car exp) '+)
	 (cdr exp))
	(t (augend (cdr exp)))))

(defun multiplier (exp)
  (car exp))
(defun multiplicand (exp)
  (cond ((null (cdddr exp)) (caddr exp))
	(t (cddr exp))))


;;;;2.3.3集合的表示
(defun element-of-set? (x set)
  (cond ((null set) nil)
	((equal x (car set)) t)
	(t (element-of-set? x (cdr set)))))

(defun adjoin-set (x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(defun intersection-set (set1 set2)
  (cond ((or (null set1) (null set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(t (intersection-set (cdr set1) set2))))

;;;;2.59
(defun union-set (set1 set2)
  (cond ((null set1) set2)
	(t (adjoin-set (car set1)
		   (union-set (cdr set1) set2)))))

;;;;2.60
;;改动下adjoin-set就行了。

;;;;2.61

(defun adjoin-set (x set)
  (cond ((< x (car set)) (cons x set))
	(t (if (element-of-set? x set)
	       set
	       (cons x set)))))

(defun adjoin-set (x set)
  (cond ((null set) (list x))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(t (cons (car set) (adjoin-set x (cdr set))))))

;;;;2.62

(defun union-set (set1 set2)
  (let ((x1 (car set1))
	(x2 (car set2)))
    (cond ((null set1) set2)
	  ((null set2) set1)
	  ((= x1 x2)
	   (cons x1 (union-set (cdr set1) (cdr set2))))
	  ((< x1 x2)
	   (cons x1 (union-set (cdr set1) set2)))
	  (t (cons x2 (union-set set1 (cdr set2)))))))



;;;;tree sets
(defun entry (tree)
  (car tree))
(defun left-branch (tree)
  (cadr tree))
(defun right-branch (tree)
  (caddr tree))
(defun make-tree (entry left right)
  (list entry left right))

(defun element-of-set (x set)
  (cond ((null set) nil)
	((= x (entry set)) t)
	((< x (entry set))
	 (element-of-set x (left-branch set)))
	((> x (entry set))
	 (element-of-set x (right-branch set)))))


(defun adjoin-set (x set)
  (cond ((null set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch tree)
		    (adjoin-set x (right-branch set))))))

;;;;2.63

(defun tree->list-1 (tree)
  (if (null tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(defun tree->list-2 (tree)
  (labels ((copy-to-list (tree result-list)
	     (if (null tree)
		 result-list
		 (copy-to-list (left-branch tree)
			       (cons (entry tree)
				     (copy-to-list (right-branch tree)
						   result-list))))))
    (copy-to-list tree '())))

;;; 这两个函数都返回一个从小到大的序列。 使用append的要慢一点。


;;;;2.64

(defun quotient (denominator numerator)
  (let ((rest (mod denominator numerator)))
    (/ (- denominator rest) numerator)))

(defun list->tree (elements)
  (car (partial-tree elements (length elements))))

(defun partial-tree (elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts) right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))
;;;T(n)

;;;;2.65

(defun union-set (set1 set2)
  (labels ((union-set-list (lst1 lst2)
	     (let ((x1 (car lst1))
		   (x2 (car lst2)))
	       (cond ((null lst1) lst2)
		     ((null lst2) lst1)
		     ((= x1 x2)
		      (adjoin-set x1 (union-set-list (cdr lst1) (cdr lst2))))
		     ((< x1 x2)
		      (adjoin-set x1 (union-set-list (cdr lst1) lst2)))
		     ((> x1 x2)
		      (adjoin-set x2 (union-set-list lst1 (cdr lst2))))))))
    (list->tree (union-set-list (tree->list-2 set1)
				(tree->list-2 set2)))))
(defun intersection-set (set1 set2)
  (labels ((intersection-set-list (lst1 lst2)
	     (let ((x1 (car lst1))
		   (x2 (car lst2)))
	       (cond ((or (null lst1) (null lst2))
		      '())
		     ((= x1 x2)
		      (adjoin-set x1 (intersection-set-list (cdr lst1) (cdr lst2))))
		     ((< x1 x2)
		      (intersection-set-list (cdr lst1) lst2))
		     ((> x1 x2)
		      (intersection-set-list lst1 (cdr lst2)))))))
    (list->tree (intersection-set-list (tree->list-2 set1)
				       (tree->list-2 set2)))))


;;;;2.66

(defun key (record)
  (car record))

(defun lookup (given-key set-of-records)
  (cond ((null set-of-records) nil)
	((= given-key (key (entry set-of-records)))
	 (entry set-of-records))
	((< given-key (key (entry set-of-records)))
	 (lookup given-key (left-branch set-of-records)))
	((> given-key (key (entry set-of-records)))
	 (lookup given-key (right-branch set-of-records)))))




;;;;huffman tree

(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))

(defun leaf? (object)
  (eq (car object) 'leaf))

(defun symbol-leaf (object)
  (cadr object))
(defun weight-leaf (object)
  (caddr object))

(defun make-code-tree (left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(defun left-branch (tree)
  (car tree))
(defun right-branch (tree)
  (cadr tree))
(defun symbols (tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(defun weight (tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(defun decode (bits tree)
  (labels ((decode-1 (bits current-branch)
	     (if (null bits)
		 '()
		 (let ((next-branch (choose-branch (car bits) current-branch)))
		   (if (leaf? next-branch)
		       (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
		       (decode-1 (cdr bits) next-branch)))))
	   (choose-branch (bit branch)
	     (cond ((= 0 bit)
		    (left-branch branch))
		   ((= 1 bit)
		    (right-branch branch))
		   (t (format t "not this branch")))))
    (decode-1 bits tree)))
	     
(defun adjoin-set (x set)
  (cond ((null set) (list x))
	((< (weight x) (weight (car set)))
	 (cons x set))
	(t (cons (car set)
		 (adjoin-set x (cdr set))))))

(defun make-leaf-set (pairs)
  (if (null pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)
			       (cadr pair))
		    (make-leaf-set (cdr pairs))))))

;;;;2.67
(let ((sample-tree 
       (make-code-tree (make-leaf 'a 4)
		       (make-code-tree 
			(make-leaf 'b 2)
			(make-code-tree (make-leaf 'd 1)
					(make-leaf 'c 1)))))
      (sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0)))
  (format t "~%~A~%" (decode sample-message sample-tree)))

;;;(a d a b b c a)

;;;;2.68

(defun encode (message tree)
  (labels ((encode-symbol (symbol tree)
	     (cond ((leaf? tree) nil)
		   ((my-memq symbol (symbols (left-branch tree)))
		    (cons 0 (encode-symbol symbol (left-branch tree))))
		   ((my-memq symbol (symbols (right-branch tree)))
		    (cons 1 (encode-symbol symbol (right-branch tree))))
		   (t (format t "not this symbol:~A~%" symbol))))
	   (my-memq (item lst)
	     (cond ((null lst) nil)
		   ((eq item (car lst)) t)
		   (t (my-memq item (cdr lst))))))
    (if (null message)
	'()
	(append (encode-symbol (car message) tree)
		(encode (cdr message) tree)))))

;;;;2.69

(defun successive-merge (leafs)
  (cond ((null leafs) '())
	((null (cdr leafs)) (car leafs))
	(t (let ((tree (make-code-tree (car leafs)
				       (cadr leafs))))
	     (successive-merge (adjoin-set tree (cddr leafs)))))))

(defun generate-huffman-tree (pairs)
  (successive-merge (make-leaf-set pairs)))

(let ((tree (generate-huffman-tree '((c 1) (d 1) (b 2) (a 4))))
      (message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
      (origin '(a d a b b c a)))
  (labels ((show (n)
	     (format t "~%~A~%" n)))
    (show tree)
    (show (encode origin tree))
    (show (decode message tree))))
			

;;;;2.70

(let ((tree (generate-huffman-tree '((a 2) (na 16) (boom 1) (sha 3) (get 2)
				     (yip 9) (job 2) (wah 1))))
      (message '(get a job
		 sha na na na na na na na na
		 get a job
		 sha na na na na na na na na
		 wah yip yip yip yip yip yip yip yip yip
		 sha boom)))
  (labels ((show (n)
	     (format t "~%~A~%" n)))
    (show (length (encode message tree)))))


;;;84为，定长编码2^8位=256位。

;;;;2.71

;;;n-1,1

;;;;2.72

;;; 1+2+3+...n=T(n^2)



;;;;2.4抽象数据的多重表示

(defun add-complex (z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(defun sub-complex (z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (real-part z2))))
(defun mul-complex (z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (+ (angle z1) (angle z2))))
(defun div-complex (z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))

;;;;ben
(defun real-part (z)
  (car z))
(defun imag-part (z)
  (cdr z))
(defun make-from-real-imag (x y)
  (cons x y))
(defun magnitude (z)
  (sqrt (+ (square (real-part z))
	   (square (imag-part z)))))
(defun angle (z)
  (atan (imag-part z)
	(real-part z)))
(defun make-from-mag-ang (x y)
  (make-from-mag-ang (* x (cos y))
		     (* x (sin y))))

;;;;alyssa
(defun real-part (z)
  (* (magnitude z) (cos (angle z))))
(defun imag-part (z)
  (* (magnitude z) (sin (angle z))))
(defun magnitude (z)
  (car z))
(defun angle (z)
  (cdr z))
(defun make-from-mag-ang (r a)
  (cons r a))
(defun make-from-real-imag (x y)
  (cons (sqrt (+ (square x)
		 (square y)))
	(atan y x)))


;;;;基于类型的分派
(defun attach-tag (type-tag contents)
  (cons type-tag contents))
(defun type-tag (datum)
  (if (consp datum)
      (car datum)
      (error "bad tagged datum -- type-tag")))

(defun contents (datum)
  (if (consp datum)
      (cdr datum)
      (error "bad tagged datum -- contents")))
(defun rectangular? (z)
  (eq (type-tag z) 'rectangular))
(defun polar? (z)
  (eq (type-tag z) 'polar))


;;;;数据导向的程序设计
(defvar *datum* '())
(defun make-sym-val (x y)
  (list x y))
(defun get-sym (z)
  (car z))
(defun get-val (z)
  (cdr z))




(defun my-put (sym1 sym2 item)
  (labels ((lookup (sym lst)
	     (cond ((null lst) nil)
		   ((equal sym (get-sym (car lst)))
		    (get-val (car lst)))
		   (t (lookup sym (cdr lst)))))
	   (put (lst)
	     (let ((val1 (lookup sym1 lst)))
	       (if val1
		   (let ((val2 (lookup sym2 val1)))
		     (if val2
			 (mapcar #'(lambda (x)
				     (if (eq sym1 (get-sym x))
					 (cons sym1
					       (mapcar #'(lambda (y)
							   (if (eq sym2 (get-sym y))
							       (cons sym2
								     (cons item nil))
							       y))
						       (get-val x)))
					 x))
				 lst)
			 (mapcar #'(lambda (x)
				     (if (eq sym1 (get-sym x))
					 (cons sym1
					       (cons (make-sym-val sym2 item)
						     (get-val x)))
					 x))
				 lst)))
		   (cons (make-sym-val sym1
				       (make-sym-val sym2 item))
			 lst)))))
    (setf *datum* (put *datum*))))

			
		    
		    

(defun my-get (sym1 sym2)
  (labels ((lookup (sym lst)
	     (cond ((null lst) nil)
		   ((equal sym (get-sym (car lst)))
		    (get-val (car lst)))
		   (t (lookup sym (cdr lst))))))
    (car (lookup sym2 (lookup sym1 *datum*)))))


(defun install-rectangular-package ()
  (labels ((real-part (z)
	     (car z))
	   (imag-part (z)
	     (cdr z))
	   (make-from-real-imag (x y)
	     (cons x y))
	   (magnitude (z)
	     (sqrt (+ (square (real-part z))
		      (square (imag-part z)))))
	   (angle (z)
	     (atan (imag-part z)
		   (real-part z)))
	   (make-from-mag-ang (r a)
	     (cons (* r (cos a))
		   (* r (sin a))))
	   (tag (x)
	     (attach-tag 'rectangular x)))
    (my-put 'real-part '(rectangular) #'real-part)
    (my-put 'imag-part '(rectangular) #'imag-part)
    (my-put 'magnitude '(rectangular) #'magnitude)
    (my-put 'angle '(rectangular) #'angle)
    (my-put 'make-from-real-imag 'rectangular #'(lambda (x y)
						  (tag (make-from-real-imag x y))))
    (my-put 'make-from-mag-ang 'rectangular #'(lambda (r a)
						(tag (make-from-mag-ang r a))))
    'done))

(defun install-polar-package ()
  (labels ((real-part (z)
	     (* (magnitude z) (cos (angle z))))
	   (imag-part (z)
	     (* (magnitude z) (sin (angle z))))
	   (make-from-real-imag (x y)
	     (cons (sqrt (+ (square x) (square y)))
		   (atan y x)))
	   (magnitude (z)
	     (car z))
	   (angle (z)
	     (cdr z))
	   (make-from-mag-ang (r a)
	     (cons r a))
	   (tag (x)
	     (attach-tag 'polar x)))
    (my-put 'real-part '(polar) #'real-part)
    (my-put 'imag-part '(polar) #'imag-part)
    (my-put 'magnitude '(polar) #'magnitude)
    (my-put 'angle '(polar) #'angle)
    (my-put 'make-from-real-imag 'polar #'(lambda (x y)
					    (tag (make-from-real-imag x y))))
    (my-put 'make-from-mag-ang 'polar #'(lambda (r a)
					  (tag (make-from-mag-ang r a))))
    'done))


(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar #'type-tag args)))
    ;(format t "~%type-tags is :~A~%" type-tags)
    (let ((proc (my-get op type-tags)))
      ;(format t "~%proc is :~A~%" proc)
      ;(format t "~%args is :~A~%" args)
      ;(format t "~%content is :~A~%" (mapcar #'contents args))
      (if proc
	  (apply proc (mapcar #'contents args))
	  (error "not these types -- apply-generic")))))

(defun real-part (z)
  (apply-generic 'real-part z))
(defun imag-part (z) (apply-generic 'imag-part z))
(defun magnitude (z) (apply-generic 'magnitude z))
(defun angle (z) (apply-generic 'angle z))
(defun make-from-real-imag (x y)
  (funcall (my-get 'make-from-real-imag 'rectangular) x y))
(defun make-from-mag-ang (r a)
  (funcall (my-get 'make-from-mag-ang 'polar) r a))

;;;初始化函数映射表
(setf *datum* nil)
(install-rectangular-package)
(install-polar-package)

(let ((z1 (make-from-real-imag 2 3))
      
      (z2 (make-from-mag-ang 5 (atan 3 4))))
  (labels ((show (n)
	     (format t "~%~A~%" n)))
    
    
    (show (add-complex z1 z2))
    (show (real-part z2))
    (show (mul-complex z1 z2))))


;;;;2.73

(defun deriv (exp var)
  (cond ((number? exp) 0)
	((variable? exp)
	 (if (same-variable? exp var)
	     1
	     0))
	(t (funcall (my-get 'deriv (operator exp))
		    (operands exp)
		    var))))
(defun operator (exp)
  (car exp))
(defun operands (exp)
  (cdr exp))

;;;a  
;number?和variable?接受的参数不是一个确定的符号，而是一种类型。其他都接受确定的符号比如+ * ^等等。便于查找对应的过程。如果接受类型的话，还是要在查找时判断一次类型的。要不查找结果不唯一的。

;;;b 可以把通用的操作放到外面，因为不涉及到数据类型的问题，只是增加操作。

(defun install-deriv-add-mul-package ()
  (labels ((make-sum (&rest args)
	     (let ((new-args (remove 0 args)))
	       (cond ((null new-args) 0)
		     ((null (cdr new-args)) (car new-args))
		     (t (cons '+ new-args)))))
	   (make-product (v1 v2)
	     (cond ((or (=number? v1 0) (=number? v2 0)) 0)
		   ((=number? v1 1) v2)
		   ((=number? v2 1) v1)
		   (t (list '* v1 v2))))
	   (addend (exp)
	     (car exp))
	   (augend (exp)
	     (apply #'make-sum (cdr exp)))
	   (multiplier (exp)
	     (car exp))
	   (multiplicand (exp)
	     (cadr exp)))
    (my-put 'deriv '+ #'(lambda (exp var)
			 ; (format t "~%~A~A~A~A~A~A~%" exp (addend exp) (augend exp) var (deriv (addend exp) var) (deriv (augend exp) var) )
		  
			  (make-sum (deriv (addend exp) var)
				    (deriv (augend exp) var))))
    (my-put 'deriv '* #'(lambda (exp var)
			  (make-sum
			   (make-product (multiplier exp)
					 (deriv (multiplicand exp) var))
			   (make-product (multiplicand exp)
					 (deriv (multiplier exp) var)))))

    
    'done))
	     
;;;初始化函数映射表	     
(setf *datum* nil)
(install-deriv-add-mul-package)

;;;c 略去了

;;;d 改下put的symbol顺序就好了。


;;;;2.74


;;;a
;分支机构的数据结构要将自己的机构的名字作为键值加入到架构的开头，大概这样：(division-name ((employee-name ((address) (salary)))))
;各个分支机构有自己的构造函数和选择函数如(get-record)(get-salary)(make-record)等等。通过(put division-name function-name  function)的方式将各自分支结构的选择函数和构造函数放入到二维表中。总部通过(get division-name function0name)的方式得到对应分支结构的操作函数，从而完成类型分派。
;a
;(defun get-record (division employee)
;  (funcall (get divison 'record)
;	   (get-datum divison) employee))
;b
;(defun get-salary (division employee)
;  (funcall (get division 'salary)
;	    (get-record division employee)))
;c
;(defun find-employee-record (employee division-lst)
;  (if (null division-lst)
;      nil
;      (let ((res (get-record (car division-lst) employee)))
;	(if res
;	    res
;	    (find-employee-record employee (cdr division-lst))))))
;d
;先将自己的名字作为键值加入到数据的开头，然后将自己公司的的选择函数和构造函数put到二维函数表中就好了。


;;;;2.75

(defun make-from-mag-ang (r a)
  #'(lambda (op)
      (cond ((eq op 'real-part) (* r (cos a)))
	    ((eq op 'imag-part) (* r (sin a)))
	    ((eq op 'magnitude) r)
	    ((eq op 'angle) a)
	    (t (error "unkown op -- make-from-mag-ang")))))

;;;;2.76

;1、基于类型分派的设计模式。在增加类型的时候要修改每一个操作函数。在增加操作函数的时候不需要修改其他代码，只需增加对应的类型操作即可。注意命名不可冲突。（将不同类型的操作都写在通用操作函数里也不是不可以，但是不容易查错）。
;2、基于消息传递的设计模式。在增加新操作的时候要修改每一个类型函数。在增加新类型的时候只需要添加类型函数就可以了，不需要修改其他代码。
;3、数据导向的设计模式。无论是增加新的类型还是增加新的操作，都要不需要修改原有的代码。只需要按照原先二维表格的结构，增加对应的操作或者类型就可以了。

;;;;2.5.1通用型算术运算 

;;清空二维表
(setf *datum* nil)

;;定义数据标签的构造函数和选择函数
(defun attach-tag (type-tag contents)
  (cons type-tag contents))
(defun type-tag (datum)
  (if (consp datum)
      (car datum)
      (error "bad tagged datum -- type-tag")))
(defun contents (datum)
  (if (consp datum)
      (cdr datum)
      (error "bad tagged datum-- contents")))

;;定义通用的函数分派函数，根据参数的数据标签选择对应的函数操作
(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar #'type-tag args)))
    (format t "~%~A~%" type-tags)
    (let ((proc (my-get op type-tags)))
      (format t "~%~A  ~A~%" op type-tags)
      (if proc 
	  (apply proc 
		 (mapcar #'contents args))
	  (error "unkown these types -- apply-generic")))))

;;定义通用的上次操作函数
(defun add (x y) (apply-generic 'add x y))
(defun sub (x y) (apply-generic 'sub x y))
(defun mul (x y) (apply-generic 'mul x y))
(defun div (x y) (apply-generic 'div x y))

;;定义常规数的算术包
(defun install-scheme-number-package ()
  (labels ((tag (x) (attach-tag 'scheme-number x)))
    (my-put 'add '(scheme-number scheme-number) 
	    #'(lambda (x y) (tag (+ x y))))
    (my-put 'sub '(scheme-number scheme-number)
	    #'(lambda (x y) (tag (- x y))))
    (my-put 'mul '(scheme-number scheme-number)
	    #'(lambda (x y) (tag (* x y))))
    (my-put 'div '(scheme-number scheme-number)
	    #'(lambda (x y) (tag (/ x y))))
    (my-put 'make 'scheme-number
	    #'(lambda (x) (tag x)))
    'done))

;;安装算术包并定义常规数的构造函数
(install-scheme-number-package)

(defun make-scheme-number (n)
  (funcall (my-get 'make 'scheme-number)
	   n))


;;定义有理数算术包	   
(defun install-rational-package ()
  (labels ((numer (x) (car x))
	   (denom (x) (cdr x))
	   (make-rat (n d)
	     (let ((g (gcd n d)))
	       (cons (/ n g) (/ d g))))
	   (add-rat (x y)
	     (make-rat (+ (* (numer x) (denom y))
			  (* (denom x) (numer y)))
		       (* (denom x) (denom y))))
	   (sub-rat (x y)
	     (make-rat (- (* (numer x) (denom y))
			  (* (denom x) (numer y)))
		       (* (denom x) (denom y))))
	   (mul-rat (x y)
	     (make-rat (* (numer x) (numer y))
		       (* (denom x) (denom y))))
	   (div-rat (x y)
	     (make-rat (* (numer x) (denom y))
		       (* (denom x) (numer y))))
	   (tag (x) (attach-tag 'rational x)))
    (my-put 'add '(rational rational)
	    #'(lambda (x y) (tag (add-rat x y))))
    (my-put 'sub '(rational rational)
	    #'(lambda (x y) (tag (sub-rat x y))))
    (my-put 'mul '(rational rational)
	    #'(lambda (x y) (tag (mul-rat x y))))
    (my-put 'div '(rational rational)
	    #'(lambda (x y) (tag (div-rat x y))))
    (my-put 'make 'rational
	    #'(lambda (n d) (tag (make-rat n d))))
    (my-put 'numer '(rational) #'numer)
    (my-put 'denom '(rational) #'denom)
    'done))

;;安装有理数算术包并定义有理数构造函数及选择函数
(install-rational-package)
(defun make-rational (n d)
  (funcall (my-get 'make 'rational)
	   n
	   d))
(defun numer (x) (apply-generic 'numer (attach-tag 'rational x)))
(defun denom (x) (apply-generic 'denom (attach-tag 'rational x)))
		   
;;安装复数的两种表示类型的包，并在这之上构造一层通用的复数算术包
(install-rectangular-package)
(install-polar-package)

(defun install-complex-package ()
  (labels ((make-from-real-imag (x y)
	     (funcall (my-get 'make-from-real-imag 'rectangular)
		      x
		      y))
	   (make-from-mag-ang (r a)
	     (funcall (my-get 'make-from-mag-ang 'polar)
		      r
		      a))
	   (add-complex (z1 z2)
	     (make-from-real-imag (+ (real-part z1) (real-part z2))
				  (+ (imag-part z1) (imag-part z2))))
	   (sub-complex (z1 z2)
	     (make-from-real-imag (- (real-part z1) (real-part z2))
				  (- (imag-part z1) (imag-part z2))))
	   (mul-complex (z1 z2)
	     (make-from-mag-ang (* (magnitude z1) (magnitude z2))
				(+ (angle z1) (angle z2))))
	   (div-complex (z1 z2)
	     (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
				(- (angle z1) (angle z2))))
	   (tag (z) (attach-tag 'complex z)))
    (my-put 'add '(complex complex)
	    #'(lambda (z1 z2) (tag (add-complex z1 z2))))
    (my-put 'sub '(complex complex)
	    #'(lambda (z1 z2) (tag (sub-complex z1 z2))))
    (my-put 'mul '(complex complex)
	    #'(lambda (z1 z2) (tag (mul-complex z1 z2))))
    (my-put 'div '(complex complex)
	    #'(lambda (z1 z2) (tag (div-complex z1 z2))))
    (my-put 'make-from-real-imag 'complex
	    #'(lambda (x y) (tag (make-from-real-imag x y))))
    (my-put 'make-from-mag-ang 'complex
	    #'(lambda (r a) (tag (make-from-mag-ang r a))))
    (my-put 'real-part '(complex)#'real-part)
    (my-put 'imag-part '(complex) #'imag-part)
    (my-put 'magnitude '(complex) #'magnitude)
    (my-put 'angle '(complex) #'angle)

    'done))

(install-complex-package)
;;定义复数的通用构造函数
(defun make-complex-from-real-imag (x y)
  (funcall (my-get 'make-from-real-imag 'complex)
	   x
	   y))
(defun make-complex-from-mag-ang (r a)
  (funcall (my-get 'make-from-mag-ang 'complex)
	   r
	   a))

;;;;2.77
;;原理是这样的。通过加入complex的real-part并指向原来的real-part。这样在调用real-part的时候如果是complex类型就对该类型的contents再次调用real-part。直到遇到rectangular或者polar类型。才调用对应的数据包内部函数。在这里一共调用2次。第一次处理complex第二次处理rectangular或者polar。

;;;;2.78

(defun attach-tag (type-tag contents)
  (if (numberp contents)
      contents
      (cons type-tag contents)))

(defun type-tag (datum)
  (cond ((numberp datum) 'scheme-number)
	((consp datum) (car datum))
	(t (error "bad tags --type-tag"))))
(defun contents (datum)
  (cond ((numberp datum) datum)
	((consp datum) (cdr datum))
	(t (error "bad tags --contents"))))

;;;;2.79

(defun install-equ?-package ()
  (my-put 'equ? '(scheme-number scheme-number) #'=)
  (my-put 'equ? '(rational rational) #'equal)
  (my-put 'equ? '(complex complex)
	  #'(lambda (z1 z2)
	      (and (= (real-part z1) (real-part z2))
		   (= (imag-part z1) (imag-part z2))))))

(install-equ?-package)


(defun equ? (x y) (apply-generic 'equ? x y))


;;;;2.80


(defun install-=zero?-package ()
  (my-put '=zero? '(scheme-number) #'(lambda (x) (= x 0)))
  (my-put '=zero? '(rational)
	  #'(lambda (x)
	      (= (numer x) 0)))
  (my-put '=zero? '(complex)
	  #'(lambda (x)
	      (and (= (real-part x) 0)
		   (= (imag-part x) 0))))
  'done)

(install-=zero?-package)

(defun =zero? (x) (apply-generic '=zero? x))

;;;;2.5.2不同类型数据组合

(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar #'type-tag args)))
    (let ((proc (my-get op type-tags)))
      (if proc
	  (apply proc
		   (mapcar #'contents args))
	  (if (= (length args) 2)
	      (let ((t1 (car type-tags))
		    (t2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(let ((t1->t2 (my-get t1 t2))
		      (t2->t1 (my-get t2 t1)))
		  (cond (t1->t2 (apply-generic op (funcall t1->t2 a1) a2))
			(t2->t1 (apply-generic op a1 (funcall t2->t1 a2)))
			(t (error "not methods for these types -- apply-generic ~A ~A" op type-tags)))))
	      (error "not methods for these types -- apply-generic ~A ~A" op type-tags))))))


;;;;2.81
;;;a
;;会陷入死循环。在apply-generic做类型转换的过程中。会一直转换到自身。

;;;b
;;很明显并没有。

;;;c
(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar #'type-tag args)))
    (let ((proc (my-get op type-tags)))
      (if proc
	  (apply proc (mapcar #'contents args))
	  (if (= (length args) 2)
	      (let ((t1 (car type-tags))
		    (t2 (cadr type-tags))
		    (a1 (car args))
		    (a2 (cadr args)))
		(if (eq t1 t2)
		    (error "not methods for these types -- apply-generic ~A" (list op type-tags))
		    (let ((t1->t2 (my-get t1 t2))
			  (t2->t1 (my-get t2 t1)))
		      (cond (t1->t2 (apply-generic op (funcall t1->t2 a1) a2))
			    (t2->t1 (apply-generic op a1 (funcall t2->t1 a2)))
			    (t (error "not methods for these types ~A" (list op type-tags)))))))
	      (error "not methods for these types ~A" (list op type-tags)))))))


;;;;2.82
;;假定存在这种情况：已知，已经定义了scheme-number->complex和rational->complex。但是scheme-number和rational之间的转换并没有定义。那么，当我们操作scheme-number和rational作为参数的时候，apply-generic的内部的转换一定是失败的。但是我们可以把他们都转换到complex的类型去操作。这种情况在这个策略下就是不具有一般性的。
;;类型塔结构的模型在这里就可以避免这中问题。

;;;;2.83

(defun install-raise-package ()
  (labels ((scheme-number->rational (x)
	     (make-rational x 1))
	   (rational->complex (x)
	     (make-complex-from-real-imag (/ (numer x) (denom x)) 0)))
    (my-put 'raise '(scheme-number) #'scheme-number->rational)
    (my-put 'raise '(rational) #'rational->complex))
  'done)

(install-raise-package)

(defun raise (x) (apply-generic 'raise x))

;;;;2.84

(defvar *type-tower* '(scheme-number rational complex))
(defun update-type-tower (new-tower)
  (setf *type-tower* new-tower))

(defun add-type-to-tower (type before-type after-type before->type type->after)
  (labels ((insert (lst)
	     (cond ((null lst) (error "can't find the position -- add-type-to-tower -insert ~A" (list before-type after-type)))
		   ((and (eq before-type (car lst))
			 (eq after-type (cadr lst)))
		    (cons (car lst) (cons type (cdr lst))))
		   (t (cons (car lst) (insert (cdr lst)))))))
	   
    
    (cond ((eq after-type 'start)
	   (update-type-tower (cons type *type-tower*))
	   (my-put 'raise type type->after))
	  ((eq before-type 'end)
	   (update-type-tower (append *type-tower* (list type)))
	   (my-put 'raise before-type before->type))
	  (t 
	   (update-type-tower (insert *type-tower*))
	   (my-put 'raise before-type before->type)
	   (my-put 'raise type type->after)))))




(defun higher-type (lst)
  (labels ((lookup (types lst)
	     (let ((type (car types)))
	       (let ((new-lst (remove type lst)))
		 (cond ((null new-lst) type)
		       ((null types) (error "these type not in type-tower--higher-type ~A" new-lst))
		       (t (lookup (cdr types) new-lst)))))))
    (lookup *type-tower* lst)))

(defun raise-to-type (data type)
  (let ((low-type (type-tag data))) 
    (if (eq low-type type)
	data
	(raise-to-type (raise data) type))))



(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar #'type-tag args)))
    (let ((proc (my-get op type-tags)))
      (if proc
	  (apply proc (mapcar #'contents args))
	  (let ((hig-type (higher-type type-tags)))
	    (let ((new-type-tags (mapcar #'(lambda (x) hig-type) type-tags)))
	      (if (my-get op new-type-tags)
		  (apply #'apply-generic (cons op (mapcar #'(lambda (x)
							      (raise-to-type x hig-type)) 
							  args)))
		  (error "not these type methods --apply-generic ~A" (list op type-tags)))))))))


;;;;2.85



(defun install-project-package ()
  (my-put 'project '(complex) #'(lambda (x) (make-rational (round (real-part x)) 1)))
  (my-put 'project '(rational) #'(lambda (x) (make-scheme-number (numer x))))
  'done)

(install-project-package)

(defun project (data) (apply-generic 'project data))

(defun drop (x)
  (cond ((eq (type-tag x) (car *type-tower*)) x)
	((equ? x (raise (project x)))
	 (drop (project x)))
	(t x)))

(defun apply-generic (op &rest args)
  (let ((type-tags (mapcar #'type-tag args)))
    (let ((proc (my-get op type-tags)))
      (if proc
	  (let ((res (apply proc (mapcar #'contents args))))
	    (if (or (eq op 'raise) (eq op 'project)) 
		(drop res)
		res))

	  (let ((hig-type-tag (higher-type type-tags)))
	    (let ((new-type-tags (mapcar #'(lambda (x) hig-type-tag) type-tags)))
	      (if (my-get 'op new-type-tags)
		  (let ((new-args (mapcar #'(lambda (x)
					      (raise-to-type x hig-type-tag))
					  args)))
		    (apply #'apply-generic (cons op new-args)))
		  (error "not these type methods--apply-generic ~A" (list op type-tags new-type-tags)))))))))
		  
