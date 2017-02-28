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
	     (cond ((= k n) t)
		   ((> (square k) n) nil)
		   ((= (mod n k) 0) nil)
		   (t (prime-test (+ k 1))))))
    (prime-test 2)))
