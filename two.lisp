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
    
    (cond ((and (>= x1 0) (>= x2 0)) (cond ((and (>= y1 0) (>= y2 0))
					    (make-interval (* x1 y1) 
							   (* y1 y2)))
					   ((and (< y1 0) (< y2 0))
					    (make-interval (* x2 y2) 
							   (* x1 y1)))
					   (t (make-interval (* x2 y1) 
							     (* x2 y2)))))
	  ((and (< x1 0) (< x2 0)) (cond ((and (>= y1 0) (>= y2 0))
					  (make-interval (* x2 y2)
							 (* x1 y1)))
					 ((and (< y1 0) (< y2 0))
					  (make-interval (* x1 y1)
							 (* x2 y2)))
					 (t (make-interval (* x2 y2)
							   (* x2 y1)))))
	  (t (cond ((and (>= y1 0) (>= y2 0))
		    (make-interval (* x1 y2) (* x2 y2)))
		   ((and (< y1 0) (< y2 0))
		    (make-interval (* x2 y2) (* x1 y2)))
		   (t (make-interval (* x1 y2) (* x2 y2))))))))
	  
					   
					  
