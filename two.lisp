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
  (list x y))

(defun start-segment (x)
  (car x))

(defun end-segment (x)
  (car (cdr x)))

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
