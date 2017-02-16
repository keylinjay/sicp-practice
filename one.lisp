(defun cube (x)
  (* x x x))
;;;;1.16
(defun square (x)
  (* x x))
(defun fast-expt (b n a)
  (cond ((= n 0) a)
	((evenp n) (fast-expt (square b) (/ n 2) a))
	('t (fast-expt b (- n 1) (* a b)))))
;;;;1.17
(defun my-double (x)
  (+ x x))
(defun halve (x)
  (/ x 2))
(defun my-multi (a b)
  (cond ((= b 0) 0)
	((evenp b) (my-double (my-multi a (halve b))))
	('t (+ a (my-multi a (- b 1))))))
;;;;1.18
(defun my-multi-iter (a b s)
  (cond ((= b 0) s)
	((evenp b) (my-multi-iter (my-double a) (halve b) s))
	('t (my-multi-iter a (- b 1) (+ s a)))))

;;;;1.19
(defun fib-iter (a b p q n)
  (cond ((= n 0) b)
	((evenp n)
	 (fib-iter a
		   b
		   (+ (* q q)
		      (* p p))
		   (+ (* q q)
		      (* 2 p q))
		   (/ n 2)))
	('t (fib-iter (+ (* a q) (* b q) (* a p))
		      (+ (* a q) (* b p))
		      p
		      q
		      (- n 1)))))

;;;;1.20
;;13次
;;5次

;;;;费马小定理判断素数

;;;求b的n次方模n的余数
(defun expmod (b e n)
  (cond ((= e 0) 1)
	((evenp e)
	 (mod (square (expmod b (/ e 2) n))
	      n))
	('t (mod (* b (expmod b (- e 1) n))
		 n))))
;;;费马定理测试
(defun fermat-test (n)
  ;;测试过程
  (defun try-it (a)
    (= a (expmod a n n)))
  ;;测试比n小的一个随机数
  (try-it (+ 1 (random (- n 1)))))
;;;判断素数
(defun fermat-primep (n times)
  (cond ((= 0 times) 't)
	((fermat-test n) (fermat-primep n (- times 1)))
	('t nil)))

;;;;1.21
(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divisorp n test-divisor) test-divisor)
	('t (find-divisor n (+ test-divisor 1)))))
(defun divisorp (n test-divisor)
  (= 0 (mod n test-divisor)))

(defun primep (n)
  (= n (smallest-divisor n)))

;;;19 t,1909 t,19999 nil

;;;;1.22
(defun newline ()
  (format t "~%"))
(defun display (s)
  (format t "~a" s))
(defun runtime ()
  (* (get-internal-real-time) 1.0))
(defun timed-prime-test (n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(defun start-prime-test (n start-time)
  (if (primep n)
      (report-prime (- (runtime) start-time))))

(defun report-prime (elapsed-time)
  (display "***")
  (display elapsed-time))

(defun next-odd (n)
  (if (evenp n)
      (+ n 1)
      (+ n 2)))

(defun search-for-primes (start n)
  (cond ((= n 0) 't) 
	((primep start) 
	 (timed-prime-test start)
	 (search-for-primes (next-odd start) (- n 1)))
	('t (search-for-primes (next-odd start) n))))
;;;我得到的结果是正比于根号10的。环境为win10.sbcl。slime下。100亿用时12毫秒，1000亿用时36毫秒。

;;;;1.23

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divisorp n test-divisor) test-divisor)
	('t (find-divisor n (next-odd test-divisor)))))

;;;比值确实接近2倍。也许跟sbcl有关。100亿用时6毫秒。

;;;;1.24

(defun start-prime-test (n start-time)
  (if (fermat-primep n 2000)
      (report-prime (- (runtime) start-time))))

;;;fermat检测在大数检测上要快的多。测试10000亿和100000亿基本没有什么差别都不到1毫秒。它的耗时和检测时随机数选取的次数（times）关系大一些。检测次数越多耗时也会增加。比如在100000亿附近检测次数从20增加到2000次，耗时增加了70倍左右。

;;;;1.25
;;;问题在于计算大数乘幂上。在很大的数的乘幂的时候，因为机器的问题会比较慢。

;;;;1.26
;;;这是因为会对(expmod base (/ exp 2) m)求值两次。

;;;;1.27

(defun expmod (b e m)
  (cond ((= e 0) 1)
	((evenp e) (mod (square (expmod b (/ e 2) m))
			m))
	('t (mod (* b (expmod b (- e 1) m))
		 m))))
(defun carm-test (n)
  (defun try-it (start)
    (cond ((= start n) t)
	  ((= start (expmod start n n)) (try-it (+ 1 start)))
	  (t nil)))
  (try-it 1))

;;;561、1105、1729、2465、2821、6601测试均通过。

;;;;1.28

(defun rabin-primep (n times)
  
  (defun not-unique-sqrt (b)
    (and (/= b (- n 1))
	 (/= b 1)
	 (= 1 (mod (square b) n))))

  (defun expmod (b e)
    (cond ((not-unique-sqrt b) 0)
	  ((= e 0) 1)
	  ((evenp e) (mod (square (expmod b (/ e 2)))
			  n))
	  (t (mod (* b (expmod b (- e 1)))
		  n))))
  (defun rabin-test ()
    (defun try-it (a)
      ;(display a)
      ;(display (expmod a (- n 1)))
      (= 1 (expmod a (- n 1))))
    (try-it (+ 1 (random (- n 1)))))
  
  (cond ((= times 0) t)
	((rabin-test) (rabin-primep n (- times 1)))
	(t nil)))

;;;;sum
(defun sum (term a next b)
  (if (> a b)
      0
      (+ (funcall term a)
	 (sum term (funcall next a) next b))))

;;;;1.29

(defun simpsen (f a b n)
 
  (defun eval-h ()
    (/ (- b a) n))
  
  (defun factor (k)
    (cond ((or (= k 0) (= k n)) 1)
	  ((evenp k) 2)
	  (t 4)))

  (defun eval-y (k)
    (funcall f (+ a (* k (eval-h)))))

  (defun term (k)
    (* (factor k)
       (eval-y k)))
  
  (defun next (k)
    (+ k 1))
  

  (if (evenp n)
      (* (/ (eval-h) 3)
	 (sum #'term 0 #'next n))
      (format t "n is not even")))
  
;;;;1.30

(defun sum (term a next b)
  (defun iter (a result)
    (if (> a b)
	result
	(iter (funcall next a)
	      (+ result (funcall term a)))))
  (iter a 0))

;;;;1.31
(defun inc (x)
  (+ x 1))

(defun product (term a next b)
  (if (> a b)
      1
      (* (funcall term a)
	 (product term (funcall next a) next b))))

(defun product (term a next b)
  (defun iter (a result)
    (if (> a b)
	result
	(iter (funcall next a)
	      (* (funcall term a)
		 result))))
  (iter a 1))
