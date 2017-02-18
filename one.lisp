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

;;;recurse
(defun product (term a next b)
  (if (> a b)
      1.0
      (* (funcall term a)
	 (product term (funcall next a) next b))))

(defun factorial (b)
  (defun inc (a)
    (+ a 1))
  (defun fy (a)
    (/ a (inc a)))
 
  (defun term (a)
    (if (evenp a)
	(fy a)
	(/ 1 (fy a))))

  (product #'term 2 #'inc b))

;;;iter
(defun product (term a next b)
  (defun iter (a result)
    (if (> a b)
	result
	(iter (funcall next a)
	      (* (funcall term a)
		 result))))
  (iter a 1.0))

;;;;1.32
;;;recurse
(defun accumulate (combiner null-value term a next b)
  (if (> a b)
      null-value
      (funcall combiner (funcall term a)
	       (accumulate combiner null-value term (funcall next a) next b))))
;;;iter
(defun accumulate (combiner null-value term a next b)
  (defun iter (a result)
    (if (> a b)
	result
	(iter (funcall next a)
	      (funcall combiner result (funcall term a)))))
  (iter a null-value))

(defun sum (term a next b)
  (accumulate #'+ 0 term a next b))

(defun product (term a next b)
  (accumulate #'* 1.0 term a next b))

;;;;1.33
;;;recurse
(defun filtered-accumulate (combiner null-value term a next b filter)
  (cond ((> a b) null-value)
	((funcall filter a)
	 (funcall combiner (funcall term a)
		  (filtered-accumulate combiner
				       null-value
				       term
				       (funcall next a)
				       next
				       b
				       filter)))
	('t (filtered-accumulate combiner
				 null-value
				 term
				 (funcall next a)
				 next
				 b
				 filter))))

(defun sum-prime (a b)
  (defun iden (x) x)
  (filtered-accumulate #'+ 0 #'iden a #'inc b #'primep))

(defun my-gcd (a b)
  (if (= b 0)
      a
      (my-gcd b (mod a b))))

(defun sum-prime-n (n)
  (defun iden (x) x)
  (defun filter (i)
    (= 1 (my-gcd n i)))
  (filtered-accumulate #'* 1.0 #'iden 1 #'inc n #'filter))

;;;;1.34
;;;调用(f f)的过程如下：(f f)=>(f 2)=>(2 2),2不是函数，执行失败。

;;;;折半法寻找平方根

(defun my-search (f neg-p pos-p)
  
  (let ((mid-p (average neg-p pos-p)))
    (if (close-enoughp neg-p pos-p)
	mid-p
	(let ((test-value (funcall f mid-p)))
	  (cond ((positivep test-value) (my-search f neg-p mid-p))
		((negativep test-value) (my-search f mid-p pos-p))
		(t mid-p))))))
(defun close-enoughp (a b)
  (< (abs (- a b)) 0.00001))
(defun positivep (x)
  (> x 0))
(defun negativep (x)
  (< x 0))
;;;;1.35


(defun fixed-point (f first-guess)
  (defun close-enoughp (a b)
    (< (abs (- a b)) 0.0001))
  
  (defun try (guess)
    (let ((next (funcall f guess)))
      (if (close-enoughp guess next)
	  next
	  (try next))))
  
  (try first-guess))
;;;两边乘x，变换为x^2-x-1=0的解。
;;;(fixed-point #'(lambda (x) (+ 1 (/ 1 x))) 1)
;;;1.618

;;;;1.36

(defun fixed-point (f guess)
  (defun close-enoughp (a b)
    (< (abs (- a b)) 0.0001))
  (defun try (guess)
    (let ((next (funcall f guess)))
      (format t "~a => ~a ~%" guess next)
      (if (close-enoughp guess next)
	  next
	  (try next))))
  (try guess))

;;;不采用平均阻尼技术的函数
;;(fixed-point #'(lambda (x) (/ (log 1000) (log x))) 10)
;;28步
;;;采用平均阻尼技术就是两边都加x，然后两边都除以2.
;;(fixed-point #'(lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 10)
;;8步
;;;结果表明采用平均阻尼技术的运算步数要少2/3左右。

;;;;1.37
;;;recurse
(defun cont-frac (n d k)
  (defun recu (i)
    (let ((val-n (funcall n i))
	  (val-d (funcall d i)))
      (if (= i k)
	  (/ val-n val-d)
	  (/ val-n (+ val-d (recu (+ i 1)))))))
  (recu 1))
;;;iter
(defun cont-frac (n d k)
  (defun iter (i result)
    (let ((val-n (funcall n i))
	  (val-d (+ result (funcall d i))))
      (if (= i 1)
	  (/ val-n val-d)
	  (iter (- i 1)
		(/ val-n val-d)))))
  (iter k 0))

;;;需要k=11才行。

;;;;1.38
;;;先写思路：观察Di发现在2、5、8、11...的位置均为偶数递增。进一步观察这些位置满足3n-1的通项公式。下面就是要分析n与i的关系。先列3n-1=i;=> 3n=i+1;n必须是大于0的整数。所以可以得出i+1为3得整数倍得时候得位置就是所要得那个位置。更进一步说，就是i+1与3的最大公约数就是3.即(gcd i 3)=3;再分析Di与i的关系由Di=2n 推导出Di=2*(i+1)/3;
(defun fd (i)
  (defun is-positionp ()
    (= 3 (gcd (+ i 1) 3)))
  (if (is-positionp)
      (/ (* 2 (+ i 1))
	 3)
      1))
;;(+ 2 (cont-frac #'(lambda (x) 1.0) #'fd 10))

;;;;1.39
;;;recu
(defun tan-cf (x k)
  (defun d (i)
    (- (* 2 i) 1))
  (defun n (i)
    (if (= i 1)
	x
	(square x)))

  (defun cont-frac-recu (n d k i)
    (let ((val-n (funcall n i))
	  (val-d (funcall d i)))
      (if (= i k)
	  (/ val-n val-d)
	  (/ val-n 
	     (- val-d (cont-frac-recu n d k (+ i 1)))))))

  (cont-frac-recu #'n #'d k 1))
;;;iter
(defun tan-cf (x k)
  (defun n (i)
    (if (= i 1)
	x
	(square x)))
  (defun d (i)
    (- (* 2 i) 1))
  (defun cont-frac-iter (i result)
    (let ((val-n (n i))
	  (val-d (- (d i) result)))
      (if (= i 1)
	  (/ val-n val-d)
	  (cont-frac-iter (- i 1)
			  (/ val-n val-d)))))
  (cont-frac-iter k 0))

;;;; 函数作为返回值
(defun average-damp (f)
  (defun average (a b)
    (/ (+ a b) 2))
  #'(lambda (x) (average x (funcall f x))))

(defun fixed-point (f guess)
  (defun try (guess)
    (let ((next (funcall f guess)))
      (if (close-enoughp guess next)
	  next
	  (try next))))
  (defun close-enoughp (a b)
    (< (abs (- a b)) 0.0001))
  (try guess))

(defun my-sqrt (x)
  (fixed-point (average-damp #'(lambda (y) (/ x y)))
	       1.0))

;;;;牛顿法

(defun deriv (f)
  (let ((dx 0.00001))
    #'(lambda (x) 
	(/ (- (funcall f (+ x dx))
	      (funcall f x))
	   dx))))
(defun newton-transform (f)
  #'(lambda (x)
      (- x
	 (/ (funcall f x)
	    (funcall (deriv f) x)))))
  
(defun newton-method (g guess)
  (fixed-point (newton-transform g) guess))

(defun my-sqrt (x)
  (newton-method #'(lambda (y) (- (square y) x))
		 1.0))

;;;;
(defun cubic (a b c)
  (defun cube (x)
    (* x x x))
  (defun square (x)
    (* x x))
  #'(lambda (x)
      (+ (cube x)
	 (* a
	    (square x))
	 (* b x)
	 c)))

;;;;1.41
(defun my-double (f)
  #'(lambda (x)
      (funcall f
	       (funcall f x))))

;;;(funcall (funcall (my-double (my-double #'mydouble)) #'inc) 5)
;;;21

;;;;1.42
(defun compose (f g)
  #'(lambda (x)
      (funcall f
	       (funcall g x))))
;;;;1.43
;;;recurse
(defun repeated (f n)
  (if (= n 1)
      f
      #'(lambda (x)
	   (funcall f
		    (funcall (repeated f (- n 1))
			     x)))))

;;;iter
(defun repeated (f n)
  (defun iter (i result)
    (if (= i n)
	result
	(iter (+ i 1)
	      #'(lambda (x)
		  (funcall f
			   (funcall result x))))))
  (iter 1 f))

;;;利用compose的版本
;;recurse
(defun repeated (f n)
  (if (= n 1)
      f
      (compose f
	       (repeated f (- n 1)))))

;;iter
(defun repeated (f n)
  (defun iter (i result)
    (if (= i n)
	result
	(iter (+ i 1)
	      (compose f result))))
  (iter 1 f))
;;你看，抽象是多么的重要。

(defun smooth (f)
  (let ((dx 0.0001))
    #'(lambda (x)
	(/ (+ (funcall f (- x dx))
	      (funcall f x)
	      (funcall f (- x dx)))
	   3))))
(defun smooth-n-times (f n)
  (repeated (smooth f) n))

;;;;1.45
(defun if-closep (f)
  (let ((dy 0.01))
    (defun close-enoughp (a b)
      (< (abs (- a b)) dy)))
  (defun try (guess n)
    (let ((next (funcall f guess)))
      (cond ((close-enoughp guess next) t)
	    ((= n 0) nil)
	    (t (try next (- n 1))))))
  (try 20.0 20))
(defun average-damp (f)
  #'(lambda (x)
      (/ (+ x
	    (funcall f x))
	 2)))
(defun average-damp-times (f)
  (defun try (f result)
    (if (if-closep f)
	result
	(try (average-damp f)
	     (+ result 1))))
  (try f 0))
