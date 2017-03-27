



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

(defvar w1 (make-withdraw 100))
(defvar w2 (make-withdraw 100))
