
(defparameter a 0)
(defparameter b 0)
(defparameter c 0)


(defparameter *test-tree* '(+ a (- b c)))


(defmacro test-macro (a b c)
  `(funcall (lambda (a b c) ,*test-tree*) ,a ,b ,c))


(defmacro test-macro-2 (tree a b c)
  `(funcall (lambda (a b c) ,tree) ,a ,b ,c))


(defmacro test-macro-3 (in-tree a b c)
  (let (tree (gensym))
    `(let ((,tree in-tree))
       (values
	(funcall (lambda (a b c) ,tree) ,a ,b ,c)
	,tree))))


;;; ------

(defparameter *trees* '(
			(+ a (- b c))
			(+ a (+ b c))
			(+ a (* b c))))


(defmacro test-macro-4 (n a b c)
  `(funcall (lambda (a b c) ,(nth n *trees*)) (,a ,b ,c)))


(defun eval-tree (n in-a in-b in-c)
  (let*
      ((tree (nth n *trees*)))
    (setf a in-a)
    (setf b in-b)
    (setf c in-c)
    (eval tree)))



;; --- TO JE VALJDA TO ???
(defun eval-tree (n in-fuel in-b in-c)
  (let*
      ((tree (nth n *trees*))
       (fuel in-a)
       (b in-b)
       (c in-c))
    (eval tree)))
  

;;; ------


(defun eval-tree (nth-program)
  (first (nth nth-program *programs*)))


(defun simul-ignition (tree fuel velocity altitude)
  (let
      ((ignition 
	(funcall 
	 (lambda 
	     (fuel velocity altitude) 
	   (eval tree))
	 fuel velocity altitude)))
    (cond
      ((< ignition 0) 0)
      ((> ignition 1) 1)
      (t ignition))))


;;; -----

(defun sort-predicate (x y)
  (if (> (second x) (second y))
      nil
      t))

(defparameter *test-programs* '(
				((+ a (- b c)) 3)
				((+ a (+ b c)) 2)
				((+ a (* b c)) 1)))