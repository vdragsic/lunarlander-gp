
#|
----------------------------------------------------------------------
Seminar work for Faculty of electrical engineering and computing, 
Zagreb, Croatia

MOONLANDER GP

Calculating optimized thruster ignition function for landing 
a moonlander on moon (or whatever) surface.

Author: Veljko Dragsic - veljko@kset.org
Tutor: Domagoj Jakobovic

October 2009
----------------------------------------------------------------------
|#


;;; Some real parameters from LM
 
; mass =  4,670 + 10,334 (fuel: 8,165) kg
; velocity = 21 m/s at 150m altitude ("landing stage")
; altitude ~= 15km



;;; CONSTANTS --------------------------------------------------------

(defparameter *gravity* 1.622
  "Moon gravity")

(defparameter *max-thrust* 45000
  "Max LM thruster force: real params: 45.04kN (throttable from 10% to 60%")

(defparameter *thruster-consumption* 25
  "Thruster fuel consumption @100% in kg/s --> assumption")

(defparameter *deltatime* 0.1
  "Time interval for landing simulation (in sec)")


;;; Lander START Parameters

(defparameter *start-altitude* 150
  "Start altitude in meters")

(defparameter *start-velocity* 21
  "Start velocity in m/s")

(defparameter *start-mass* 7000
  "Start mass (fuel not incalculated) in kg")

(defparameter *start-fuel* 2000
  "Start fuel in kg")


;;; Math helper functions

(defun square (x)
  "Returns square of argument."

  (* x x))

(defun safe-/ (a b)
  "'Division by zero'-proof division,
    if b=0 then returns 1 --> aproximation."

  (if (< (abs b) 1e-6)
      1
      (/ a b)))

(defun safe-log (x)
  "Log function that rounds argument<0.1 to log(0.1)."

  (if (<= x 0.1)
      (log 0.1)
      (log x)))

(defun safe-sqrt (x)
  "Square root that ignores negative arguments."
  
  (if (< x 0)
      (sqrt (* -1 x))
      (sqrt x)))


;;; GENETIC PROGRAMMING parametars

;; operators
(defparameter *ops* '(+ - * safe-/))
(defparameter *ops2* '(safe-log sin safe-sqrt square))

;; variables
(defparameter *vars* '(velocity altitude fuel))

;; init parameters
(defparameter *population* 20)
(defparameter *mutation* 0.01)
(defparameter *tree-depth* 4)
(defparameter *stop-fitness* 1)

;; elements are (tree, tree-fitness)
(defvar *programs* nil
    "GP programs with fitnesses")


;;; Landing Simulation -----------------------------------------------


;;(defmacro eval-tree (nth-in-population fuel velocity altitude)
;;  "Evaluates 'GP program' with given values."
;;  `(funcall 
;;    (lambda ,*vars* ,(first (nth nth-in-population *programs*)))
;;    ,fuel ,velocity ,altitude))

;; check ... 
;;(defmacro eval-tree (tree-func fuel velocity altitude)
;;  "Evaluates 'GP program' with given values."
;;  `(funcall (lambda ,*vars* ,(eval tree-func)) ,fuel ,velocity ,altitude))


;; FIX --> ugly eval of programs/trees 
;; not working without global parametars !?

(defparameter fuel 0)
(defparameter velocity 0)
(defparameter altitude 0)

(defun eval-tree (nth-tree in-fuel in-velocity in-altitude)
  "Evaluates nth 'GP program' in population with given values."

  (let*
      ((tree (first (nth nth-tree *programs*)))
       (fuel in-fuel)
       (velocity in-velocity)
       (altitude in-altitude))
    (eval tree)))
;; ----------------------------------------


(defun simul-ignition (nth fuel velocity altitude)
  "Simulates ignition of truster, scales eval-tree to range [0 1]."

  (let
      ((ignition (/ (eval-tree nth fuel velocity altitude) 100)))
    (cond
      ((< ignition 0) 0)
      ((> ignition 1) 1)
      (t ignition))))


(defun simul-delta-velocity (mass force)
  "Returns delta velocity for given mass and produced force in deltatime"

  ;; G - F = m * a = m * v / t   --->
  ;; velocity = ( (G - F) / mass) * deltatime
  (* (/ (- (* mass *gravity*) force) mass) *deltatime*))


(defun simul-loop (nth &optional 
		   (fuel *start-fuel*)  
		   (velocity *start-velocity*) 
		   (altitude *start-altitude*))
  "Recursively simulates LM landing.

   For given state via arguments (ignition, mass/fuel, velocity, altitude)
   evaluates next state and checks end conditions (altitude, fuel, ...).

   Returns values fuel, velocity, altitude."

  ;; temp output
  ;;(format t "fuel: ~a; velocity: ~a; altitude: ~a ~%" fuel velocity altitude) 

  (cond

    ;; if altitude <= 0 "eagle has landed" :-)
    ((<= altitude 0) (values fuel velocity altitude))

    ;; if fuel <= 0 "houston, we have a problem" :-0
    ((<= fuel 0) (values fuel velocity  altitude))

    (t
     (let* (
	    ;; get ignition
	    (ignition (simul-ignition nth fuel velocity altitude))

	    ;; force produced by thruster
	    (force (* ignition *max-thrust*))
	    
	    ;; fuel left after consumption by thruster in deltatime
	    (next-fuel (- fuel (* ignition *deltatime* *thruster-consumption*)))
	    
	    ;; next velocity 
	    (next-velocity (+ velocity (simul-delta-velocity (+ *start-mass* fuel) force)))
	    
	    ;; new altitude crossed after current velocity in deltatime
	    (next-altitude (- altitude (* *deltatime* velocity))))

       (simul-loop nth next-fuel next-velocity next-altitude)))))



;;; Genetic Programing -----------------------------------------------


;; Generate population

(defun random-list-elem (list &optional elem)
  "Returns random element from given list.
   If elem is provided then function will return element not equal to passed one."

  (do* ((new-elem 
	(nth (random (length list)) list)
	(nth (random (length list)) list)))
       ((not (eql elem new-elem)) new-elem)))


(defun gen-random-tree (depth)
  "Generates random GP tree from *ops* and *vars*."

  (if (> depth 0)
      ;; 50% - 50% chance for ops 1. or ops 2.
      (if (= (random 2) 0)
	  (list (random-list-elem *ops*) (gen-random-tree (1- depth)) (gen-random-tree(1- depth)))
	  (list (random-list-elem *ops2*) (gen-random-tree (1- depth))))
      (random-list-elem *vars*)))


(defun gen-start-programs (n)
  "Creates initial population of programs.
   Clears list of programs, then generates n-population of random programs"

  (setf *programs* nil)
  (dotimes (i n)
    (let*
	((program (gen-random-tree *tree-depth* )))
    (push (list program 0) *programs*))))
    

;; Evaluate population

(defun calc-fitness (nth)
  "Calculates fitness based on results from landing simulation for nth program.
   Best fitness is 0 and worst 100."

  (multiple-value-bind
	(fuel velocity altitude)
      (simul-loop nth)
    (cond

      ;; not good if it's out of fuel
      ((< fuel 0) 100)

      ;; or not landed
      ((> altitude 0) 100)

      ;; ...
      ((> velocity 100) 100)

      ;; otherwise use velocity
      (t velocity))))


(defun eval-fitness (nth-prog)
  "Evaluates (calculates and saves) fitness for nth program."

  (setf (second (nth nth-prog *programs*)) (calc-fitness nth-prog)))


(defun sort-population ()
  "Sorts programs in population by fitness in ascedencing order."

  (setf *programs* (sort *programs* #'< :key #'second))

  ;; suppress output
  nil)
  

(defun eval-population ()
  "Evaluates all programs, saves their fitnesses and then sorts programs by fitness.
   First elem in *programs* is tree-function, and the second it's fitness."

  (progn
    ;; calculates fitnesses
    (dotimes (i *population*)
      (eval-fitness i))

    ;; sorts programs
    (sort-population)))


;; Apply genetic operators

(defun tree-size (tree)
  "Returns number of elements in nested list, in this case tree/program."

  (let 
      ((n 0))
    (dolist
	(elem tree)
      (cond
	((atom elem) (setf n (1+ n)))
	((listp elem) (setf n (+ n (tree-size elem))))))
    n))


;; --- function by Koza
(defun subtree-nth (tree pointer-to-tree index)
  "Given a tree or subtree, a pointer to that tree/subtree and
   an index return the component subtree that is numbered by
   Index.  We number left to right, depth first."
  (if (= index 0)
      (values pointer-to-tree (copy-tree tree) index)
      (if (consp tree)
          (do* ((tail (rest tree) (rest tail))
                (argument (first tail) (first tail)))
               ((not tail) (values nil nil index))
            (multiple-value-bind
                (new-pointer new-tree new-index)
                (subtree-nth argument tail (- index 1))
              (if (= new-index 0)
                  (return
                    (values new-pointer new-tree new-index))
                  (setf index new-index))))
          (values nil nil index))))
;; ---


(defun get-random-subtree-index (nth-prog)
  "Returns randomly selectes index of subtree in nth program.
   Must not be 0 --> don't want to crossover entire tree/program"

  (+ (random (- (tree-size (first (nth nth-prog *programs*))) 1)) 1))


(defun gp-crossover (nth-male nth-female)
  "Applies crossover on two programs.
   Programs are passed by their indexies in list *programs*, 
   new programs (breed) are pushed at the beginning of list *programs*.

   Subtrees are taken randomly except root node."

  (let*
      (
       ;; select parent subtree indexes
       (male-subtree (get-random-subtree-index nth-male))
       (female-subtree (get-random-subtree-index nth-female))

       ;; copy parents because of destructive operator (crossover) 
       (new-male (copy-tree (first (nth nth-male *programs*))))
       (new-female (copy-tree (first (nth nth-female *programs*)))))

    ;; test output
    ;;(print male-subtree)
    ;;(print female-subtree)

    ;; copies material/subtrees between male and female 
    (setf 
     (first (subtree-nth new-male nil male-subtree))
     (copy-tree (first (subtree-nth (first (nth nth-female *programs*)) nil female-subtree))))

    (setf 
     (first (subtree-nth new-female nil female-subtree))
     (copy-tree (first (subtree-nth (first (nth nth-male *programs*)) nil male-subtree))))

    ;; pushes new programs at the beginning
    (push (list new-female 0) *programs*)
    (push (list new-male 0) *programs*)

    ;; eval their fitnesses
    (eval-fitness 0)
    (eval-fitness 1)
  ))

(defun gp-mutation (nth)
  "Mutates one node of nth program in population.
   Takes care about type of node (ops, ops2, vars)."

  (let*
      ((index (get-random-subtree-index nth))
       (node (subtree-nth 
	      (first (nth nth *programs*)) 
	      nil 
	      index)))

    ;; operator or variable?
    (if (listp (first node))
	;; op1 or op2
	(if (member (caar node) *ops*)
	    (setf (caar node) (random-list-elem *ops* (caar node)))
	    (setf (caar node) (random-list-elem *ops2* (caar node))))
	;; variable
	(setf (car node) (random-list-elem *vars* (car node))))
    
    (eval-fitness nth)))
	    

(defun print-best (n)
  "Prints fitnesses of first/best n programs."
  
  (dotimes (i n)
    (print (second (nth i *programs*)))))


(defun test-gp (&optional (population *population*))
  "Test loop from GP."

  ;; optionaly change population
  (setf *population* population)
       
  ;; generates initial population of programs and evaluates fitnesses
  (gen-start-programs *population*)
  (eval-population)

  (do 
   ((i 0 (1+ i)))
   ((< (second (first *programs*)) *stop-fitness*) i)
    
    ;; apply crossover on first/best two programs
    (gp-crossover 0 1)
    
    ;; drops last/worst two programs --> to preserve population size
    (nbutlast *programs*)
    (nbutlast *programs*)
    
    ;; apply mutation --> every program has *mutation* posibility to mutate
    (dotimes (i *population*)
      (if (<= (random 1.0) *mutation*)
	  (gp-mutation i)))
    
    ;; sort programs
    (sort-population)))




;; --- some unused code left ... 

;; tree size
;;(defun tree-size (x)
;;  (cond
;;    ((null x) 0)
;;    ((atom x) 1)
;;    ((listp x) (+ (tree-size (car x)) (tree-size (cdr x))))))


(defun tree-flatten (list)
  "Flattens nested list."

  (loop for i in list if (listp i) append (tree-flatten i) else collect i))


;;(defun tree-nth (n tree)
;;  "Returns nth element in nested list, goes in depth by left side."
;;
;;  (if (= n 0)
;;      (values tree n)
;;      (if (listp tree)
;;	  (do*
;;	      ((tail (cdr tree) (cdr tail))
;;	       (elem (car tail) (car tail)))
;;	      ((not tail) (values nil n))
;;	    (multiple-value-bind
;;		  (new-tree new-n)
;;		(tree-nth (1- n) elem)
;;	      (if (= new-n 0)
;;		  (return (values new-tree new-n))
;;		  (setf n new-n))))
;;	  (values nil n))))
