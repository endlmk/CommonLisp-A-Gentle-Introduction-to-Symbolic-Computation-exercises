(defun good-style (p)
  (let ((p (+ p 5)))
    (list 'result 'is p)))

(defun throw-die ()
  (let ((r (random 6)))
    (+ r 1)))

(defun throw-dice ()
  (let ((first (throw-die))
	(second (throw-die)))
    (list first second)))

(defun snake-eyes-p (l)
  (and (equal (first l) 1)
       (equal (second l) 1)))

(defun boxcars-p (l)
  (and (equal (first l) 6)
       (equal (second l) 6)))

(defun throw-value (l)
  (+ (first l) (second l)))

(defun instant-win-p (l)
  (member (throw-value l) '(7 11)))

(defun instant-loss-p (l)
  (member (throw-value l) '(2 3 12)))

(defun say-throw (l)
  (cond ((snake-eyes-p l) 'snake-eyes)
	((boxcars-p l) 'boxcars)
	(t (throw-value l))))

(defun craps ()
  (let* ((th (throw-dice))
	 (result (list 'throw (first th) 'and (second th)))
	 (v (say-throw th)))
    (cond ((instant-win-p th) (cons result (list '-- v '-- 'you 'win)))
	  ((instant-loss-p th) (cons result (list '-- v '-- 'you 'lose)))
	  (t (cons result (list '-- 'your 'point 'is v)))))))
