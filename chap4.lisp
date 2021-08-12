(defun make-even (x) (if (oddp x) (+ 1 x) x))

(defun further (x) (if (> x 0) (+ x 1) (- x 1)))

(defun my-not (x) (if x nil T))

(defun ordered (l) (if (< (first l) (second l)) l (list (second l) (first l)))) 

(defun my-abs (x)
  (cond ((< x 0) (- x))
	(t x)))

(DEFUN emphasize3 (x)
  (cond ((equal (first x) 'good) (cons 'great (rest x)))
	((equal (first x) 'bad) (cons 'awful (rest x)))
	(t (cons 'very x))))

(defun constrain (x min max)
  (cond ((> x max) max)
	((< x min) min)
	(t x)))

(defun constarin1 (x min max)
  (if (> x max) max (if (< x min) min x)))

(defun firstzero (x)
  (cond ((equal (first x) 0) 'first)
	((equal (second x) 0) 'second)
	((equal (third x) 0) 'third)))

(defun cycle (x)
  (cond ((equal x 99) 1)
	(t (+ x 1))))

(defun howcompute (x y z)
  (cond ((equal (+ x y) z) 'sum-of)
	((equal (* x y) z) 'product-of)
	(t '(beats me))))

(defun geq (x y)
  (or (> x y)
      (equal x y)))

(defun compute (x)
  (cond ((and (oddp x) (> x 0)) (* x x))
	((and (oddp x) (< x 0)) (* x 2))
	(t (/ x 2))))

(defun borgp (x y)
  (or (and (or (equal x 'boy) (equal x 'girl)) (equal y 'child))
      (and (or (equal x 'man) (equal x 'woman)) (equal y 'adult))))

(defun play (f s)
  (cond ((equal f s) 'tie)
	((or (and (equal f 'rock) (equal s 'scissors))
	     (and (equal f 'scissors) (equal s 'paper))
	     (and (equal f 'paper) (equal s 'rock)))
	 'first-win)
	((or (and (equal f 'rock) (equal s 'paper))
	     (and (equal f 'paper) (equal s 'scissors))
	     (and (equal f 'scissors) (equal s 'rock)))
	 'second-win)))

(defun and1 (x y z w)
  (cond (x
	 (cond (y
		(cond (z
		       (cond (w w)))))))))

(defun and2 (x y z w)
  (if x (if y (if z (if w w)))))

(defun compare1 (x y)
  (if (equal x y) 'same
      (if (< x y) 'bigger 'smaller)))

(defun compare2 (x y)
  (or (and (equal x y) 'same)
      (and (< x y) 'bigger)
      (and (> x y) 'smaller)))

(defun gtest (x y)
  (cond ((> x y) t)
	((zerop x) t)
	((zerop y) t)
	(t nil)))

(defun boilingp (temp scaling)
  (cond ((equal scaling 'fahrenheit) (> temp 212))
	((equal scaling 'celsius) (> temp 100))))

(defun boilingp1 (temp scaling)
  (or (and (equal scaling 'fahrenheit) (> temp 212))
      (and (equal scaling 'celsius) (> temp 100))))

(defun boilingp2 (temp scaling)
  (if (equal scaling 'fahrenheit) (> temp 212)
      (if (equal scaling 'celsius) (> temp 100))))

(defun logical-and1 (x y)
  (if x (if y t)))

(defun logical-and2 (x y)
  (cond (x (cond (y t)))))

(defun logical-or (x y)
  (or (and x t)  (and y t)))

(defun nand (x y) (not (and x y)))

(defun logical-and3 (x y)
  (nand (nand x y) (nand x y)))

(defun logical-or1 (x y)
  (nand (nand x x) (nand y y)))
