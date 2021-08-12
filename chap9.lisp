
(defun draw-line (n)
  (cond ((zerop n) nil)
	(t (format t "*") (draw-line (- n 1)))))

(defun draw-box (n m)
  (cond ((zerop m) nil)
	(t (draw-line n) (format t "~%") (draw-box n (- m 1)))))

(defun print-line (b)
  (format t "~& ~A | ~A | ~A " (first b) (second b) (third b)))

(defun print-board (x)
  (let ((b2 (sublis '((x."X")(o."O")(nil." ")) x)))
    (print-line b2)
    (format t "~&-----------")
    (print-line (nthcdr 3 b2))
    (format t "~&-----------")
    (print-line (nthcdr 6 b2))))
  

(defun gross-pay ()
  (format t "Please type hourly wage.")
  (let ((x (read)))
    (format t "Please type number of hours.")
    (let ((h (read)))
      (format t "Gross pay is ~S.~%" (* x h)))))

(defun cookie-monster ()
  (format t "Give me cookie!!!~%")
  (format t "Cookie?")
  (let ((item (read)))
    (cond ((equal item 'cookie) (format t "Thank you!...Munch munch munch...BURP~%"))
	  (t (format t "No want ~A...~%" item) (cookie-monster)))))

(defun space-over (n)
  (cond ((> 0 n) (format t "Error!"))
	((zerop n) nil)
	(t (format t " ") (space-over (- n 1)))))

(defun plot-one-point (plotstr y)
  (space-over y)
  (format t "~A~%" plotstr))

(defun plot-points (plotstr x)
  (cond ((null x) nil)
	(t (plot-one-point plotstr (first x)) (plot-points plotstr (rest x)))))

(defun generate (m n)
  (cond ((> m n) nil)
	(t (cons m (generate (+ m 1) n)))))

(defun make-graph ()
  (let* ((fn (prompt-for "Function to graph?"))
	 (s (prompt-for "Starting x value?"))
	 (e (prompt-for "Ending x value?"))
	 (plotstr (prompt-for "Plotting string?")))
    (plot-points plotstr (mapcar fn (generate s e))) t))

(defun prompt-for (promptstr)
  (format t "~A" promptstr)
  (read))

(defun square (n)
  (* n n))

(defun dot-prin1 (x)
  (cond ((atom x) (format t "~S" x))
	(t (format t "(") (dot-prin1 (car x)) (format t " . ") (dot-prin1 (cdr x)) (format t ")"))))

(defun hybrid-prin1(x)
  (cond ((atom x) (format t "~S" x))
	(t (format t "(") (format t "~S" (car x)) (cdr-prin1 (cdr x)))))

(defun cdr-prin1 (x)
  (cond ((null x) (format t ")"))
	((atom x) (format t " . ~S)" x))
	(t (format t " ~S" (car x)) (cdr-prin1 (cdr x)))))
  
			     
	
