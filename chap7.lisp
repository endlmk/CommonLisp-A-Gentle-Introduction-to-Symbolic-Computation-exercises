(defun rank (x)
  (first x))

(defun suit (x)
  (second x))

(setf my-hand
      '((3 hearts)
	(5 clubs)
	(2 diamonds)
	(4 diamonds)
	(acs spades)))

(defun count-suit (s x)
  (count-if #'(lambda (e) (equal (suit e) s)) x))

(setf colors
      '((clubs black)
	(diamonds red)
	(hearts red)
	(spades black)))

(defun color-of (hand)
  (second (assoc (suit hand) colors)))

(defun first-red (x)
  (find-if #'(lambda (e) (equal (color-of e) 'red)) x))

(defun black-cards (x)
  (remove-if-not #'(lambda (e) (equal (color-of e) 'black)) x))

(defun what-ranks (s x)
  (mapcar #'rank (remove-if-not #'(lambda (e) (equal (suit e) s)) x)))

(setf all-ranks '(2 3 4 5 6 7 8 9 10 jack queen king ace))

(defun beforep (x y l)
  (member y (member x l)))

(defun higher-rank-p (x y)
  (beforep (rank y) (rank x) all-ranks))

(defun high-card (x)
  (reduce #'(lambda (x y) (if (higher-rank-p x y) x y)) x))

(defun match-element (x y)
  (or (equal x y) (equal y '?)))

(defun match-triple (x y)
  (and (match-element (first x) (first y))
       (match-element (second x) (second y))
       (match-element (third x) (third y))))

(setf database
      '((b1 shape brick)
	(b1 color green)
	(b1 size small)
	(b1 supported-by b2)
	(b1 supported-by b3)
	(b2 shape brick)
	(b2 color red)
	(b2 size small)
	(b2 supports b1)
	(b2 left-of b3)
	(b3 shape brick)
	(b3 color red)
	(b3 size small)
	(b3 supports b1)
	(b3 right-of b2)
	(b4 shape pyramid)
	(b4 color blue)
	(b4 size large)
	(b4 supported-by b5)
	(b5 shape cube)
	(b5 color green)
	(b5 size large)
	(b5 supports b4)
	(b6 shape brick)
	(b6 color purple)
	(b6 size large)))

(defun fetch (p)
  (remove-if-not #'(lambda (e) (match-triple e p)) database))

(defun ask-color (b)
  (list b 'color '?))

(defun supporters (b)
  (mapcar #'first (fetch (list '? 'supports b))))

(defun supp-cube (b)
  (member 'cube (mapcar #'(lambda (e) (third (car (fetch (list e 'shape '?))))) (supporters b))))

(defun desc1 (b)
  (fetch (list b '? '?)))

(defun desc2 (x)
  (mapcar #'rest x))

(defun description (b)
  (reduce #'append (desc2 (desc1 b))))
