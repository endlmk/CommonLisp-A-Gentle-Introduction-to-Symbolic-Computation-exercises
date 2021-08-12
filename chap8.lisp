(defun laugh (n)
  (cond ((zerop n) nil)
	(t (cons 'ha (laugh (- n 1))))))

(defun add-up (x)
  (cond ((null x) 0)
	(t (+ (car x) (add-up (rest x))))))

(defun alloddp (x)
  (cond ((null x) t)
	((oddp (first x)) (alloddp (rest x)))
	(t nil)))

(defun rec-member (e x)
  (cond ((null x) nil)
	((equal e (first x)) x)
	(t (rec-member e (rest x)))))

(defun rec-assoc (k m)
  (cond ((null m) nil)
	((equal k (first (first m))) (first m))
	(t (rec-assoc k (rest m)))))

(defun rec-nth (n x)
  (cond ((null x) nil)
	((zerop n) (first x))
	(t (rec-nth (- n 1) (rest x)))))

(defun add1 (n)
  (+ n 1))

(defun sub1 (n)
  (- n 1))

(defun rec-plus (x y)
  (cond ((zerop y) x)
	(t (rec-plus (add1 x) (sub1 y)))))

(defun fib (n)
  (cond ((zerop n) 1)
	((equal n 1) 1)
	(t (+ (fib (- n 1)) (fib (- n 2))))))

(defun find-first-odd (x)
  (cond ((null x) nil)
	((oddp (first x)) (first x))
	(t (find-first-odd (rest x)))))

(defun last-element (x)
  (cond ((null (rest x)) (first x))
	(t (last-element (rest x)))))

(defun add-nums (n)
  (cond ((zerop n) 0)
	(t (+ n (add-nums (- n 1))))))

(defun all-equal (x)
  (cond ((< (length x) 2) t)
	((not (equal (first x) (second x))) nil)
	(t (all-equal (rest x)))))

(defun count-down (n)
  (cond ((zerop n) nil)
	(t (cons n (count-down (- n 1))))))

(defun fact (n)
  (reduce #'* (count-down n)))

(defun count-down1 (n)
  (cond ((equal -1 n) nil)
	(t (cons n (count-down1 (- n 1))))))

(defun count-down2 (n)
  (cond ((zerop n) '(0))
	(t (cons n (count-down2 (- n 1))))))

(defun square-list (x)
  (cond ((null x) nil)
	(t (let ((n (first x)))
	     (cons (* n n) (square-list (rest x)))))))

(defun my-nth (n x)
  (cond ((zerop n) (first x))
	((null x) nil)
	(t (my-nth (- n 1) (rest x)))))

(defun my-member (m x)
  (cond ((equal m (first x)) x)
	((null x) nil)
	(t (my-member m (rest x)))))

(defun my-assoc (k x)
  (cond ((equal k (first (first x))) (first x))
	((null x) nil)
	(t (my-assoc k (rest x)))))

(defun compare-lengths (x y)
  (cond ((and (null x) (not (null y))) 'second-is-longer)
	((and (not (null x)) (null y)) 'first-is-longer)
	((and (null x) (null y)) 'same-length)
	(t (compare-lengths (rest x) (rest y)))))

(defun sum-numeric-elements (x)
  (cond ((null x) 0)
	((numberp (first x)) (+ (first x) (sum-numeric-elements (rest x))))
	(t (sum-numeric-elements (rest x)))))

(defun my-remove (e x)
  (cond ((null x) nil)
	((not (equal e (first x))) (cons (first x) (my-remove e (rest x))))
	(t (my-remove e (rest x)))))

(defun my-intersection (x y)
  (cond ((null x) nil)
	((null y) nil)
	((member (first x) y) (cons (first x) (my-intersection (rest x) y)))
	(t (my-intersection (rest x) y))))

(defun my-set-difference (x y)
  (cond ((null x) nil)
	((null y) x)
	((not (member (first x) y)) (cons (first x) (my-set-difference (rest x) y)))
	(t (my-set-difference (rest x) y))))

(defun count-odd (x)
  (cond ((null x) 0)
	((oddp (first x)) (+ 1 (count-odd (rest x))))
	(t (count-odd (rest x)))))

(defun count-odd1 (x)
  (cond ((null x) 0)
	(t (+ (if (oddp (first x)) 1 0) (count-odd1 (rest x))))))

(defun count-atom (x)
  (cond ((null x) 1)
	((atom x) 1)
	(t (+ (count-atom (car x))
	      (count-atom (cdr x))))))

(defun sum-tree (x)
  (cond ((numberp x) x)
	((atom x) 0)
	(t (+ (sum-tree (car x))
	      (sum-tree (cdr x))))))

(defun my-subst (ra rb x)
  (cond ((null x) nil)
	((equal x rb) ra)
	((atom x) x)
	(t (cons (my-subst ra rb (car x))
		 (my-subst ra rb (cdr x))))))

(defun tree-depth (x)
  (cond ((atom x) 0)
	(t (+ 1 (max (tree-depth (car x))
		     (tree-depth (cdr x)))))))

(defun paren-depth (x)
  (cond ((atom x) 0)
	(t (max (+ 1 (paren-depth (car x)))
		(paren-depth (cdr x))))))

(defun count-up (n)
  (cond ((zerop n) nil)
	(t (append (count-up (- n 1)) (list n)))))

(defun make-loaf (n)
  (if (zerop n) nil (cons 'x (make-loaf (- n 1)))))

(defun bury (x n)
  (cond ((zerop n) x)
	(t (cons (bury x (- n 1)) nil))))

(defun pairing (x y)
  (cond ((null x) nil)
	((null y) nil)
	(t (cons (list (first x) (first y)) (pairing (rest x) (rest y))))))

(defun sublists (x)
  (cond ((null x) nil)
	(t (cons x (sublists (rest x))))))

(defun reverse-helper (w r)
  (cond ((null r) w)
	(t (reverse-helper (cons (first r) w) (rest r)))))

(defun my-reverse (x)
  (reverse-helper nil x))

(defun my-union (x y)
  (cond ((null y) x)
	((member (first y) x) (my-union x (rest y)))
	(t (my-union (cons (first y) x) (rest y)))))

(defun largest-even (x)
  (cond ((null x) 0)
	((oddp (first x)) (largest-even (rest x)))
	(t (max (first x) (largest-even (rest x))))))

(defun huge-helper (b n)
  (cond ((zerop n) 1)
	(t (* b (huge-helper b (- n 1))))))

(defun huge (b)
  (huge-helper b b))

(defun every-other-helper (r c)
  (cond ((null r) nil)
	(c (cons (first r) (every-other-helper (rest r) nil)))
	(t (every-other-helper (rest r) t))))

(defun every-other (x)
  (every-other-helper x t))

(defun left-half-helper (r n)
  (cond ((zerop n) nil)
	(t (cons (first r) (left-half-helper (rest r) (- n 1))))))

(defun left-half (x)
  (left-half-helper x (ceiling (length x) 2)))

(defun merge-lists (x y)
  (cond ((null x) y)
	((null y) x)
	((< (first x) (first y)) (cons (first x) (merge-lists (rest x) y)))
	(t (cons (first y) (merge-lists x (rest y))))))
(setf family
      '((colin nil nil)
	(deirdre nil nil)
	(arthur nil nil)
	(kate nil nil)
	(frank nil nil)
	(linda nil nil)
	(suzanne colin deirdre)
	(bruce arthur kate)
	(charles arthur kate)
	(david arthur kate)
	(ellen arthur kate)
	(george frank linda)
	(hillary frank linda)
	(andre nil nil)
	(tamara bruce suzanne)
	(vincent bruce suzanne)
	(wanda nil nil)
	(ivan george ellen)
	(julie george ellen)
	(marie george ellen)
	(nigel andre hillary)
	(frederick nil tamara)
	(zelda vincent wanda)
	(joshua ivan wanda)
	(quentin nil nil)
	(robert quentin julie)
	(olivia nigel marie)
	(peter nigel marie)
	(erica nil nil)
	(yvette robert zelda)
	(diane peter erica)))

(defun father (p)
  (second (assoc p family)))

(defun mother (p)
  (third (assoc p family)))

(defun parents (p)
  (remove-if #'null (list (father p) (mother p))))

(defun children (p)
  (reduce #'(lambda (w e)
	      (if (or (equal p (second e)) (equal p (third e))) (cons (first e) w) w)) family :initial-value nil))

(defun siblings-helper (e r)
  (cond ((null r) nil)
	((and
	  (or (equal (second e) (second (first r)))

	      (equal (third e) (third (first r))))
	  (not (equal (first e) (first (first r)))))
	 (cons (first (first r)) (siblings-helper e (rest r))))
	(t (siblings-helper e (rest r)))))

(defun siblings (p)
  (siblings-helper (assoc p family) family))

(defun mapunion (f x)
  (reduce #'union (mapcar f x)))

(defun grandparents (p)
  (mapunion #'parents (parents p)))

(defun cousin (p)
  (mapunion #'children (mapunion #'siblings (parents p))))

(defun descended-from (p a)
  (cond ((null p) nil)
	((member a (parents p)) t)
	(t (or (descended-from (father p) a)
	       (descended-from (mother p) a)))))

(defun ancestors (p)
  (cond ((null (parents p)) nil)
	(t (append (ancestors (father p))
		   (ancestors (mother p))
		   (parents p)))))

(defun generation-gap-helper-p (p q g)
  (cond ((null (parents q)) nil)
	((member p (parents q)) (+ g 1))
	(t (or (generation-gap-helper-p p (father q) (+ g 1))
	       (generation-gap-helper-p p (mother q) (+ g 1))))))

(defun generation-gap-helper-q (p q g)
  (cond ((null (parents p)) nil)
	((member q (parents p)) (+ g 1))
	(t (or (generation-gap-helper-q (father p) q (+ g 1))
	       (generation-gap-helper-q (mother p) q (+ g 1))))))

(defun generation-gap (p q)
  (or (generation-gap-helper-p p q 0)
      (generation-gap-helper-q p q 0)))
  
(defun count-up-helper (cnt n w)
  (cond ((> cnt n) w)
	(t (count-up-helper (+ cnt 1) n (append w (list cnt))))))

(defun my-count-up (n)
  (count-up-helper 1 n nil))

(defun fact-helper (cnt n w)
  (cond ((> cnt n) w)
	(t (fact-helper (+ cnt 1) n (* cnt w)))))

(defun fact (n)
  (fact-helper 1 n 1))

(defun union-helper (x y w)
  (cond ((null y) w)
	((member (first y) x) (union-helper x (rest y) w))
	(t (union-helper x (rest y) (cons (first y) w)))))

(defun my-union (x y)
  (union-helper x y x))

(defun intersection-helper (x y w)
  (cond ((null y) w)
	((member (first y) x) (intersection-helper x (rest y) (cons (first y) w)))
	(t (intersection-helper x (rest y) w))))

(defun my-intersection (x y)
  (intersection-helper x y nil))

(defun my-set-difference (x y)
  (union-helper y x nil))

(defun tree-find-if (fn x)
  (cond ((null x) nil)
	((atom (first x)) (if (funcall fn (first x)) (first x) nil))
	(t (or (tree-find-if fn (car x))
	       (tree-find-if fn (cdr x))))))

(defun tr-count-slices (x)
  (labels ((tr-cs1 (loaf n)
	     (cond ((null loaf) n)
		   (t (tr-cs1 (rest loaf) (+ n 1))))))
    (tr-cs1 x 0)))

(defun tr-reverse (x)
  (labels ((tr-rv1 (x w)
	     (cond ((null x) w)
		   (t (tr-rv1 (rest x) (cons (first x) w))))))
    (tr-rv1 x nil)))

(defun arith-eval (x)
  (cond ((numberp x) x)
	((equal (second x) '+) (+ (arith-eval (first x)) (arith-eval (third x))))
	((equal (second x) '-) (- (arith-eval (first x)) (arith-eval (third x))))
	((equal (second x) '*) (* (arith-eval (first x)) (arith-eval (third x))))
	((equal (second x) '/) (/ (arith-eval (first x)) (arith-eval (third x))))))

(defun legalp (x)
  (cond ((numberp x) t)
	((and (equal (length x) 3) (equal (second x) '+)) (and (legalp (first x)) (legalp (third x))))
	((and (equal (length x) 3) (equal (second x) '-)) (and (legalp (first x)) (legalp (third x))))
	((and (equal (length x) 3) (equal (second x) '*)) (and (legalp (first x)) (legalp (third x))))
	((and (equal (length x) 3) (equal (second x) '/)) (and (legalp (first x)) (legalp (third x))))
	(t nil)))

(defun factor-tree-helper (n p)
  (cond ((equal n p) n)
	((zerop (rem n p))
	 (list n p (factor-tree-helper (/ n p) p)))
	(t (factor-tree-helper n (+ p 1)))))

(defun factor-tree (n)
  (factor-tree-helper n 2))
	     
