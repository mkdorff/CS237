
(defun poly (n)
  (- (* n n) n -41))

(defun is-poly-prime (n)
  (setq list '())
  (loop for i from 0 to n
        when (is-prime i)
        do (if (is-prime (poly n))
               (append list (poly n))
             nil)))



(defun factorial (n)
  (if (zerop n)
      1
    (* n (factorial (1- n)))))

(defun consecutive-composite-numbers-sequence-of-length (r)
   (let ((r+1-factorial (factorial (1+ r))))
     (loop for n from 1 to r
        collect (+ r+1-factorial n 1))))

(defun all-composite (r) ...)

(= (gcd 56 8) 8)
(= (gcd 65 15) 5)
(= (lcm 5 7) 35)
(= (lcm 4 6) 12)
(= (gcd 28 65) 1)

(defun gcd-set-t-or-nil (setList)
  (let* ((gcd-1 t)
         (count 0))
    (loop for i in setList
          do (setq count1 (+ count 1))
          do (loop for (+ count 1) to (length setList)
                   do (if (= 1 (gcd i (nth count1 setList)))
                          t
                        (setq gcd-1 nil))
                   do (setq count1 (+ count1 1)))
          do (setq count (+ count 1))
        )))

(= (* (gcd a b) (lcm a b)) (* a b))

(defun aaaabbbb (a b)
  (= (* (gcd a b) (lcm a b)) (* a b)))

(aaaabbbb 900000 900003)
t

(aaaabbbb 900000 90)
t

(aaaabbbb 12879 90)
t

(aaaabbbb 30 12)
t

(defun alternate-base-representation (n b)
  (let ((d (div n b))
        (m (mod n b)))
    (if (zerop d)
        (list m)
      (append (alternate-base-representation d b) (list m)))))

(alternate-base-representation 987654321 127)

(alternate-base-representation 987654321 127)
(3 101 20 87 86)

(alternate-base-representation 9 2)
(1 0 0 1)

(alternate-base-representation 321 2)
(1 0 1 0 0 0 0 0 1)

(alternate-base-representation 1234 3)
(1 2 0 0 2 0 1)

(defun gcd-candidate (a b)
  (let ((x a) (y b) r)
    (while (not (zerop y))
      (setq r (mod x y) x y y r))
    x))

(gcd-candidate 2234 2342349)
1117

(gcd-candidate 24 36)
12
