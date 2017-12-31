
The following two functions are from the GNU library.

(defun next-prime (n)
  "Return the next prime number after N, or else nil."
  (if (integerp n)
    (cond
     ((> n 1)
      (let* ((k (if (= 0 (% n 2));start k at next odd number after n
                    (1+ n)
                  (+ n 2))))
        (while (not (prime-p k)) ;and loop upward over odd numbers
          (setq k (+ k 2)))
        k))
     (t 2))
    nil))

(defun prime-p (n)
  "Return N if it is a prime number, else nil."
  (interactive)
  (if (integerp n)
      (cond ((< n 2) nil)               ;there are no primes below 2
            ((= n 2) 2)                 ;special case for the only even prime
            ((= 0 (% n 2)) nil)         ;there are no other even primes
            (t (catch 'RESULT           ;else we have a positive odd candidate value
                 (let ((limit (floor (sqrt n)))
                       (divisor 2))
                   (while (<= divisor limit)
                     (if (= 0 (% n divisor))
                         (throw 'RESULT nil))
                     (setq divisor (1+ divisor)))
                   n))))
    nil))

(defun find-p-and-q (message)
  (let* ((p-and-q-list '()))
    (add-to-list 'p-and-q-list (next-prime (+ 500 (random 500))))
    (append p-and-q-list (next-prime (+ 500 (random 500))))))

(defun calc-n-and-totient (p q)  
  (let* ((n-and-totient '()))
    (add-to-list 'n-and-totient (* p q))
    (add-to-list 'n-and-totient (* (- p 1) (- q 1)))
    (reverse n-and-totient)))

(defun find-e (totient) 65537)

(defun find-inverse (e totient)
   (let ((variable 0)
        (remainder totient)
        (newvar 1)
        (newremainder e))
    (while (calc-eval "$1 != 0" 'pred newremainder)
      (let ((quotient (calc-eval "$1 \\ $2" nil remainder newremainder)))
        (cl-psetf variable newvar
                  newvar (calc-eval "$1 - $2 * $3" nil
                                  variable quotient newvar))
        (cl-psetf remainder newremainder
                  newremainder (calc-eval "$1 - $2 * $3" nil
                                  remainder quotient newremainder))))
    (if (calc-eval "$1 < 0" 'pred variable)
        (calc-eval "$1 + $2" nil variable totient)
     (string-to-number variable))))

(defun from-base-27 (message)
  (let* ((message (upcase message))
         (count (- (length message) 1))
         (message-list '()))
    (loop for c across message
          do (add-to-list 'message-list (* (- c 64) (expt 27 count)))
          do (setq count (- count 1)))
    (loop for i in message-list
          sum i)))
  
(defun to-base-27 (message)
  (let* ((decrypted '())
         (count 1))
    (while (>= message (expt 27 (- count 1)))
      (add-to-list 'decrypted
                   (byte-to-string (+ (/
                                       (% message (expt 27 count))
                                       (expt 27 (- count 1)))
                                      64)))
      (setq message (- message (% message (expt 27 count))))
      (setq count (+ count 1)))
    (apply 'concat decrypted)))

(require 'calc) ; for big integer functions

(defun pow-mod (b n m)
  "Computes b^n%m (very inefficiently)."
  (let ((x 1))
    (dotimes (i n)
      (setq x (% (* x b) m)))
    x))

(defun rsa-encrypt (message &optional raw)
  (let* ((message (if raw message (from-base-27 message)))
         (p-and-q (find-p-and-q message))
         (p (car p-and-q))
         (q (cdr p-and-q))
         (n-and-totient (calc-n-and-totient p q))
         (n (car n-and-totient))
         (totient (cdr n-and-totient))
         (e (find-e totient))
         (encrypted (pow-mod message e n)))
    (list encrypted p q e)))

(defun rsa-decrypt (encrypted-p-q-e &optional raw)
  (let* ((encrypted (nth 0 encrypted-p-q-e))
         (p (nth 1 encrypted-p-q-e))
         (q (nth 2 encrypted-p-q-e))
         (e (nth 3 encrypted-p-q-e))
         (n-and-totient (calc-n-and-totient p q))
         (n (car n-and-totient))
         (totient (cdr n-and-totient))
         (d (find-inverse e totient))
         (message (pow-mod encrypted d n)))
    (if raw
        (list n totient d message)
      (to-base-27 message))))

(defun round-trip (message)
  (setq message (mapconcat #'identity (split-string message " ") "@"))
  (string= message (rsa-decrypt (rsa-encrypt message))))

(defun test-rsa (&optional arg-string)
  (let* ((message-with-spaces (or arg-string (getenv "ARGS")))
         (message (mapconcat #'identity (split-string message-with-spaces " ") "@"))
         (encrypted (rsa-encrypt message))
         (raw t)
         (decrypted (rsa-decrypt encrypted raw))
         (decrypted-message (to-base-27 (nth 3 decrypted)))
         (decrypted-message-with-spaces
          (mapconcat #'identity (split-string decrypted-message "@") " "))
         (success (string= message-with-spaces decrypted-message-with-spaces)))
    (princ (format "\"%s\"\nwas encoded and encrypted as\n%s\nthen decrypted as\n%s\nand decoded as\n\"%s\"\n"
                   message-with-spaces encrypted decrypted decrypted-message-with-spaces))
    (princ (if success "" "un"))
    (princ "successfully.\n")
    success))
