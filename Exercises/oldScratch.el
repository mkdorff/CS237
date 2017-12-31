q(setq A [verve vim vigor] B [butter vinegar pepper vigor])
[butter vinegar pepper vigor]

(defun set-union (vec1 vec2)
  (apply 'vector (union (append vec1 nil) (append vec2 nil))))
set-union

(setq Set1 (set-union A B))
[vim verve butter vinegar pepper vigor]

(defun pred (x)
  (= (elt (symbol-name x) 0) ?v))
pred

(remove-if-not 'pred Set1)
[vim verve vinegar vigor]

(remove-if-not (lambda (x) (= (elt (symbol-name x) 0) 118)) Set1)
[vim verve vinegar vigor]

(defun set-intersection (vec1 vec2)
  apply 'vector (intersection (append vec1 nil) (append vec2 nil)))
set-intersection

(setq Set2 (set-intersection A B))

(symbol-function 'pred)
(lambda (x) (= (elt (symbol-name x) 0) 118))


{x | x \subset Set1 /\ x starts with 'v'}
v

box
3 white
2 red

teacher
3 students

There's an underlying unassumption that the first students thought things through. That's not even closely fair.

red                            red               

white red                      white red





white white                    white

                               white red
                               


white

white white

awhite red

red red

red







ONE.DEF
(defun list-some-computations-on (a b c d)
  (list (+ a b) (/ d b) (- d a) (* c d)))
list-some-computations-on

(list-some-computations-on 1 2 3 4)
(3 2 3 12)

; Well of course there isn't any restriction on the objects a function can return or recieve. That's still pretty common in other languages as well (though C++ has a return type and the compiler evaluates the function being passed in) Though, functions need prototype parameters...

(defun square (number)
  "Squaring is multiplying a number by itself..."
  (* number number))
square

(square 5)
25

(let ((letters "abc"))
  (list  (symbol-name 'letters) (symbol-value 'letters) letters))
("letters" "abc" "abc")

(= 3 4)
nil

; 2.2.3.2
; this will obvious be nil
(let ((x 2)
      (y 3)
      (z 7))
  (= (+ x y z) (* x y z )))
nil

(let ((x 0)
      (y 0)
      (z 0))
  (= (+ x y z) (* x y z )))
t
; but it depends on if we're counting 0 as a number


; 2.2.3.3
; = is a equal to comparator
; I'll guess output will be (nil nil nil t)
; ?evaluates the following character as a character.
(let ((a 1)
      (b 2)
      (c 1))
  (list (= 1 2) (= a b) (= ?a ?A) (= a c)))
(nil nil nil t)


; 2.2.7
; Upside down A symbol. EACH or EVERY

; 2.2.8
; Backwards E symbol. There EXISTS

; 2.2.12.1
; D&C 130:20-21
; Answer is B. ANY/EVERY blessing is predicated on SOME law.



; Exploration 1
(defun greater-than (x y)
  "A function description"
  (> x y))
greater-than

(greater-than 3 4)
nil
(greater-than 4 3)
t


(require 'cl) ; for loop macro
cl

loop
"The Common Lisp `loop' macro.
Valid clauses include:
  For clauses:
    for VAR from/upfrom/downfrom EXPR1 to/upto/downto/above/below EXPR2 by EXPR3
    for VAR = EXPR1 then EXPR2
    for VAR in/on/in-ref LIST by FUNC
    for VAR across/across-ref ARRAY
    for VAR being:
      the elements of/of-ref SEQUENCE [using (index VAR2)]
      the symbols [of OBARRAY]
      the hash-keys/hash-values of HASH-TABLE [using (hash-values/hash-keys V2)]
      the key-codes/key-bindings/key-seqs of KEYMAP [using (key-bindings VAR2)]
      the overlays/intervals [of BUFFER] [from POS1] [to POS2]
      the frames/buffers
      the windows [of FRAME]
  Iteration clauses:
    repeat INTEGER
    while/until/always/never/thereis CONDITION
  Accumulation clauses:
    collect/append/nconc/concat/vconcat/count/sum/maximize/minimize FORM
      [into VAR]
  Miscellaneous clauses:
    with VAR = INIT
    if/when/unless COND CLAUSE [and CLAUSE]... else CLAUSE [and CLAUSE...]
    named NAME
    initially/finally [do] EXPRS...
    do EXPRS...
    [finally] return EXPR

For more details, see Info node `(cl)Loop Facility'.

(loop CLAUSE...)"

funcall
"Call first argument as a function, passing remaining arguments to it.
Return the value that function returns.
Thus, (funcall 'cons 'x 'y) returns (x . y).

(funcall FUNCTION &rest ARGUMENTS)"

(defun for-all (predicate domain)
  (loop for item across domain
	always (funcall predicate item)))
for-all

(defun for-some (predicate domain)
  (loop for item across domain
	thereis (funcall predicate item)))
for-some

evenp
"Return t if INTEGER is even.

(evenp INTEGER)"

oddp
"Return t if INTEGER is odd.

(oddp INTEGER)"

(for-all 'evenp [1 2 3])
nil
(for-some 'evenp [1 2 3])
t
(for-all 'evenp [2 4 6])
t
(for-some 'evenp [1 3 7])
nil
(for-all 'oddp [1 3 5])
t
(for-some 'oddp [2 6 10])
nil
(for-all 'oddp [5 4 3])
nil
(for-some 'oddp [6 4 3])
t

symbolp
"Return t if OBJECT is a symbol.

(symbolp OBJECT)"

(for-all 'symbolp [a b c])
t
(for-all 'symbolp [a b c 1 2 3])
nil

(defun square (n)
  "Square a number n by multiplying it by itself."
  (* n n))
square

square
"Square a number n by multiplying it by itself."
; Look at that, that's the definition.

zerop
"Return t if NUMBER is zero.

(zerop NUMBER)"


(defun is-pythagorean (x y)
  "Does x^2 + y^2 = z^2 --- a perfect square?"
  (let* ((z (sqrt (+ (square x) (square y))))
         (diff (- z (floor z))))
    (zerop diff)))
is-pythagorean

eq
"Return t if the two args are the same Lisp object.

(eq OBJ1 OBJ2)"

and
"Eval args until one of them yields nil, then return nil.
The remaining args are not evalled at all.
If no arg yields nil, return the last arg's value.

(and CONDITIONS...)"
(and t t t t (= 3 (+ 3 1)))
nil
t

or
"Eval args until one of them yields non-nil, then return that value.
The remaining args are not evalled at all.
If all args return nil, return nil.

(or CONDITIONS...)"

;; sample binary predicate using everyday things
(defun is-typically-eaten (food meal)
  "Very incomplete knowledge-base of food and meals."
  (or (and (eq meal 'breakfast)
           (eq food 'eggs))
      (and (eq meal 'breakfast)
	   (eq food 'bacon))
      (and (eq meal 'breakfast)
           (eq food 'toast))
      (and (eq meal 'lunch)
           (eq food 'sandwich))
      (and (eq meal 'dinner)
           (eq food 'steak))
      (and (eq meal 'dinner)
           (eq food 'potatoes))))
is-typically-eaten

(defun tf (t-or-nil)
  "Convenience adapter converting t to \"TRUE\" and nil to \"FALSE\"."
  (if t-or-nil "TRUE" "FALSE"))
tf

princ
"Output the printed representation of OBJECT, any Lisp object.
No quoting characters are used; no delimiters are printed around
the contents of strings.

OBJECT is any of the Lisp data types: a number, a string, a symbol,
a list, a buffer, a window, a frame, etc.

A printed representation of an object is text which describes that object.

Optional argument PRINTCHARFUN is the output stream, which can be one
of these:

   - a buffer, in which case output is inserted into that buffer at point;
   - a marker, in which case output is inserted at marker's position;
   - a function, in which case that function is called once for each
     character of OBJECT's printed representation;
   - a symbol, in which case that symbol's function definition is called; or
   - t, in which case the output is displayed in the echo area.

If PRINTCHARFUN is omitted, the value of `standard-output' (which see)
is used instead.

(princ OBJECT &optional PRINTCHARFUN)"

(defun run-test (function predicate domain1 domain2 expected)
  "Run function with predicate and domains to get ACTUAL result.
   Check to see if ACTUAL is the same as EXPECTED, with clarity of test
   output achieved by AT-A-GLANCE matching of TRUE with TRUE or FALSE with FALSE."
  (let ((actual (funcall function predicate domain1 domain2)))
    (princ (format "\nFor the predicate '%s\nand domains\n%s and %s:
%s\nwas expected to return\n%s, the actual value returned was\n%s.\n\n"
                   predicate domain1 domain2 function (tf expected) (tf actual)))
    t))
run-test

intern
"Return the canonical symbol whose name is STRING.
If there is none, one is created by this function and returned.
A second optional argument specifies the obarray to use;
it defaults to the value of `obarray'.

(intern STRING &optional OBARRAY)"

; ...what?
(defun test-any-or-all (&rest test-names)
  "Test some or all of many tests identified by their symbol codenames."
  (let* ((all-testnames '(true-for-all-for-all false-for-all-for-all
			  true-for-all-for-some false-for-all-for-some
			  true-for-some-for-all false-for-some-for-all
			  true-for-some-for-some false-for-some-for-some))
         (todo-testnames (if (null test-names) all-testnames test-names)))
    (loop for test-name in todo-testnames
          always (funcall (intern (format "test-%s" test-name))))))
test-any-or-all

; I'm so done with these acronynm. I'm being verbose and changing this code. 
; It took me forever to realize what was being written.

; I will need to create 4 functions
; for-all-for-all

;; this is fixed based off what matt gave me
(defun for-all-for-all (predicate domain1 domain2)
  (loop for item1 across domain1
	always (loop for item2 across domain2
		     always (funcall predicate item1 item2))))
(for-all-for-all '> [1 2 3] [4 5 6])
nil
(for-all-for-all '> [4 5 6] [1 2 3])
t




 
	      
; for-all-for-some
(defun for-all-for-some (predicate domain1 domain2)
  (loop for item1 across domain1
	always (loop for item2 across domain2
		     thereis (predicate item1 item2))))
for-all-for-some




; for-some-for-all
(defun for-some-for-all (predicate domain1 domain2)
  (loop for item1 across domain1
	thereis (loop for item2 across domain2
		     always (predicate item1 item2))))
for-some-for-all




; for-some-for-some
(defun for-some-for-some (predicate domain1 domain2)
  (loop for item1 across domain1
	thereis (loop for item2 across domain2
		     thereis (predicate item1 item2))))
for-some-for-some



; I also need to finish writing the test functions
; TAS, FAS, TSA, FSA, TSS, FSS


(defun test-true-for-all-for-all ()
  (let ((expect-true t))
    (run-test 'for-all-for-all 'greater-than [4 5 6] [1 2 3] expect-true)
    ;; use '> directly, without the 'greater-than wrapper.
    (run-test 'for-all-for-all '> [4 5 6] [1 2 3] expect-true)
    ;; my-numerical-predicate needs to be defined for this to work
    ; (run-test 'for-all-for-all 'my-numerical-predicate [42 31 20] [18 23 5] expect-true)
    (run-test 'for-all-for-all 'is-typically-eaten [bacon eggs] [breakfast] expect-true)
    ))
test-true-for-all-for-all


; When we have gained confidence, we will execute the below function
(test-true-for-all-for-all)
; This throw so many error's I don't even know where to start.
; I think I'll write al the function how I think they should run and then call that
; good for now.
(test-true-for-all-for-some)





(defun test-false-for-all-for-all ()
  (let ((expect-false nil))
    (run-test 'for-all-for-all '> [1 2 3] [4 5 6] expect-false)
    (run-test 'for-all-for-all 'is-typically-eaten [bacon sandwich] [breakfast lunch]
	      expect-false)
    ))
test-false-for-all-for-all

; the following are stub functions. we must make and run them 
(defun test-true-for-all-for-some ()
  (let ((expect-true t))
    (runt-test 'for-all-for-some '> [4 5 6] [1 2 7] expect-true)
    ))
test-true-for-all-for-some


; I kind of don't want to write more because I know it's not working. I'll 
; write psuedo code
false-for-all-for-some
; greater than works really great for all of them
; '> [1 2 7] [4 5 6]


true-for-some-for-all
; '> [1 4 5] [2 3 4]



false-for-some-for-all
; '> [2 3 4] [1 4 5]


true-for-some-for-some
; '> [1 5 7] [2 4 6]



false-for-some-for-some
; '> [1 2 3] [5 6 7]





represntational fluency for functions


xRy /\ yRx --> x = y

Antisymettric relationship

xRy /\ x != y --> !yRx

the contrapostive

yRx --> != (xRy /\ x != y)

DeMorgan's law

yRx --> ! xRy \/ x = y

conditional elimination

!yRx \/ ! xRy \/ x =y

reverse demorgan

!(xRy /\ xRy) \/ x 






(defun vector-to-string (array)
  (loop for item across array
	collect (symbol-name item)))

(vector-to-string [a b c])
("a" "b" "c")

(defun vector-to-string (array)
  (loop for item across array
	collect (concat (symbol-name item) " ")))
vector-to-string


(vector-to-string [a b c])
("a " "b " "c ")



(defun vector-to-string (array)
  (loop with answer = ""
	for symbol across array
	do (setq answer (concat answer (symbol-name item) " "))
	finally return (subtring answer 0 (1- (length answer)))))

(vector-to-string [a b c])

(defun vector-to-string (vector)
  (mapconcat 'symbol-name vector " "))
vector-to-string


(vector-to-string [a b c])
"a b c"

alt-x info


(map 'vector 'symbol-name [apple peer banana])
["apple" "peer" "banana"]

(aref "asdfasdfasdf" 0)
97






(defun f1 (symbol)
  (let ((a 1) (b 2) (c 3) (d 3))
    (if (eq symbol 'a) a
      (if (eq symbol 'b) b
	(if (eq symbol 'c) c
	  (if (eq symbol 'd) d))))))
f1

(f1 'a)
1

(f1 'b)
2

(f1 'c)
3

(f1 'e)
nil

(defun f1 (symbol)
  (case symbol
    ((a b c) (- (aref (symbol-name symbol) 0) 96))
    (d 3)))
f1

(list (f1 'a) (f1 'b) (f1 'c) (f1 'd))
(1 2 3 3)

assoc
"Return non-nil if KEY is `equal' to the car of an element of LIST.
The value is actually the first element of LIST whose car equals KEY.

(assoc KEY LIST)"
car
"Return the car of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a cons cell.  See also `car-safe'.

See Info node `(elisp)Cons Cells' for a discussion of related basic
Lisp concepts such as car, cdr, cons cell and list.

(car LIST)"
cdr
"Return the cdr of LIST.  If arg is nil, return nil.
Error if arg is not nil and not a cons cell.  See also `cdr-safe'.

See Info node `(elisp)Cons Cells' for a discussion of related basic
Lisp concepts such as cdr, car, cons cell and list.

(cdr LIST)"

(defun f1 (symbol)
  (let ((alist '((a . 1) (b . 2) (c . 3) (d . 3))))
    (cdr (assoc symbol alist))))
f1

f1

(f1 'a)
(a . 1)


(f1 'a)
1

(f1 'g)
nil

(list (f1 'a) (f1 'b) (f1 'c) (f1 'd))
(1 2 3 3)

member
"Return non-nil if ELT is an element of LIST.  Comparison done with `equal'.
The value is actually the tail of LIST whose car is ELT.

(member ELT LIST)"



(defun number-of-binary-relations-on (set)
  "Computes the number of binary relations on a set with *n* elements."
  (let ((n (size set))))
  (* n n))


(retrieve-package 'ment)
388
(require 'ment)
ment

(symbol-function 'gmy)
(lambda nil (interactive) (goto-char (point-max)) (insert (grab-more-yahtzee-analysis)) (save-buffer))
(symbol-function 'grab-more-yahtzee-analysis)
(lambda nil (if (executable-find "curl") (shell-command-to-string "curl -s https://firstthreeodds.org/yans.org") (error "No curl, no grabbing more")))

(princ "Hellp")
Hellp"Hellp"



(setq identity [[1 0 0]
		[0 1 0]
		[0 0 1]])
[[1 0 0] [0 1 0] [0 0 1]]

(setq A [[2 -1 1]
	 [4 1 -1]
	 [1 1 1]])
[[2 -1 1] [4 1 -1] [1 1 1]]

(setq b [[1]
	 [5]
	 [0]])
[[1] [5] [0]]

(defun dot* (v1 v2)
  (apply '+ (map 'list '* v1 v2)))
dot*


(dot* (aref A 0) (aref A 1))
6

(defun mi()
  (interactive)
  (insert (shell-command-to-string "curl -s https://firstthreeodds.org/mi.org"))
  (save-buffer))





| 2 | -1 |  1 | 1 |
| 4 |  1 | -1 | 5 |
| 1 |  1 |  1 | 0 |


1

3.0

path-seperator

(+ 1 2 3)
6

(* (* 2 3)
   (/ 8 4))
12


(message "Hello")
"Hello"

(insert " ; inserted text!")
 ; inserted text!nil

(quote (1 2 3))
(1 2 3)

'(1 2 (3 4 5) (6 (7)))
(1 2 (3 4 5) (6 (7)))

(car '(1 2 3))
1

(cdr '(1 2 3))
(2 3)

'()
nil
()
nil
nil

(null nil)
t

(cdr '(42))
nil


CAR gives us the first item
CDR gives us the rest of a list
LISP is built on lists.

(cons 0 '(1 2 3))
(0 1 2 3)

(1 2 3)

(cons 1 (cons 2 (cons 3 '())))
(1 2 3)

(append '(1 2) '(3 4))
(1 2 3 4)

SET takes a symbol and makes that quoted symbol equal to that value
set 'var = setq var

(set 'some-list '(1 2 3))
(1 2 3)
some-list
(1 2 3)
'some-list
some-list



(setq my-list '(foo bar baz))
(foo bar baz)

my-list
(foo bar baz)

(let ((a 1)
      (b 5))
  (format "a is %d and b is %d" a b))
"a is 1 and b is 5"

LET just sets local variables in that scope
LET* is defined in series as apposed to parallel. Thus the following works

(let* ((a 1)
       (b (+ a 5)))
  (format "a is %d and b is %d" a b))
"a is 1 and b is 6"


(defun say-hello ()
  (message "hello!"))

(say-hello)
"hello!"

(defun square (x)
  (* x x))

(square 2)
4

defun
"Define NAME as a function.
The definition is (lambda ARGLIST [DOCSTRING] BODY...).
See also the function `interactive'.
DECL is a declaration, optional, of the form (declare DECLS...) where
DECLS is a list of elements of the form (PROP . VALUES).  These are
interpreted according to `defun-declarations-alist'.
The return value is undefined.

(defun NAME ARGLIST &optional DOCSTRING DECL &rest BODY)"

(defun distance (x1 y1 x2 y2)
  (sqrt (+ (square (- x2 x1))
	   (square (- y2 y1)))))

(distance 3 0 0 4)
5.0

LETS TRY TO GET A DISTANCE FUNCTION WORKING ACCEPTING LISTS AS INPUT

(defun mmmm (p0 p1)
  (sqrt (+ (square (- (car p0) (car p1)))
	   (square (- (cdr p0) (cdr p1))))))
(mmmm '(3 0) '(0 4))

still cant get it :/

WHEN is like the nice if. one statement and only runs if true, no else to worry about
(when (= (+ 2 2) 4)
  (message "true"))
"true"

IF has two part to it.
if
"If COND yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE's.
THEN must be one expression, but ELSE... can be zero or more expressions.
If COND yields nil, and there are no ELSE's, the value is nil.

(if COND THEN ELSE...)"
 
(defun evens-or-odds (n)
  (if (= 0 (% n 2))
      "even"
    "odd"))

(evens-or-odds 2)
"even"

(evens-or-odds 3)
"odd"

(defun pick-a-word (n)
  (cond
   ((= n 1) "bulb")
   ((= n 2) "asdf")
   ((= n 3) "meow")
   (t "rawr")))

(pick-a-word 67)
"rawr"
"asdf"

(defun factorial (n)
  (if (< n 1)
      1
    (* n (factorial (- n 1)))))

(factorial 9)
362880

(factorial 20)
-2178784010250747904
1353190950536478720
121645100408832000
2

ANONYMOUS FUNCTIONS

(lambda (x) (* x x x))
((lambda (x) (* x x x)) 5)
125

((lambda (x y) (+ x y)) 1 2)
3

fset
"Set SYMBOL's function definition to DEFINITION, and return DEFINITION.

(fset SYMBOL DEFINITION)"

(fset 'cube (lambda (x) (* x x x)))
(lambda (x) (* x x x))

(cube 4)
64

mapcar
"Apply FUNCTION to each element of SEQUENCE, and make a list of the results.
The result is a list just as long as SEQUENCE.
SEQUENCE may be a list, a vector, a bool-vector, or a string.

(mapcar FUNCTION SEQUENCE)"

(mapcar 'cube '(1 2 3 4 5 6))
(1 8 27 64 125 216)

(mapcar 'upcase '("a" "b" "c"))
("A" "B" "C")

oddp
"Return t if INTEGER is odd.

(oddp INTEGER)"

remove-if-not
"Remove all items not satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.

Keywords supported:  :key :count :start :end :from-end

(remove-if-not PREDICATE SEQ [KEYWORD VALUE]...)"

(remove-if-not 'oddp '(0 1 2 3 4 5 6 7 8))
(1 3 5 7)

(defun qs (list)
  (if (null list)
      '()
    (let* ((pivot (car list))
	   (rest (cdr list))
	   (lesser (remove-if-not
		    (lambda (x) (<= x pivot)) rest))
	   (greater (remove-if-not
		     (lambda (x) (> x pivot)) rest)))
      (append (qs lesser) (list pivot) (qs greater)))))

(qs '(3 4 5 2 6 4 3 6 7 5 5 3 11 12 56 3 2 3 4 1 1))
(1 1 2 2 3 3 3 3 3 4 4 4 ...)

sor






























(require 'cl) ; for loop macro

(defun for-all (predicate domain)
  (loop for item across domain
	always (funcall predicate item)))

(defun for-some (predicate domain)
  (loop for item across domain
	thereis (funcall predicate item)))


; I made these
; for-all-for-all
(defun for-all-for-all (predicate domain1 domain2)
  (loop for item1 across domain1
	always (loop for item2 across domain2
		     always (funcall predicate item1 item2))))
 	      
; for-all-for-some
(defun for-all-for-some (predicate domain1 domain2)
  (loop for item1 across domain1
	always (loop for item2 across domain2
		     thereis (funcall predicate item1 item2))))

; for-some-for-all
(defun for-some-for-all (predicate domain1 domain2)
  (loop for item1 across domain1
	thereis (loop for item2 across domain2
		     always (funcall predicate item1 item2))))


; for-some-for-some
(defun for-some-for-some (predicate domain1 domain2)
  (loop for item1 across domain1
	thereis (loop for item2 across domain2
		     thereis (funcall predicate item1 item2))))


(defun tf (t-or-nil)
  "Convenience adapter converting t to \"TRUE\" and nil to \"FALSE\"."
  (if t-or-nil "TRUE" "FALSE"))

(defun run-test (function predicate domain1 domain2 expected)
  "Run function with predicate and domains to get ACTUAL result.
     Check to see if ACTUAL is the same as EXPECTED, with clarity of test 
     output achieved by AT-A-GLANCE matching of TRUE with TRUE or FALSE with FALSE."
  (let ((actual (funcall function predicate domain1 domain2)))
    (princ (format "\nFor the predicate '%s\nand domains\n%s and %s:
  %s\nwas expected to return\n%s, the actual value returned was\n%s.\n\n"
  		   predicate domain1 domain2 function (tf expected) (tf actual)))
    t))

(defun test-any-or-all (&rest test-names)
  "Test some or all of many tests identified by their symbol codenames."
  (let* ((all-testnames '(TAA FAA TAS FAS TSA FSA TSS FSS))
  	 (todo-testnames (if (null test-names) all-testnames test-names)))
    (loop for test-name in todo-testnames
  	  always (funcall (intern (format "test-%s" test-name))))))


; Brother Neff wrote this one
(defun greater-than (x y)
  "A function of arity 2 that wraps the one already bound to '>."
  (> x y))

; My Numerical Predicate
(defun is-x-multiple-of-y (x y)
  "This function checks if the first parameter is a multiple of the second"
  (if (eq y 0)
      nil
    (let* ((z (mod x y)))
       (if (eq z 0)
	   t
	 nil))))

; My Everyday Example
; Specific colors that we would see in certain terrains
(defvar terrain-colors
  '((forest brown green orange tan yellow)
    (marshlands blue brown green red tan teal)
    (desert black brown green tan)
    (mountains brown green orange tan white)))

; Function needed to compare an element in any two terrains
(defun compare-colors (terrain color)
  (not (null (member color (cdr (assoc terrain terrain-colors))))))

; Here are the tests
(defun test-TAA ()
  (let ((expect-true t))
    (run-test 'for-all-for-all '> [4 5 6] [1 2 3] expect-true)
    (run-test 'for-all-for-all 'is-x-multiple-of-y [3 6 9] [1 3] expect-true)
    (run-test 'for-all-for-all 'compare-colors [forest desert] [brown green] expect-true)
    ))

(defun test-FAA ()
  (let ((expect-false nil))
    (run-test 'for-all-for-all '> [0 -1 2] [7 5 6] expect-false)
    (run-test 'for-all-for-all 'is-x-multiple-of-y [3 6 10] [1 3] expect-false)
    (run-test 'for-all-for-all 'compare-colors [forest desert] [yellow green] expect-false)
    ))

(defun test-TAS ()
  (let ((expect-true t))
    (run-test 'for-all-for-some '> [3 4 5] [1 2 7] expect-true)
    (run-test 'for-all-for-some 'is-x-multiple-of-y [3 6 9] [2 3] expect-true)
    (run-test 'for-all-for-some 'compare-colors [forest desert] [orange green] expect-true)
    ))

(defun test-FAS ()
  (let ((expect-false nil))
    (run-test 'for-all-for-some '> [1 2 3] [19 2 20] expect-false)
    (run-test 'for-all-for-some 'is-x-multiple-of-y [1 6 9] [2 3] expect-false)
    (run-test 'for-all-for-some 'compare-colors [forest mountains] [black red] expect-false)
    ))

(defun test-TSA ()
  (let ((expect-true t))
    (run-test 'for-some-for-all '> [0 2 5] [1 2 3] expect-true)
    (run-test 'for-some-for-all 'is-x-multiple-of-y [2 8 9] [2 4] expect-true)
    (run-test 'for-some-for-all 'compare-colors [forest mountains] [yellow green] expect-true)
    ))

(defun test-FSA ()
  (let ((expect-false nil))
    (run-test 'for-some-for-all '> [0 1 2] [3 4 5] expect-false)
    (run-test 'for-some-for-all 'is-x-multiple-of-y [3 7 9] [2 4] expect-false)
    (run-test 'for-some-for-all 'compare-colors [desert mountains] [yellow green] expect-false)
    ))

(defun test-TSS ()
  (let ((expect-true t))
    (run-test 'for-some-for-some '> [2 4 6] [1 3 5] expect-true)
    (run-test 'for-some-for-some 'is-x-multiple-of-y [1 14 30] [7 13] expect-true)
    (run-test 'for-some-for-some 'compare-colors [forest mountains] [yellow red] expect-true)
    ))

(defun test-FSS ()
  (let ((expect-false nil))
    (run-test 'for-some-for-some '> [1 2 3] [4 5 6] expect-false)
    (run-test 'for-some-for-some 'is-x-multiple-of-y [17 19 21] [5 9] expect-false)
    (run-test 'for-some-for-some 'compare-colors [forest desert] [blue red] expect-false)
    ))

(test-any-or-all)

For the predicate '>
and domains
[4 5 6] and [1 2 3]:
  for-all-for-all
was expected to return
TRUE, the actual value returned was
TRUE.


For the predicate 'is-x-multiple-of-y
and domains
[3 6 9] and [1 3]:
  for-all-for-all
was expected to return
TRUE, the actual value returned was
TRUE.


For the predicate 'compare-colors
and domains
[forest desert] and [brown green]:
  for-all-for-all
was expected to return
TRUE, the actual value returned was
TRUE.


For the predicate '>
and domains
[0 -1 2] and [7 5 6]:
  for-all-for-all
was expected to return
FALSE, the actual value returned was
FALSE.


For the predicate 'is-x-multiple-of-y
and domains
[3 6 10] and [1 3]:
  for-all-for-all
was expected to return
FALSE, the actual value returned was
FALSE.


For the predicate 'compare-colors
and domains
[forest desert] and [yellow green]:
  for-all-for-all
was expected to return
FALSE, the actual value returned was
FALSE.


For the predicate '>
and domains
[3 4 5] and [1 2 7]:
  for-all-for-some
was expected to return
TRUE, the actual value returned was
TRUE.


For the predicate 'is-x-multiple-of-y
and domains
[3 6 9] and [2 3]:
  for-all-for-some
was expected to return
TRUE, the actual value returned was
TRUE.


For the predicate 'compare-colors
and domains
[forest desert] and [orange green]:
  for-all-for-some
was expected to return
TRUE, the actual value returned was
TRUE.


For the predicate '>
and domains
[1 2 3] and [19 2 20]:
  for-all-for-some
was expected to return
FALSE, the actual value returned was
FALSE.


For the predicate 'is-x-multiple-of-y
and domains
[1 6 9] and [2 3]:
  for-all-for-some
was expected to return
FALSE, the actual value returned was
FALSE.


For the predicate 'compare-colors
and domains
[forest mountains] and [black red]:
  for-all-for-some
was expected to return
FALSE, the actual value returned was
FALSE.


For the predicate '>
and domains
[0 2 5] and [1 2 3]:
  for-some-for-all
was expected to return
TRUE, the actual value returned was
TRUE.


For the predicate 'is-x-multiple-of-y
and domains
[2 8 9] and [2 4]:
  for-some-for-all
was expected to return
TRUE, the actual value returned was
TRUE.


For the predicate 'compare-colors
and domains
[forest mountains] and [yellow green]:
  for-some-for-all
was expected to return
TRUE, the actual value returned was
TRUE.


For the predicate '>
and domains
[0 1 2] and [3 4 5]:
  for-some-for-all
was expected to return
FALSE, the actual value returned was
FALSE.


For the predicate 'is-x-multiple-of-y
and domains
[3 7 9] and [2 4]:
  for-some-for-all
was expected to return
FALSE, the actual value returned was
FALSE.


For the predicate 'compare-colors
and domains
[desert mountains] and [yellow green]:
  for-some-for-all
was expected to return
FALSE, the actual value returned was
FALSE.


For the predicate '>
and domains
[2 4 6] and [1 3 5]:
  for-some-for-some
was expected to return
TRUE, the actual value returned was
TRUE.


For the predicate 'is-x-multiple-of-y
and domains
[1 14 30] and [7 13]:
  for-some-for-some
was expected to return
TRUE, the actual value returned was
TRUE.


For the predicate 'compare-colors
and domains
[forest mountains] and [yellow red]:
  for-some-for-some
was expected to return
TRUE, the actual value returned was
TRUE.


For the predicate '>
and domains
[1 2 3] and [4 5 6]:
  for-some-for-some
was expected to return
FALSE, the actual value returned was
FALSE.


For the predicate 'is-x-multiple-of-y
and domains
[17 19 21] and [5 9]:
  for-some-for-some
was expected to return
FALSE, the actual value returned was
FALSE.


For the predicate 'compare-colors
and domains
[forest desert] and [blue red]:
  for-some-for-some
was expected to return
FALSE, the actual value returned was
FALSE.

t



(defvar *varcnt -1)
*varcnt

(defun mything ()
  (aref))

(defun dot* (v1 v2)
  (apply '+ (map 'list '* v1 v2)))

(dot* [70 21 15] [1 1 3])
136



[]

(defun poop (v1 v2)
  (let 'i 1))


(poop 1 2 3)
1

(defun brute-force (r1 r2 r3 m1 m2 m3)
  (loop for n from 0 below (* m1 m2 m3)
	if (and (= r1 (% n m1))
		(= r2 (% n m2))
		(= r3 (% n m3)))
	return n))

(brute-force 1 1 3 3 5 7)
31

(defun brute-force-solve-congruent (vm vr)
  (loop for n from 0 below (* m1 m2 m3)
	if (and (= r1 (% n m1))
		(= r2 (% n m2))
		(= r3 (% n m3)))
	return n))

(map 'list '* [2 3 4])

(setq meow [2 3 4])
[2 3 4]

(iterate (for i in meow)
	 (* i))



(defun gcd (a b)
  (let ((x a)
	(y b))
    (while (not (zerop y))
      (setq x (/ x y)
	    y (% x y))
      )
    x))
(gcd 97 11)
1


(defun gcdr (a b)
  (if (zerop b)
      a
    (gcdr b (% a b))))

(defun ax+by (a x b y)
  (insert (format "%d + %d = %d" (* a x) (* b y) (+ (* a x) (* b y)))))














(map 'list (lambda (m) (% 987 m)) [7 11 13])
(0 8 12)

(4 2 6)

(2 3 4)

(3 2 1)



(% 191 7)
2

(% 191 11)
4

(% 191 13)
9




(% 432 7)
5

(% 432 11)
(% 432 13)




(% 492 7)
2

(% 492 11)
8

(% 492 13)




(% 131 7)
5

(% 131 11)
(% 131 13)










 |         |             |   | oenology |   |            |   |   |
 |         | campanology |   |          |   | phrenology |   |   |
 | alchemy |             |   |          |   |            |   |   |
 |         |             |   |          |   |            |   |   |
 |         |             |   |          |   |            |   |   |
 |         |             |   |          |   |            |   |   |
 |         |             |   |          |   |            |   |   |
 |         |             |   |          |   |            |   |   |
 |         |             |   |          |   |            |   |   |





HUFFMAN CODES

(setq message '("she" "sells" "sea" "shells"))
("she" "sells" "sea" "shells")


(mapconcat
"Apply FUNCTION to each element of SEQUENCE, and concat the results as strings.
In between each pair of results, stick in SEPARATOR.  Thus, " " as
SEPARATOR results in spaces between the values returned by FUNCTION.
SEQUENCE may be a list, a vector, a bool-vector, or a string.

(mapconcat FUNCTION SEQUENCE SEPARATOR)"

(mapconcat #'identity message "")
"shesellsseashells"

(concat
"Concatenate all the arguments and make the result a string.
The result is a string whose elements are the elements of all the arguments.
Each argument may be a string or a list or vector of characters (integers).

(concat &rest SEQUENCES)"

(apply 'concat message)
"shesellsseashells"

apply
"Call FUNCTION with our remaining args, using our last arg as list of args.
Then return the value FUNCTION returns.
Thus, (apply '+ 1 2 '(3 4)) returns 10.

(apply FUNCTION &rest ARGUMENTS)"

(defun count-letters (word-list)
  (let ((letter-counts (make-vector 26 0)))
    (loop for word in word-list
	  do (loop for char across word
		   do (incf (aref letter-counts (- char ?a)))))
    letter-counts))


(count-letters message)
[1 0 0 0 4 0 0 2 0 0 0 4 0 0 0 0 0 0 6 0 0 0 0 0 0 0]

(defun get-frequencies-as-priority-queue (letter-counts)
  (sort (loop for count across letter-counts
	      for c from ?a
	      unless (zerop count)
	      collect (cons (string c) count)
	      )
	(lambda (x y) (< (cdr x) (cdr y)))))

(get-frequencies-as-priority-queue (count-letters message))
(("a" . 1) ("h" . 2) ("e" . 4) ("l" . 4) ("s" . 6))


(setq message1 '("elba" "was" "able" "ere" "long"))
("elba" "was" "able" "ere" "long")

(get-frequencies-as-priority-queue (count-letters message1))
(("g" . 1) ("n" . 1) ("o" . 1) ("r" . 1) ("s" . 1) ("w" . 1) ("b" . 2) ("a" . 3) ("l" . 3) ("e" . 4))




























