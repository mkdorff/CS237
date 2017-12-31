
(require 'cl) ; for loop macro

(defun for-all (predicate domain)
  (loop for item across domain
        always (funcall predicate item)))

(defun for-some (predicate domain)
  (loop for item across domain
        thereis (funcall predicate item)))

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

(defun greater-than (x y)
  "A function of arity 2 that wraps the one already bound to '>."
  (> x y))

(defun is-x-multiple-of-y (x y)
  "This function checks if the first parameter is a multiple of the second"
  (if (eq y 0)
      nil
    (let* ((z (mod x y)))
       (if (eq z 0)
           t
         nil))))

; Specific colors that we would see in certain terrains
(defvar terrain-colors
  '((forest brown green orange tan yellow)
    (marshlands blue brown green red tan teal)
    (desert black brown green tan)
    (mountains brown green orange tan white)))

; Function needed to compare an element in any two terrains
(defun compare-colors (terrain color)
  (not (null (member color (cdr (assoc terrain terrain-colors))))))

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
