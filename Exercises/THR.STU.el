
(cons 'a 'b)
(a . b)
(cons (cons 'a 'b) (cons 'c 'd))
((a . b) c . d)

(cons 'a (cons 'b (cons 'c (cons 'd nil))))
(a b c d)

(setq orange 612)
612

(cons 4 (cons (cons orange 'a) '("meow" 12)))
(4 (612 . a) "meow" 12)
