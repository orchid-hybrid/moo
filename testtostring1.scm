(display (foldl (lambda (x y) (display x) (display y) (display 'eee) (string-append x y)) "(" (list "A" "B" "C")))

