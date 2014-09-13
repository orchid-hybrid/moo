(let loop ((l '()))
  (if (null? l) (loop '(a b c d e f g h i j k l m ief eiajga egjeag))
      (begin
        (loop (cdr l)))))
