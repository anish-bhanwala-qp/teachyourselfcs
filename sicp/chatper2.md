# Exercise 2.1
```
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and (< n 0) (< d 0)) (cons (/ (* -1 n) g) (/ (* d -1) g)))
          ((< d 0) (cons (/ (* -1 n) g) (/ (* d -1) g)))
          (else (cons (/ n g) (/ d g))))))     
```
