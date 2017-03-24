#lang racket
(require (for-syntax syntax/parse))

(provide error-prop)
(define-syntax error-prop
  (syntax-parser
    #:literals (+ - * / expt log sqr)
    ; d(a +- b) = sqrt(a^2 + b^2)
    [(_ (+ a b))
     #'(match* ((error-prop a) (error-prop b))
         [((list av da) (list bv db))
          (list (+ av bv)
                (sqrt (+ (sqr da)
                         (sqr db))))])]
    [(_ (- a b))
     #'(match* ((error-prop a) (error-prop b))
         [((list av da) (list bv db))
          (list (- av bv)
                (sqrt (+ (sqr da)
                         (sqr db))))])]

    ; d(a?b) = (a?b) * sqrt((da/a)^2 + (db/b)^2)
    [(_ (* a b))
     #'(match* ((error-prop a) (error-prop b))
         [((list av da) (list bv db))
          (list (* av bv)
                (* (sqrt (+ (sqr (/ da av))
                            (sqr (/ db bv))))
                   av bv))])]
    [(_ (/ a b))
     #'(match* ((error-prop a) (error-prop b))
         [((list av da) (list bv db))
          (list (/ av bv)
                (* (sqrt (+ (sqr (/ da av))
                            (sqr (/ db bv))))
                   (/ av bv)))])]

    ; d(x^n) = x^n * |n| * dx/x
    [(_ (expt a n))
     #'(let ([nv n])
         (match (error-prop a)
           [(list av da)
            (list (expt av nv)
                  (* (expt av (sub1 nv))
                     (abs nv)
                     da))]))]

    ; d(ln(x)) = dx/x
    [(_ (log a))
     #'(match (error-prop a)
         [(list av da)
          (list (log av)
                (/ da av))])]

    [(_ (- a b c ...))
     #'(error-prop (- a (+ b c ...)))]
    [(_ (+ a b c ...))
     #'(error-prop (+ a (+ b c ...)))]
    [(_ (* a b c ...))
     #'(error-prop (* a (* b c ...)))]
    [(_ (/ a b c ...))
     #'(error-prop (/ a (* b c ...)))]
    [(_ (sqr a))
     #'(error-prop (* a a))]

    [(_ x:id) #'x]
    [(_ n:number) #'(list n 0)]))
