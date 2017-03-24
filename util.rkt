#lang racket
(require racket/sequence)
(require plot)

(define (data-as-rows data keys)
  (let ([n-keys (length keys)])
    (for/list ([k (in-list keys)]
               [d (in-slice n-keys data)])
      (cons k d))))
(define (data-as-cols data keys)
  (let ([n-keys (length keys)])
    (map cons
         keys
         (apply (curry map list)
                (sequence->list (in-slice n-keys data))))))
(define (data-as-vals data keys)
  (map cons keys data))

(provide read-data-file)
(define (read-data-file path)
  (let ([tables
         (let ([in (open-input-file path)])
           (begin0
               (read in)
             (close-input-port in)))])
    (append*
     (for/list ([table (in-list tables)])
       (match-let ([(list* layout keys data) table])
         (case layout
           [(rows) (data-as-rows data keys)]
           [(cols) (data-as-cols data keys)]
           [(vals) (data-as-vals data keys)]
           [else
            (error (format "invalid table layout: ~v" layout))]
           ))))))


(provide current-dataset)
(define current-dataset (make-parameter '()))

(provide ds)
(define (ds x)
  (cdr (or (assoc x (current-dataset))
           (error (format "no key in dataset: ~v\n" x)))))

(provide j)
(define (j . args)
  (apply (curry map list) args))

(provide ds-xy)
(define (ds-xy x y)
  (j (ds x) (ds y)))





(provide mean)
(define (mean l)
  (/ (foldl + 0.0 l)
     (length l)))

(provide std-dev)
(define (std-dev l [m (mean l)])
  (sqrt (/ (foldl (lambda (x a)
                    (+ a (sqr (- x m))))
                  0.0 l)
           (length l))))

(provide correlation)
(define (correlation xs ys
                     [mx (mean xs)]
                     [my (mean ys)]
                     [sx (std-dev xs mx)]
                     [sy (std-dev ys my)])
  (let ([n (length xs)])
    (/ (- (foldl (lambda (x y a)
                   (+ a (* x y)))
                 0.0 xs ys)
          (* n mx my))
       (* n sx sy))))

(provide reg-str-precision)
(define reg-str-precision
  (make-parameter 3))


(provide lin-reg)
;;; (lin-reg xs ys) => a b f l
;;; where
;;;   f(x) = a + b x
(define (lin-reg xs ys
                 #:y-var [y-var "y"]
                 #:x-var [x-var "x"]
                 #:prec [prec (reg-str-precision)])
  (let* ([mx (mean xs)]
         [my (mean ys)]
         [sx (std-dev xs mx)]
         [sy (std-dev ys my)]
         [r (correlation xs ys mx my sx sy)]
         [b (* r (/ sy sx))]
         [a (- my (* b mx))])
    (values a b
            (lambda (x)
              (+ a (* b x)))
            (format "~a = ~a*~a + ~a"
                    y-var
                    (~r b #:precision prec)
                    x-var
                    (~r a #:precision prec)))))


(provide exp-reg)
(define (exp-reg xs ys
                 #:y-var [y-var "y"]
                 #:x-var [x-var "x"]
                 #:prec [prec (reg-str-precision)])
  (let-values ([(a b f s) (lin-reg xs (map log ys))])
    (let ([a (exp a)])
      (values a b
              (lambda (x)
                (* a (exp (* b x))))
              (format "~a = ~a e^(~a*~a)"
                      y-var
                      (~r a #:precision prec)
                      (~r b #:precision prec)
                      x-var)))))
