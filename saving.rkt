#lang racket
(require plot "util.rkt")
(provide (all-defined-out))

(define plots (make-parameter '()))
(define-syntax-rule (plot+ name args ...)
  (plots
   (append (plots)
           (list (cons name (plot args ...))))))

(define saved '())
(define no-save? #f)
(define (no-save) (set! no-save? (not no-save?)))
(define (save-plots dir)
  (unless (directory-exists? dir)
    (make-directory dir))
  (unless no-save?
    (for ([p (in-list (plots))])
      (let ([bm (send (cdr p) get-bitmap)]
            [to (format "~a/~a.png" dir (car p))])
        (send bm save-file to 'png)
        (printf "; saved => ~s\n" to))))
  (set! saved (append saved (map cdr (plots)))))

(define-syntax-rule (with-data x body ...)
  (parameterize ([current-dataset (read-data-file (format "data/~a.lisp" x))]
                 [plots '()])
    body ...
    (save-plots (format "images/~a" x))))
