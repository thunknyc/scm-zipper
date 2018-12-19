(define-library (thunknyc core)
  (export -> collection? first rest final drop-final
          empty prepend construct len
          xlate-seq-ref ref-in-domain? split-at-ref inc dec)

  (cond-expand
    (chibi (import (except (scheme red) first)))
    (gerbil (import (scheme base) (except (std srfi 1) first))))

  (begin

;;; Code to abstract away collection types. Add COND cases to support
;;; additional collections such as vectors, hash-tables, sets, bags.

    (define (collection? col)
      (and (list? col) (not (null? col))))

    (define (first col)
      (cond ((list? col) (car col))))

    (define (rest col)
      (cond ((list? col) (cdr col))))

    (define (final col)
      (cond ((list? col) (last col))))

    (define (drop-final col)
      (cond ((list? col) (drop-right col 1))))

    (define (empty col)
      (cond ((list? col) '())))

    (define (prepend a col)
      (cond ((list? col) (cons a col))))

    (define (construct col els)
      (cond ((list? col) els)))

    (define (len col)
      (cond ((list? col) (length col))))

    (define (xlate-seq-ref col i)
      (if (>= i 0) i
          (+ (len col) i)))

    (define (ref-in-domain? col i)
      (cond ((list? col)
             (let ((i (xlate-seq-ref col i)))
               (and (>= i 0) (< i (length col)))))))

    (define (split-at-ref col i)
      (cond ((list? col)
             (let*-values (((i) (xlate-seq-ref col i))
                           ((a b) (split-at col i)))
               (values (car b) a (cdr b))))))

    (define (inc n) (+ n 1))
    (define (dec n) (- n 1))

    (define-syntax ->
      (syntax-rules ()
        ((_ val (proc arg ...) form ...)
         (-> (proc val arg ...) form ...))
        ((_ val proc form ...)
         (-> (proc val) form ...))
        ((_ val)
         val)))))
