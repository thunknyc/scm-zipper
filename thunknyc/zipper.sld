;;;
;;; Zippers
;;;
;;; Edwin Watkeys
;;; Thunk NYC Corp.
;;; edw@poseur.com
;;;
;;; 2018-12-18
;;;

(define-library (thunknyc zipper)
  (export <Zipper> zipper zipper? zipper-top?
          
          zip-down zip-up zip-right zip-left zip-top
          zip-to-ref zip-into-ref
          zip-first zip-final

          zip-insert-left zip-insert-right
          zip-append zip-prepend

          zip-insert-left* zip-insert-right*
          zip-append* zip-prepend*

          zip-update zip-swap
          
          unzip zip-select)

  (cond-expand
    (chibi (import (except (scheme red) first)))
    (gerbil (import (scheme base))))
  (import (thunknyc core))

  (begin

    (define-record-type <Zipper>
      (make-zipper current left right above)
      zipper?
      (current zipper-current)
      (left zipper-left)
      (right zipper-right)
      (above zipper-above))

    (define (zipper o)
      (make-zipper o '() '() '()))

    (define (zip-down z)
      (let ((c (zipper-current z))
            (l (zipper-left z))
            (r (zipper-right z))
            (a (zipper-above z)))
        (cond ((collection? c)
               (make-zipper (first c) (empty c) (rest c) z))
              (else
               (error "Cannot zip down from here" z c)))))

    (define (zip-to-ref z i . o)
      (let ((fail (if (pair? o) (car o)
                      (lambda (z i col)
                        (error "Reference does not exist in collection"
                          i col))))
            (c (zipper-current z))
            (l (zipper-left z))
            (r (zipper-right z))
            (a (zipper-above z)))
        (let ((col (construct l `(,@l ,c ,@r))))
          (cond ((ref-in-domain? col i)
                 (let-values (((c l r) (split-at-ref col i)))
                   (make-zipper c l r a)))
                (else
                 (fail z i col))))))

    (define (zip-into-ref z i . o)
      (zip-down (apply zip-to-ref z i o)))

    (define (zip-left z . o)
      (let loop ((n (if (pair? o) (car o) 1)) (z z))
        (cond ((<= n 0) z)
              (else
               (let ((c (zipper-current z))
                     (l (zipper-left z))
                     (r (zipper-right z))
                     (a (zipper-above z)))
                 (cond ((collection? l)
                        (loop (dec n) (make-zipper (final l)
                                                   (drop-final l)
                                                   (prepend c r)
                                                   a)))
                       (else
                        (error "Cannot zip left from here" z c))))))))

    (define (zip-right z . o)
      (let loop ((n (if (pair? o) (car o) 1)) (z z))
        (cond ((<= n 0) z)
              (else
               (let ((c (zipper-current z))
                     (l (zipper-left z))
                     (r (zipper-right z))
                     (a (zipper-above z)))
                 (cond ((collection? r)
                        (loop (dec n) (make-zipper (first r)
                                                   (construct l `(,@l ,c))
                                                   (rest r)
                                                   a)))
                       (else
                        (error "Cannot zip right from here" z c))))))))

    (define (zip-first z)
      (let ((c (zipper-current z))
            (l (zipper-left z))
            (r (zipper-right z))
            (a (zipper-above z)))
        (let ((col (construct l `(,@l ,c ,@r))))
          (make-zipper (first col)
                       (construct l `())
                       (construct l (rest col))
                       a))))

    (define (zip-final z)
      (let ((c (zipper-current z))
            (l (zipper-left z))
            (r (zipper-right z))
            (a (zipper-above z)))
        (let ((col (construct l `(,@l ,c ,@r))))
          (make-zipper (final col)
                       (construct l (drop-final col))
                       (construct l '())
                       a))))
    
    (define (zipper-top? z)
      (null? (zipper-above z)))

    (define (zip-up z)
      (let ((c (zipper-current z))
            (l (zipper-left z))
            (r (zipper-right z))
            (a (zipper-above z)))
        (cond ((zipper-top? z)
               (error "Cannot zip up from here" z c))
              (else
               (let ((al (zipper-left a))
                     (ar (zipper-right a))
                     (aa (zipper-above a)))
                 (make-zipper (construct l `(,@l ,c ,@r)) al ar aa))))))

    ;; insert to left of current, keeping current
    (define (zip-insert-left z el)
      (let ((c (zipper-current z))
            (l (zipper-left z))
            (r (zipper-right z))
            (a (zipper-above z)))
        (make-zipper c (construct l `(,@l ,el)) r a)))

    ;; insert to left of current, make left current
    (define (zip-insert-left* z el)
      (zip-left (zip-insert-left z el)))

    ;; insert to right of current, keeping current
    (define (zip-insert-right z el)
      (let ((c (zipper-current z))
            (l (zipper-left z))
            (r (zipper-right z))
            (a (zipper-above z)))
        (make-zipper c l (construct r `(,el ,@r)) a)))

    ;; insert to right of current, make right current
    (define (zip-insert-right* z el)
      (zip-right (zip-insert-right z el)))

    ;; insert at end of current's structure, keeping current
    (define (zip-append z el)
      (let ((c (zipper-current z))
            (l (zipper-left z))
            (r (zipper-right z))
            (a (zipper-above z)))
        (make-zipper c l (construct r `(,@r ,el)) a)))

    ;; insert at end of current's structure, make final current
    (define (zip-append* z el)
      (zip-final (zip-append z el)))

    ;; insert at beginning of current's structure, keeping current
    (define (zip-prepend z el)
      (let ((c (zipper-current z))
            (l (zipper-left z))
            (r (zipper-right z))
            (a (zipper-above z)))
        (make-zipper c `(,el ,@l) r a)))

    ;; insert at beginning of current's structure, make first current
    (define (zip-prepend* z el)
      (zip-first (zip-prepend z el)))

    (define (zip-update z proc)
      (let ((c (zipper-current z))
            (l (zipper-left z))
            (r (zipper-right z))
            (a (zipper-above z)))
        (make-zipper (proc c) l r a)))

    (define (zip-swap z new)
      (zip-update z (lambda (ignore) new)))

    (define (zip-top z)
      (if (zipper-top? z) z
          (zip-top (zip-up z))))

    (define (unzip z)
      (zipper-current (zip-top z)))

    (define (zip-select z)
      (zipper-current z))))


;; (let ((update (lambda (el) (string-upcase (symbol->string el)))))
;;   (-> (zipper '(a (b c d e) f g))
;;       zip-down
;;       (zip-right 2)
;;       (zip-update update)
;;       zip-left
;;       zip-down
;;       (zip-to-ref -1)
;;       (zip-update update)
;;       (zip-left 3)
;;       (zip-swap 'was-b)
;;       unzip)))
