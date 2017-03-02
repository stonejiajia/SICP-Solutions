(define (triangle row col)
  (cond ((> col row) 0)
        ((< col 0) 0)
        ((= col 1) 1)
        ((+ (triangle (- row 1) (- col 1))
            (triangle (- row 1) col)))))

(triangle 5 3)


