






(defun f (n)
  "f(n) = f(n-1) + 2f(n-2) + 3f(n-3)"
  (cond ((< n 3) n)
        (else (+ f (-n 1))
              (* 2 (f (- n 2)))
              (* 3 (f (- n 3))))))


(defun f-recr (n)
  "By means of a recursive process"
  (if (< n 3)
      n
    (+ (f-recr (- n 1)) (* 2 (f-recr (- n 2))) (* 3 (f-recr (- n 3))))))

(f-recr 9)


