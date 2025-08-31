(define (if-program condition if-true if-false)
  `(if ,condition ,if-true ,if-false))

(define (square n) (* n n))

(define (pow-expr base exp)
  (cond ((= exp 0) 1)
        ((even? exp) `(square ,(pow-expr base (/ exp 2))))
        (else `(* ,base ,(pow-expr base (- exp 1))))))

;本质：在 Scheme 里，(lambda (params) body) 本身就是“产生一个过程值”的表达式。对它求值不会执行 body，而是返回一个可调用的过程；只有在之后你再用 (f) 调用它时，body 才被执行。

;(lambda () expr) 的意思是“一个零参数的过程，调用它时再执行 expr”。这类零参过程也叫 thunk，用来“延迟求值”。
(define-macro (repeat n expr)
  `(repeated-call ,n (lambda() ,expr)))

; Call zero-argument procedure f n times and return the final result.
(define (repeated-call n f)
  (if (= n 1)
      (f)
      (begin (f)(repeated-call (- n 1) f))))
