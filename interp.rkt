#lang eopl

;; interpreter for the PROC language, using the procedural
;; representation of procedures.



(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; run : String -> ExpVal
(define run
  (lambda (s)
    (value-of-program (scan&parse s))))

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      
      (const-exp (num) (num-val num))
      
      
      (var-exp (var) (apply-env env var))
      
     
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      
      
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      
     
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      
      
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      
      (proc-exp (var body)
                (proc-val (procedure var body env)))
      
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator env)))
                      (arg (value-of rand env)))
                  (apply-procedure proc arg)))
      
      ;;----------------------------------------------------
      ; INSERT YOUR CODE HERE
      ; Write the required expressions starting from here

      ;;-------------------------------------------------

      (stack-exp ()
                 (stack-val '()))

      (stack-push-exp (stack exp)
                      (let ((val1 (expval->stack (value-of stack env)))
                            (val2 (expval->num (value-of exp env))))
                      (stack-val (append (list val2) val1))))

      (stack-pop-exp (stack)
                     (let ((val1 (expval->stack (value-of stack env))))
                       (if (null? val1)
                         (begin
                           (display "Warning: Stack is empty. Cannot pop element.\n")
                           (stack-val '()))
                         (stack-val (cdr val1)))))

      (stack-peek-exp (exp1)
                (let ((stack (expval->stack (value-of exp1 env))))
                  (if (eq? stack '()) (begin (display "Warning: Stack is empty. Cannot peek.\n") (num-val 2813))
                  (num-val (car stack)))))

      (stack-push-multi-exp (stack exps)
                (stack-val (push-multi-helper (expval->stack (value-of stack env)) exps env)))

      (stack-merge-exp (stack1 stack2)
                       (let ((s1 (expval->stack (value-of stack1 env)))
                             (s2 (expval->stack (value-of stack2 env))))
                         (stack-val (append (reverse-list s2) s1))))
                         

      ))) 
  


;;-----------------------------------------
; INSERT YOUR CODE HERE
; you may use this area to define helper functions
;;-----------------------------------------

(define push-multi-helper (lambda (stack exps env)
                            (if (null? exps)
                                stack
                                (push-multi-helper
                                 (append (list (expval->num (value-of (car exps) env))) stack)
                                 (cdr exps)
                                 env))))

; stack-merge helper
(define reverse-list (lambda (lst) (if (eq? lst '())
                                       '()
                                       (append (reverse-list (cdr lst)) (list (car lst))))))

;;-----------------------------------------

(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
                 (value-of body (extend-env var val saved-env))))))
