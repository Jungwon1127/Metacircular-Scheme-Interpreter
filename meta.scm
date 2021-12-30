;; metacircular interpreter for a subset of scheme

;; An environment is a list of one element.  That element is an association
;; list.  This allows us to add a binding to an environment.

(define popl-error
   (lambda strings
      (for-each display strings)
      (newline)
      (error "run time error")))


(define *TOP-ENVIRONMENT*
  (list (list))) ;; totally empty environment

(define (popl-bind symbol value env)
  (let ((bindings (car env)))
    (set-car! env (cons (list symbol value) bindings)))
  symbol)

(define (popl-get-binding symbol env)
  (assoc symbol (car env)))


;; return the value of a symbol bound in the given environment
(define (popl-env-value symbol env)
  (let ((pr (popl-get-binding symbol env)))
    (if pr
        (cadr pr)
        (popl-error "Symbol  " symbol " not found"))))


(define (popl-set! symbol value env)
  (let ((pr (popl-get-binding symbol env)))
    (if pr
        (set-car! (cdr pr) value)
        (popl-error "No binding found for " symbol))))

(define (let*-to-let expr)
   (let ((bindings (second expr))
         (body (cddr expr)))
      (if (< (length bindings) 2)
         (cons 'let (cons bindings body))
         (cons 'let (list (list (car bindings))
           (let*-to-let
             (cons 'let* (cons (cdr bindings) body))))))))

(define (let*-to-let expr)
  (let ((bindings (second expr))
	(body (cddr expr)))
    (if (< (length bindings) 2)
	`(let ,bindings ,@body)
	`(let (,(car bindings))
	   ,(let*-to-let (let* ,(cdr bindings) ,@body))))))  ;;; Campbell's homework

;;; (let ((a 1) (b x)) ...)
;;; ((lambda (a b) ...) 1 x)


(define (unzip L)
  (fold-right
   (lambda (item result)
     (list (cons (first item) (first result))
	   (cons (second item) (second result))))
   '(() ())
   L))

(define (let-to-lambda expr)
  (let* ((bindings (second expr))
	 (rest (cddr expr))
	 (vv (unzip bindings))
	 (vars (first vv))
	 (vals (second vv)))
    `((lambda ,vars ,@rest) ,@vals)))
	     
(popl-bind '+ + *TOP-ENVIRONMENT*)
(popl-bind '- - *TOP-ENVIRONMENT*)
(popl-bind '* * *TOP-ENVIRONMENT*)
(popl-bind '/ / *TOP-ENVIRONMENT*)
(popl-bind '= = *TOP-ENVIRONMENT*)
(popl-bind 'cons cons *TOP-ENVIRONMENT*)


(define println
   (lambda stuff
         (for-each display stuff)
         (newline)))

(define (popl-apply function arguments)
   (let* ((original-env (third function))
          (params (first function))
          (body (second function))
          (env (list (car original-env)))
          (return-value #!unspecific))
      (for-each (lambda (pair) (popl-bind (car pair) (cadr pair) env))
        (zip params arguments))
      (for-each (lambda (expr)
                   (set! return-value (popl-eval expr env)))
                 body)
      return-value))


(define (popl-eval expr env)
  (println "Evaluating " expr)
  (cond ((or (number? expr) (boolean? expr)) expr)
        ((symbol? expr) (popl-env-value expr env))
        ((pair? expr)
         (cond ((eq? (car expr) 'quote) (cadr expr))
               ((eq? (car expr) 'if)
                (if (popl-eval (second expr) env)
                    (popl-eval (third expr) env)
                    (popl-eval (fourth expr) env)))
               ((eq? (car expr) 'define)
                (popl-bind (second expr) (popl-eval (third expr) env) env))
               ((eq? (car expr) 'lambda)
                (list (second expr) ;; parameters
                      (cddr expr) ;; body
                      env)) ;; environment
               (else ;;assume function call
                  (let* ((values (map (lambda (exp) (popl-eval exp env))
                                      expr))
                         (function (car values)))
                     (if (procedure? function) (apply function (cdr values))
                         (popl-apply function (cdr values)))))))))

(define (popl-repl)
  (display "H]=> ")
  (let* ((expr (read)))
    (cond ((equal? expr '(exit)) (display "Bye then!"))
          (else (display (popl-eval expr *TOP-ENVIRONMENT*)) (newline)
                (popl-repl)))))
