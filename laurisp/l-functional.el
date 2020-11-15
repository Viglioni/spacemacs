;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; functional related functions
;;



(defsubst _compose (function &rest more-functions)
  (cl-reduce (lambda (f g)
               (lexical-let ((f f) (g g))
                 (lambda (&rest arguments)
                   (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))

(defsubst >> (function &rest arguments)
  "Curry a function"
  (lexical-let ((function function)
                (arguments arguments))
    (lambda (&rest more) (apply function (append arguments more)))))

(defun -> (arg functions)
  (compose (reverse functions) arg))

(defun apply-curry-on-list (list)
  (apply #'>> list))

(defun compose (functions args)
  (let* ((funcs (seq-map #'eval functions))
         (curried (seq-map #'apply-curry-on-list funcs)))
    (funcall (apply #'_compose curried) args)))

(defun identity (x) x)

;; (compose '('(#'+ 1)
;;            '(#'* 10))
;;          10) ;; => 101

;; (-> 10
;;    '('(* 10)
;;      '(+ 1))) ;; => 101





(defmacro list-func (function-list)
  (destructuring-bind (fn &rest args) function-list
    `(append (list (quote ,fn)) (quote ,args))
    ))

;; (list-func (* 1 2))

(defmacro comp (binding )
  (destructuring-bind ( f  &rest fs) binding

    ;(seq-map #'list-func fs)
    ))

;; (comp ((* 1 2 3)
;;        (+ 1))
;;       )

;; (append (list '*)  '(1 2))


;; (comp ( (* 10) (+ 1)) 1) 

