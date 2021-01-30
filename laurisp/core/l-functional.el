;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; functional related functions
;;


;;;###autoload
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

;;;###autoload
(defun -> (arg functions)
  (compose (reverse functions) arg))

;;;###autoload
(defun apply-curry-on-list (list)
  (apply #'>> list))

;;;###autoload
(defun compose (functions args)
  (let* ((funcs (seq-map #'eval functions))
         (curried (seq-map #'apply-curry-on-list funcs)))
    (funcall (apply #'_compose curried) args)))



