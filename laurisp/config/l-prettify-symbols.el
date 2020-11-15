;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; prettify-symbols related functions
;;

;; set to run globally
(global-prettify-symbols-mode +1)

;; symbols do be used in any mode
(setq generic-symbols '(("->" . ?→)
                        ("=>" . ?⇒)
                        ("/=" . ?≠)
                        ("!==" . ?≠)
                        ("<=" . ?≤)
                        (">=" . ?≥)
                        ("&&" . ?∧)
                        ("||" . ?∨)
                        (">>" . ?≫)))

(setq js-family-symbols nil)

(defun add-symbols-to-mode (&optional symbols)
  (dolist (symbol (append generic-symbols symbols))
    (if (not-contains prettify-symbols-alist symbol)
        (push symbol prettify-symbols-alist))))

;; applying on all js/ts modes 
(add-hook 'tide-mode-hook
          (lambda () (add-symbols-to-mode js-family-symbols)))

;; applying to elisp mode
(add-hook 'emacs-lisp-mode-hook
          (lambda () (add-symbols-to-mode)))
