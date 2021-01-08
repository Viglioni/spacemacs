;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; paredit related functions
;;

(eval-after-load "paredit"
  (lambda ()
    (progn
      (sp-use-paredit-bindings) ;; other modules
      (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
      (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode) 
      (add-hook 'ielm-mode-hook 'enable-paredit-mode)
      (add-hook 'lisp-mode-hook 'enable-paredit-mode)
      (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
      (add-hook 'scheme-mode-hook 'enable-paredit-mode)
      (add-hook 'clojure-mode-hook 'enable-paredit-mode)
      )))

