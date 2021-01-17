;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; org, latex or pdf related functions
;;


;;
;; ORG
;;

(eval-after-load "ox-latex"
  (lambda ()
    (add-to-list
     'org-latex-classes
     '("ic-tese-v3" "\\documentclass{ic-tese-v3}"
       ("\\chapter{%s}" . "\\chapter*{%s}")
       ("\\section{%s}" . "\\section*{%s}")
       ("\\subsection{%s}" . "\\subsection*{%s}")
       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

(eval-after-load "org"
  (lambda ()
    (progn
      ;; disable line count in presentation mode :)
      (add-hook 'epresent-mode-hook (lambda () (linum-mode 0)))
      ;; show formula not preview when cursor is over
      ;; automatically preview a written formula
      (add-hook 'org-mode-hook 'org-fragtog-mode)
      ;; highlight latex
      (setq org-highlight-latex-and-related '(latex script entities))
      ;; set custom preview options for latex
      (plist-put org-format-latex-options :scale 1.3)
      (plist-put org-format-latex-options :foreground "White")
      )))

(add-hook
 'org-mode-hook
 (lambda ()
   ;; key-bindings
   (local-set-key (kbd "C-<up>") 'org-move-subtree-up)
   (local-set-key (kbd "C-<down>") 'org-move-subtree-down)
   (local-set-key (kbd "C-<left>") 'org-promote-subtree)
   (local-set-key (kbd "C-<right>") 'org-demote-subtree)
   ;; force line breaks
   (spacemacs/toggle-visual-line-navigation-on)
   (setq-local word-wrap nil)
))



;;
;; PDF
;;

(add-hook 'pdf-view-mode-hook (lambda () (linum-mode 0)))



;;
;; LaTeX
;;

(eval-after-load "tex-fold"
 (lambda ()
   (progn
     ;; fold buffer on opening
     (add-hook 'find-file-hook 'TeX-fold-buffer t)
     ;; fold automatically
     (setq TeX-fold-auto t)
      ;; fold these envs:
      (add-to-list 'TeX-fold-env-spec-list '("[definition]" ("definition")))
      (add-to-list 'TeX-fold-env-spec-list '("[lemma]" ("lemma")))
      (add-to-list 'TeX-fold-env-spec-list '("[theorem]" ("theorem")))
      (add-to-list 'TeX-fold-env-spec-list '("[example]" ("example")))
      )))

