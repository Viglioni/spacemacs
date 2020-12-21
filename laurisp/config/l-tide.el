;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; tide related functions
;;

(defun tide-setup-hook ()
    (tide-setup)
    (eldoc-mode)
    (tide-hl-identifier-mode +1)
    (setq web-mode-enable-auto-quoting nil)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-attr-value-indent-offset 2)
    (setq lsp-eslint-server-command '("node" (concat (getenv "HOME") "/var/src/vscode-eslint/server/out/eslintServer.js") "--stdio")))

;; New prefixes for commands
(spacemacs/declare-prefix-for-mode 'typescript-mode "mf" "format")
(spacemacs/declare-prefix-for-mode 'rjsx-mode "mf" "format")
(spacemacs/declare-prefix-for-mode 'web-mode "mf" "format")

(spacemacs/declare-prefix-for-mode 'typescript-mode "me" "errors")
(spacemacs/declare-prefix-for-mode 'rjsx-mode "me" "errors")
(spacemacs/declare-prefix-for-mode 'web-mode "me" "errors")

;; Keybinds inside tide-mode
(add-hook
 'tide-mode-hook
 (lambda ()
   ;; M-m m f // format
   (local-set-key (kbd "M-m m f f") 'tide-fix)
   (local-set-key (kbd "M-m m f =") 'tide-format)
   (local-set-key (kbd "M-m m f o") 'tide-organize-imports)
   (local-set-key (kbd "M-m m f r") 'tide-refac)
   ;; M-m m r // rename
   (local-set-key (kbd "M-m m r f") 'tide-rename-file)
   ;; M-m m e // errors
   (local-set-key (kbd "M-m m e p") 'tide-error-at-point)
   ;; Repl
   (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
   (local-set-key (kbd "C-c C-b") 'ts-send-buffer)
   ))

;; hooks
(add-hook 'before-save-hook 'tide-format-before-save)


;; use rjsx-mode for .js* files except json and use tide with rjsx
(add-to-list 'auto-mode-alist '("\\.js.*$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'rjsx-mode-hook 'tide-setup-hook)


;; web-mode extra config
(add-hook 'web-mode-hook 'tide-setup-hook
          (lambda () (pcase (file-name-extension buffer-file-name)
                  ("tsx" ('tide-setup-hook))
                  (_ (my-web-mode-hook)))))
(flycheck-add-mode 'typescript-tslint 'web-mode)
(add-hook 'web-mode-hook 'company-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook #'turn-on-smartparens-mode t)




