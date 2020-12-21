;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; slack related functions
;;



;; load and start
(load-file "~/.private/slack-config.el")

;; hooks
(add-hook 'slack-mode-hook 'emojify-mode)

;; keybinds

