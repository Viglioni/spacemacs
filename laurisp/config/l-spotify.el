;; -*- lexical-binding: t -*-
;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; spotify related functions
;;



;; set alias keybindings
(which-key-add-key-based-replacements "M-p" "Spotify")
(global-set-key (kbd "M-p M-p") 'spotify-helper)
(global-set-key (kbd "M-p p") 'spotify-status )


;; set default spacemacs keybidings 
(global-set-key (kbd "M-m a m s t") 'spotify-playpause)
(global-set-key (kbd "M-m a m s n") 'spotify-next)
(global-set-key (kbd "M-m a m s p") 'spotify-previous)
(global-set-key (kbd "M-m a m s s") 'helm-spotify-plus) ; search

;; unset default keys
(global-unset-key (kbd "M-m a m s g"))
(global-unset-key (kbd "M-m a m s N"))



