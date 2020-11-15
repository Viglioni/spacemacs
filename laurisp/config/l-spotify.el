;; -*- lexical-binding: t -*-
;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; spotify related functions
;;




(defun spotify-share-song ()
  (let* ((cmd-ret (shell-command-to-string "spotify share url"))
         (url (replace-regexp-in-string "Spotify URL: " "" cmd-ret)))
    (kill-new url)
    (print "Url is copied into your clipboard")))

(defun kill-spotify-buffer (buffer-name)
  (if (get-buffer buffer-name) (kill-buffer buffer-name)))

(defun get-spotify-status ()
  (shell-command-to-string "spotify status"))

(defun get-spotify-vol ()
  (shell-command-to-string "spotify vol"))

(defun spotify-volume-down ()
  (shell-command-to-string "spotify vol down"))

(defun spotify-volume-up ()
  (shell-command-to-string "spotify vol up"))


(defun insert-spotify-buffer-content (buffer-name status vol)
  (let ((inhibit-read-only t)
        (status-formatted (compose
                           '('(replace-regexp-in-string "Artist" "\nArtist")
                              '(replace-regexp-in-string "Position: .*\n" ""))
                           status)))
    (with-current-buffer
        buffer-name
      (erase-buffer)
      (insert (concat status-formatted "\n" vol)))))


(defun spotify-helper (x)
  (interactive "k(t) Play/Pause; (n) Next; (p) Previous; (/) Search; (s) Share; (u) Vol Up; (d) Vol Down; (any) Quit helper" )
  (if (cond
       ((equal x "u") (spotify-volume-up))
       ((equal x "d") (spotify-volume-down))
       ((equal x "t") (spotify-playpause))
       ((equal x "n") (spotify-next))
       ((equal x "p") (spotify-previous))
       ((equal x "/") (helm-spotify-plus))
       ((equal x "s") (spotify-share-song) nil))
      (if (get-buffer "spotify-status")
          (call-interactively 'spotify-status)
        (call-interactively 'spotify-helper))
    (kill-spotify-buffer "spotify-status")))


(defun show-spotify-status ()
  (let* ((buffer-name "spotify-status")
         (status (get-spotify-status))
         (vol (get-spotify-vol)))
    (if (not (get-buffer buffer-name)) (get-buffer-create buffer-name))
    (display-buffer-in-side-window (get-buffer buffer-name) display-buffer-alist)
    (select-window (get-buffer-window buffer-name))
    (insert-spotify-buffer-content buffer-name status vol)
    (toggle-read-only)))


(defun spotify-status ()
  (interactive)
  (show-spotify-status)
  (call-interactively 'spotify-helper))


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



