;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; neotree related functions
;;

(defun get-ttfs (dir)
  "get .ttf files from dir"
  (directory-files dir nil "\\.ttf$"))

(defun activate-neotree-icons ()
  "copy ttf files from ~/spacemacs/Library/Fonts to user/Library/Fonts. MacOS only."
  (if (eq system-type 'darwin)
      (let* ((spacemacs-fonts-dir "~/Library/Fonts/")
             (system-fonts-dir "~/../Library/Fonts/")
             (spacemacs-ttfs (get-ttfs spacemacs-fonts-dir))
             (system-ttfs (get-ttfs system-fonts-dir))
             (ttfs-to-be-copied (list-difference spacemacs-ttfs system-ttfs)))
        (dolist (ttf ttfs-to-be-copied)
          (copy-file (concat spacemacs-fonts-dir ttf)
                     (concat system-fonts-dir ttf))))))



(setq all-the-icons-icon-alist icons-definitions)



(setq neo-smart-open t)
(global-set-key (kbd "M-s M-s s") 'neotree-toggle) 
(activate-neotree-icons)
(setq neo-theme 'icons)
(setq projectile-switch-project-action 'neotree-projectile-action)
(setq neo-window-fixed-size nil)
