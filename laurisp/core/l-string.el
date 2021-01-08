;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; string related functions
;;

;;;###autoload
(defun join-path (path file)
  "concat path and file. Adds '/' to the end of the path if necessary"
  (concat path (if (string-match-p "/$" path) "" "/") file))

;;;###autoload
(defun file-extension (filename extension)
  "returns filename.extension"
  (concat filename "." extension))
