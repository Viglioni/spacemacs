;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; org-latex-pdf related functions
;;

(require 'helm)


;;
;; common functions
;;

;;;###autoload
(defun get-org-buffer-name (src-buffer)
  (replace-regexp-in-string "\\[" "" (nth 2 (split-string src-buffer " "))))



;;
;;  inserting LaTeX enviroment in org mode
;;  and editing them in special mode
;;

;;;###autoload
(defun insert-lautex-env (env-name)
  (let ((begin (concat "\\begin{" env-name "}"))
        (end (concat "\\end{" env-name "}")))
    (insert (concat begin "\n\n" end))))

(setq lautex-env-names
      '("theorem" "definition" "remark" "example" "figure" "Table"  "description" "enumerate" "itemize" "list"  "math" "displaymath" "split" "array" "eqnarray" "equation" "equation*" "Matrix" "environments" "Cases" "align" "align*" "alignat" "environments" "center" "flushleft" "flushright" "minipage" "quotation" "quote" "verbatim" "verse" "tabbing" "tabular" "Thebibliography" "Titlepage"))

(setq lautex-srcs
      (helm-build-sync-source "Environment"
          :candidates 'lautex-env-names
          :action 'insert-lautex-env))

(setq lautex-srcs-fallback
      (helm-build-dummy-source "Environment"
        :action 'insert-lautex-env))

;;;###autoload
(defun org-lautex-env ()
  "inserts env and opens edit special"
 (interactive)
  (if (eq 'org-mode major-mode)
      (progn
        (helm :sources '(lautex-srcs lautex-srcs-fallback)) 
        (org-edit-special)
        (previous-line)
        (spacemacs/indent-region-or-buffer))))

;;;###autoload
(defun org-lautex-env-exit ()
  "exits edit special and toggle latex fragment to image"
  (interactive)
  (org-edit-src-exit)
  (org-clear-lautex-preview (point) (+ 1 (point)))
  (org-toggle-lautex-fragment))

;;;###autoload
(defun org-lautex-preview-env ()
  "preview on org buffer latex changes in special edit buffer"
  (interactive)
  (let* ((edit-buff (buffer-name))
         (original-buf (get-org-buffer-name edit-buff))
         (buffer-exists (bool (get-buffer original-buf))))
    (if buffer-exists
        (progn
          (org-edit-src-save)
          (switch-to-buffer original-buf)
          (org-clear-latex-preview (point) (+ 1 (point)))
          (org-toggle-latex-fragment)
          (switch-to-buffer edit-buff))
      (print (concat "Buffer " original-buf " does not exists!")))))


;;
;; Reference existing labels
;;

;;;###autoload
(defun get-label (line)
  (let* ((without-preffix 
          (replace-regexp-in-string ".*label\{" "" line)))
    (replace-regexp-in-string "\}.*" "" without-preffix)))

;;;###autoload
(defun insert-reference (label)
  (insert (concat "\\ref{" label "}")))

;;;###autoload
(defun get-org-file-name ()
  (or (buffer-file-name)
     (buffer-file-name (get-buffer (get-org-buffer-name (buffer-name))))))

;;;###autoload
(defun get-candidates ()
  (let* ((text  (get-string-from-file (get-org-file-name)))
         (matches (regex-matches "\\\\label\{.*\}" text)))
    (seq-map 'get-label matches)))

;;;###autoload
(defun buffer-source ()
  (helm-build-sync-source "Label Source"
    :candidates (get-candidates)
    :action '(lambda (line) (insert-reference (get-label line)))))

;;;###autoload
(defun lautex-insert-reference ()
  (interactive)
  (helm :sources (buffer-source)
        :buffer "*helm buffer source*"))




