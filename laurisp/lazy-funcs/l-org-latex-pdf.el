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
(defun lautex--get-org-buffer-name (src-buffer)
  (replace-regexp-in-string "\\[" "" (nth 2 (split-string src-buffer " "))))



;;
;;  inserting LaTeX enviroment in org mode
;;  and editing them in special mode
;;

;;;###autoload
(defun lautex--insert-lautex-env (env-name)
  (let ((begin (concat "\\begin{" env-name "}"))
        (end (concat "\\end{" env-name "}")))
    (insert (concat begin "\n\n" end))))

(setq lautex--env-names
      '("theorem" "definition" "remark" "example" "figure" "Table"  "description" "enumerate" "itemize" "list"  "math" "displaymath" "split" "array" "eqnarray" "equation" "equation*" "Matrix" "environments" "Cases" "align" "align*" "alignat" "environments" "center" "flushleft" "flushright" "minipage" "quotation" "quote" "verbatim" "verse" "tabbing" "tabular" "Thebibliography" "Titlepage"))

(setq lautex--helm-env-sources
      (helm-build-sync-source "Environment"
          :candidates 'lautex--env-names
          :action 'lautex--insert-lautex-env))

(setq lautex--helm-env-sources-fallback
      (helm-build-dummy-source "Environment"
        :action 'lautex--insert-lautex-env))

;;;###autoload
(defun LauTeX-org-env ()
  "inserts LaTeX env and opens edit special"
 (interactive)
  (if (eq 'org-mode major-mode)
      (progn
        (helm :sources '(lautex--helm-env-sources lautex--helm-env-sources-fallback)) 
        (org-edit-special)
        (previous-line)
        (spacemacs/indent-region-or-buffer))))

;;;###autoload
(defun LauTeX-org-env-exit ()
  "exits edit special and toggle latex fragment to image"
  (interactive)
  (org-edit-src-exit)
  (org-clear-lautex-preview (point) (+ 1 (point)))
  (org-toggle-lautex-fragment))

;;;###autoload
(defun LauTeX-preview-org-env ()
  "preview on org buffer latex changes in special edit buffer"
  (interactive)
  (let* ((edit-buff (buffer-name))
         (original-buf (lautex--get-org-buffer-name edit-buff))
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
(defun lautex--get-label (line)
  "get string %s in .*\\label{%s}.*"
  (-> line
     ((replace-regexp-in-string ".*label\{" "")
      (replace-regexp-in-string "\}.*" "" ))))

;;;###autoload
(defun lautex--insert-reference (label)
  "insert \\ref{label} on text"
  (insert (concat "\\ref{" label "}")))

;;;###autoload
(defun lautex--get-org-file-name ()
  "If in a edit-special buffer, return the org one,
   else return the buffer it was called"
  (or (buffer-file-name)
      (-> (buffer-name)
         ((lautex--get-org-buffer-name)
          (get-buffer)
          (buffer-file-name)))))

;;;###autoload
(defun lautex--get-candidates ()
  "Get all labels from a org/latex file"
  (let* ((text  (get-string-from-file (lautex--get-org-file-name)))
         (matches (regex-matches "\\\\label\{.*\}" text)))
    (seq-map 'lautex--get-label matches)))

;;;###autoload
(defun lautex--helm-label-source ()
  (helm-build-sync-source "Label Source"
    :candidates (lautex--get-candidates)
    :action '(lambda (line) (lautex--insert-reference (lautex--get-label line)))))

;;;###autoload
(defun LauTeX-insert-reference ()
  (interactive)
  (helm :sources (lautex--helm-label-source)
        :buffer "*helm buffer source*"))


;;
;; Insert libs
;;








