;;
;; @author Laura Viglioni
;; 2020
;; GNU Public License 3.0
;;

;;
;; flutter related functions
;;

(defun flutter-comment (comment-text)
  "insert a comment block"
  (interactive "sEnter the comment: ")
  (insert-text-on-point (format "/*\n * %s\n */\n\n" comment-text)))

(defun flutter-import ()
  "add default import to flutter file"
  (interactive)
  (insert "import 'package:flutter/material.dart';\n" (beginning-of-buffer)))


;;
;; Templates
;;

(defun template-dart-scaffold (component-name title)
  "returns a scaffold component"
  (format "Scaffold %s(context) => Scaffold(
  appBar: AppBar(title: Text(\"%s\")),
  body: Center( child:
    Column (
      children: [])));" component-name title))


;;
;; interactive templates calls
;;

(defun flutter-scaffold (component-name title)
  "inserts a scaffold"
  (interactive "sEnter component name: \nsEnter title: ")
  (insert (template-dart-scaffold component-name title)))
