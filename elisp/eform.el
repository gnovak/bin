;; Version 2:
;; 1) more generality: function should take strings, return strings
;; 2) But don't lose efficiency... most (all?) use is on numbers, don't
;;    want to convert them back and forth to strings consantly
;;    Could do this by mapping function plus reduction function, w/ defaults.
;; 3) Unify regexps.
;; 4) More rational functions: Entities are: 
;;      data rectangles => data lists
;;      name regexps => data lists
;;      results
;; 5) pre/post functions... more important than I thought.
;; Notation: result = [(regexp)] [pre] [op] [post] name (...  )
;; [re-beg-char regexp re-end-char] [name-char] name [pre] [op] [post] 
;;   [result-beg-char] ... [result-end-char].
;; AND
;; regexp name-char [A-z0-9-] or something
;; [rect-beg-char]name ... [rect-end-char] name or something
;; "eform char" is customizeable, 
;; 6) recalc until buffer stops changing?
;; 7) use buffer local lisp variables for results of computations?
;; 8) Lispier syntax?

(require 'gsn)

;; Should be buffer local
(defvar eform-mode nil "Electric Form Mode")
(make-variable-buffer-local 'eform-mode)

(add-to-list 'minor-mode-alist '(eform-mode " EForm"))

(defun eform-mode (&optional arg)
  (interactive "P")
  (setq eform-mode 
	(if (null arg) (not eform-mode)
	  (> (prefix-numeric-value arg) 0))))

(defun eform-update ()
  "Update all electric forms"
  (interactive)
  (if eform-mode
      (progn
	(eform-update-rectangles)
	(eform-update-regexps))))

(setq *eform-open* "#(")
(setq *eform-close* "#)")
(setq *eform-result* "#")

(defun eform-update-rectangles ()
  "Update rectangle data columns"
  (mapc
   'eform-do-rectangle
   (filter 'eform-find-rectangle (eform-find-all-results))))

(defun eform-update-regexps ()
  "Update result regexp style of data."
  (mapc 'eform-do-regexp 
	(filter (lambda (x) (not (eform-find-rectangle x))) (eform-find-all-results))))
  
(defun eform-extract-regexp (regexp)
  "Collect data from results matching regexp"
   (let (result accum)
     (save-excursion
       (goto-char (point-min))       
       (while (setq result (eform-incremental-find-result regexp))
	 (setq accum (cons (nth 3 result) accum))))
     accum))
     
(defun eform-reduce-rectangle (op start end)
  "Reduce data coming from a rectangle style column"
   (number-to-string
    (reduce op
	    (filter 'identity 
		    (mapcar 'string-to-number 
			    (extract-rectangle start end))))))

(defun eform-reduce-regexp (op regexp)
  "Reduce data coming from a regexp style input."
   (number-to-string 
    (reduce op
	    (filter 'identity 
		    (mapcar 'string-to-number 
			    (eform-extract-regexp regexp))))))
  
(defun eform-find-rectangle (name)
  "Return start and end associated with region name"
  (save-excursion 
   (let (start end)
     (goto-char (point-max))
     (setq start (search-backward (concat *eform-open* name) nil t))
     (goto-char (point-min))     
     (setq end (search-forward (concat name *eform-close*) nil t))
     (if (or (null start) (null end)) nil       
       (list start end)))))

(defun eform-do-regexp (name)
  "Find, extract data for, and insert result from a regexp style input"
  (save-excursion
    (let* ((destination (eform-find-regexp-result name))
	   (op (nth 0 destination))
	   (start (nth 1 destination))
	   (length (nth 2 destination))
	   (value (nth 3 destination))
	   (regexp (nth 4 destination))
	   (result (string-to-number 
		    (funcall 'eform-reduce-regexp op regexp))))
      (delete-region start (+ start length))
      (goto-char start)      
      (insert (format (concat "%" (number-to-string length) "g") result)))))

(defun eform-do-rectangle (name)
  "Find, reduce, and insert answer from rectangle style input."
  (save-excursion
    (let* ((destination (eform-find-result name))
	   (op (nth 0 destination))
	   (start (nth 1 destination))
	   (length (nth 2 destination))
	   (result (string-to-number 
	    (apply 'eform-reduce-rectangle op (eform-find-rectangle name)))))
      (delete-region start (+ start length))
      (goto-char start)      
      (insert (format (concat "%" (number-to-string length) "g") result)))))

(defun eform-incremental-find-result (name)
  "Find the next result entry matching regexp <name>"
  (if (not (re-search-forward 
	    (concat *eform-result* name " \\([A-z+/\*-]*\\) (\\(.*\\))")
	    nil
	    t))
      '()
    (list (intern-soft (match-string 1))
	  (match-beginning 2)
	  (- (match-end 2) (match-beginning 2))			 
	  (match-string 2))))

(defun eform-find-result (name)
  "Find result field with <name>"
  (save-excursion     
   (goto-char (point-min))
   (eform-incremental-find-result name)))

(defun eform-find-regexp-result (name)
  "Find regexp result field with <name>"
  (save-excursion 
   (goto-char (point-min))
   (re-search-forward (concat  "(\\(.*\\)) " *eform-result* name 
			       " \\([A-z+/\*-]*\\) (\\(.*\\))"))
   (list 
    (intern-soft (match-string 2))
    (match-beginning 3)
    (- (match-end 3) (match-beginning 3))			 
    (match-string 3)
    (match-string 1))))

(defun eform-find-all-results ()
  "Return a list of all result fields in the document."
  (let (result)
    (save-excursion 
      (goto-char (point-min))
      (while (not (null (re-search-forward 
			 (concat *eform-result* "\\([A-z0-9]*\\) [A-z+/\*-]* (.*)") 
			 nil
			 t)))
	(setq result (cons (match-string 1) result)))
      result)))
    
(provide 'eform)




