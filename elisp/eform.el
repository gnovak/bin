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
;; 9) Thin rectangles: #(gsn 1 2 3 gsn#) gives  #gsn + (   6 )
;;    Or, don't need elaborate open + close for a single line
;; 10) Can do 9 w/ pre, map, reduce, post functions.
;;     heirarchy should be: nothing; pre=id map=string-to-number reduce=+ post=number-to-string
;;                           one -- pre=id map=string-to-number reduce=one post=number-to-string
;;                           one two -- pre=id           map=one     reduce=two   post=number-to-string
;;                           one two three -- pre=id  map=one reduce=two post=three
;;                           one two three four -- pre=one map=two red=three post=four

(require 'gsn)

(defvar eform-mode nil "Electric Form Mode")
(make-variable-buffer-local 'eform-mode)
(add-to-list 'minor-mode-alist '(eform-mode " EForm"))

(setq *eform-beg* "(")
(setq *eform-end* ")")
(setq *eform-tag* "#")
(setq *eform-error* "#")
(setq *eform-char* "[A-z0-9+/\*-]")
(setq *eform-ws* "[ \t]")
(setq *eform-result-regexp-format* (concat 
			     "\\(?:" *eform-beg* "\\(.*\\)" *eform-end* "\\)?" *eform-ws* "*"
			     ; *eform-tag* "\\(" *eform-char* "+\\)" *eform-ws* "*" 
			     *eform-tag* "\\(%s\\)" *eform-ws* "*"
			     "\\(" *eform-char* "*\\)" *eform-ws* "*"
			     "\\(" *eform-char* "*\\)" *eform-ws* "*"
			     "\\(" *eform-char* "*\\)" *eform-ws* "*"
			     "\\(" *eform-char* "*\\)" *eform-ws* "*"
			     *eform-beg* "\\(.*\\)" *eform-end*))

;; Should the whitespace be allowed?  Causes problems for whitespace splits
(setq *eform-beg-rectangle-format* (concat *eform-beg* 
					   *eform-ws* "*"
					   *eform-tag* "%s"))

(setq *eform-end-rectangle-format* (concat *eform-tag* "%s" 
					   *eform-ws* "*"
					   *eform-end*))

(defun split (lst)
  ;; Assumes that list has only one element
  (split-string (car lst)))

(defun fhm (str)
  ;; from hour minute to number of minutes
  (let ((split (split-string str ":")))
    (if (= (length split) 1)
	;; only minutes
	(string-to-number str)
      (+ (* 60 (string-to-number (car split))) (string-to-number (cadr split))))))

(defun thm (total)
;; from number of minutes to hour minute
  (let ((hour (/ total 60))
	(min (mod total 60)))
    (format "%d:%2d" hour min)))

(defun eform-mode (&optional arg)
  (interactive "P")
  (setq eform-mode 
	(if (null arg) (not eform-mode)
	  (> (prefix-numeric-value arg) 0))))

(defun eform-extract-regexp (regexp)
  (mapcar (lambda (x) (nth 6 x)) (eform-find-all-results regexp)))

(defun eform-extract-rectangle (name)
  "Extract data from recangle
name is a string naming the rectangle
returns a list of strings"
  (save-excursion
    (let (start end)
      (goto-char (point-max))
      (setq start (re-search-backward (format *eform-beg-rectangle-format* name) nil t))
      (goto-char (point-min))
      (setq end   (re-search-forward  (format *eform-end-rectangle-format* name) nil t))
      (extract-rectangle start end))))

(defun eform-incremental-find-result (&optional name)
  "Find the next result corresponding to regexp <name>
Return list containing:
0 regexp contributing to result (string)
1 name of result (string)
2 preprocessing function list of strings -> obj-1
3 mapping function         obj-1 -> obj-2
4 operator for reduction   obj-2, obj-2 -> obj-2
5 postprocessing function obj-2 -> string 
6 current result (string)
7 result start (int)
8 result end (int)"
  (if (not (re-search-forward 
	    (format *eform-result-regexp-format*
		    (if (not (null name)) name
		      (concat *eform-char* "+"))) nil t))
      ;; if no match, return nil
      nil
    ;; otherwise, do a bunch of shit
    (let (pre map op post)    
      (cond ((string= (match-string 3) "") 
	     ;; Specified zero functions
	     (progn (setq pre  (symbol-function 'identity))
		    (setq map  (symbol-function 'string-to-number))
		    (setq op   (symbol-function '+))
		    (setq post (symbol-function 'number-to-string))))
	    ((string= (match-string 4) "") 
	     ;; Specified one function
	     (progn (setq pre  (symbol-function 'identity))
		    (setq map  (symbol-function 'string-to-number))
		    (setq op   (symbol-function (intern-soft (match-string 3))))
		    (setq post (symbol-function 'number-to-string))))
	    ((string= (match-string 5) "") 
	     ;; Specified two functions
	     (progn (setq pre  (symbol-function 'identity))
		    (setq map  (symbol-function (intern-soft (match-string 3))))
		    (setq op   (symbol-function (intern-soft (match-string 4))))
		    (setq post (symbol-function 'number-to-string))))
	    ((string= (match-string 6) "") 
	     ;; Specified three functions
	     (progn (setq pre  (symbol-function 'identity))
		    (setq map  (symbol-function (intern-soft (match-string 3))))
		    (setq op   (symbol-function (intern-soft (match-string 4))))
		    (setq post (symbol-function (intern-soft (match-string 5))))))
	    ;; Specified four functions
	    (t (progn (setq pre  (symbol-function (intern-soft (match-string 3))))
		      (setq map  (symbol-function (intern-soft (match-string 4))))
		      (setq op   (symbol-function (intern-soft (match-string 5))))
		      (setq post (symbol-function (intern-soft (match-string 6)))))))        
      (list 
       (match-string 1)                     ; 0 regexp   
       (match-string 2)                     ; 1 name     
       pre                                  ; 2 pre	     
       map                                  ; 3 map	     
       op                                   ; 4 operator 
       post                                 ; 5 post     
       (match-string 7)                     ; result     
       (match-beginning 7)                  ; result start
       (match-end 7)))))		   	; result end 
  
(defun eform-find-result (&optional name)
  "Find first result field with name matching regexp <name>"
  (save-excursion     
   (goto-char (point-min))
   (eform-incremental-find-result name)))

(defun eform-find-all-results (&optional name)
  "Return a list of all result fields in the document."
  (let (this-result results)
    (save-excursion
      (goto-char (point-min))
      (setq this-result (eform-incremental-find-result name))
      (while (not (null this-result))
	(setq results (cons this-result results))
	(setq this-result (eform-incremental-find-result name))))
    (reverse results)))

(defun eform-find-all-names ()
  (mapcar (lambda (x) (nth 1 x)) (eform-find-all-results regexp)))

(defun eform-reduce (data pre map op post)
  "Reduce data"
  (funcall post (reduce op (mapcar map (funcall pre data)))))

(defun eform-update-result (name)
  (let* ((dest (eform-find-result name))
	 (regexp (nth 0 dest))
	 (pre (nth 2 dest))
	 (map (nth 3 dest))
	 (op (nth 4 dest))
	 (post (nth 5 dest))
	 (start (nth 7 dest))
	 (end (nth 8 dest))
	 (data (if (not (null regexp))
		   (eform-extract-regexp regexp)
		 (eform-extract-rectangle name))))
    (save-excursion      
      (delete-region start end)
      (goto-char start)
      (insert (eform-reduce data pre map op post)))))

(defun eform-update ()
  "Update all electric forms"
  (interactive)
  (dolist result (eform-find-all-names)
	  (message "Doing something")
	  (eform-update-result result)))

(provide 'eform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Old definitions.

;; (setq *eform-open* "#(")
;; (setq *eform-close* "#)")
;; (setq *eform-result* "#")

;; (defun eform-update-rectangles ()
;;   "Update rectangle data columns"
;;   (mapc
;;    'eform-do-rectangle
;;    (filter 'eform-find-rectangle (eform-find-all-results))))

;; (defun eform-update-regexps ()
;;   "Update result regexp style of data."
;;   (mapc 'eform-do-regexp 
;; 	(filter (lambda (x) (not (eform-find-rectangle x))) (eform-find-all-results))))
  
;; (defun eform-extract-regexp (regexp)
;;   "Collect data from results matching regexp"
;;    (let (result accum)
;;      (save-excursion
;;        (goto-char (point-min))       
;;        (while (setq result (eform-incremental-find-result regexp))
;; 	 (setq accum (cons (nth 3 result) accum))))
;;      accum))
     
;; (defun eform-reduce-rectangle (op start end)
;;   "Reduce data coming from a rectangle style column"
;;    (number-to-string
;;     (reduce op
;; 	    (filter 'identity 
;; 		    (mapcar 'string-to-number 
;; 			    (extract-rectangle start end))))))

;; (defun eform-reduce-regexp (op regexp)
;;   "Reduce data coming from a regexp style input."
;;    (number-to-string 
;;     (reduce op
;; 	    (filter 'identity 
;; 		    (mapcar 'string-to-number 
;; 			    (eform-extract-regexp regexp))))))
  
;; (defun eform-find-rectangle (name)
;;   "Return start and end associated with region name"
;;   (save-excursion 
;;    (let (start end)
;;      (goto-char (point-max))
;;      (setq start (search-backward (concat *eform-open* name) nil t))
;;      (goto-char (point-min))     
;;      (setq end (search-forward (concat name *eform-close*) nil t))
;;      (if (or (null start) (null end)) nil       
;;        (list start end)))))

;; (defun eform-do-regexp (name)
;;   "Find, extract data for, and insert result from a regexp style input"
;;   (save-excursion
;;     (let* ((destination (eform-find-regexp-result name))
;; 	   (op (nth 0 destination))
;; 	   (start (nth 1 destination))
;; 	   (length (nth 2 destination))
;; 	   (value (nth 3 destination))
;; 	   (regexp (nth 4 destination))
;; 	   (result (string-to-number 
;; 		    (funcall 'eform-reduce-regexp op regexp))))
;;       (delete-region start (+ start length))
;;       (goto-char start)      
;;       (insert (format (concat "%" (number-to-string length) "g") result)))))

;; (defun eform-do-rectangle (name)
;;   "Find, reduce, and insert answer from rectangle style input."
;;   (save-excursion
;;     (let* ((destination (eform-find-result name))
;; 	   (op (nth 0 destination))
;; 	   (start (nth 1 destination))
;; 	   (length (nth 2 destination))
;; 	   (result (string-to-number 
;; 	    (apply 'eform-reduce-rectangle op (eform-find-rectangle name)))))
;;       (delete-region start (+ start length))
;;       (goto-char start)      
;;       (insert (format (concat "%" (number-to-string length) "g") result)))))

;; (defun eform-incremental-find-result (name)
;;   "Find the next result entry matching regexp <name>"
;;   (if (not (re-search-forward 
;; 	    (concat *eform-result* name " \\([A-z+/\*-]*\\) (\\(.*\\))")
;; 	    nil
;; 	    t))
;;       '()
;;     (list (intern-soft (match-string 1))
;; 	  (match-beginning 2)
;; 	  (- (match-end 2) (match-beginning 2))			 
;; 	  (match-string 2))))

;; (defun eform-find-result (name)
;;   "Find result field with <name>"
;;   (save-excursion     
;;    (goto-char (point-min))
;;    (eform-incremental-find-result name)))

;; (defun eform-find-regexp-result (name)
;;   "Find regexp result field with <name>"
;;   (save-excursion 
;;    (goto-char (point-min))
;;    (re-search-forward (concat  "(\\(.*\\)) " *eform-result* name 
;; 			       " \\([A-z+/\*-]*\\) (\\(.*\\))"))
;;    (list 
;;     (intern-soft (match-string 2))
;;     (match-beginning 3)
;;     (- (match-end 3) (match-beginning 3))			 
;;     (match-string 3)
;;     (match-string 1))))

;; (defun eform-find-all-results ()
;;   "Return a list of all result fields in the document."
;;   (let (result)
;;     (save-excursion 
;;       (goto-char (point-min))
;;       (while (not (null (re-search-forward 
;; 			 (concat *eform-result* "\\([A-z0-9]*\\) [A-z+/\*-]* (.*)") 
;; 			 nil
;; 			 t)))
;; 	(setq result (cons (match-string 1) result)))
;;       result)))
    




