;; Version 2:
;; 1) recalc until buffer stops changing.
;; 2) use buffer local lisp variables for results of computations?
;; 3) Figure out how to exclude open/close characters from .* regexp in result field
;; 4) Clear all results function
;; 5) Bug when total-d-dec starts with a d..

(require 'gsn)

(defvar eform-mode nil "Electric Form Mode")
(make-variable-buffer-local 'eform-mode)
(add-to-list 'minor-mode-alist '(eform-mode " EForm"))

(setq eform-beg "(")
(setq eform-end ")")
(setq eform-tag "#")
(setq eform-char "[A-z0-9+/\*-]")
(setq eform-ws "[ \t]")
(setq eform-result-regexp-format (concat 
			     "\\(?:" eform-beg "\\(.*\\)" eform-end "\\)?" eform-ws "*"
			     ; eform-tag "\\(" eform-char "+\\)" eform-ws "*" 
			     eform-tag "\\(%s\\)" eform-ws "*"
			     "\\(" eform-char "*\\)" eform-ws "*"
			     "\\(" eform-char "*\\)" eform-ws "*"
			     "\\(" eform-char "*\\)" eform-ws "*"
			     "\\(" eform-char "*\\)" eform-ws "*"
			     eform-beg "\\(.*\\)" eform-end))

;; Should the whitespace be allowed?  Causes problems for whitespace splits
(setq eform-beg-rectangle-format (concat eform-beg 
					   eform-ws "*"
					   eform-tag "%s"))

(setq eform-end-rectangle-format (concat eform-tag "%s" 
					   eform-ws "*"
					   eform-end))

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
    (format "%d:%02d" hour min)))

(defun eform-mode (&optional arg)
  (interactive "P")
  (setq eform-mode 
	(if (null arg) (not eform-mode)
	  (> (prefix-numeric-value arg) 0))))

(defun eform-extract-regexp (&optional regexp)
  (mapcar (lambda (x) (nth 6 x)) (eform-find-all-results regexp)))

(defun eform-extract-rectangle (name)
  "Extract data from recangle
name is a string naming the rectangle
returns a list of strings"
  (save-excursion
    (let (start end)
      (goto-char (point-max))
      (setq start (re-search-backward (format eform-beg-rectangle-format name) nil t))
      (goto-char (point-min))
      (setq end   (re-search-forward  (format eform-end-rectangle-format name) nil t))
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
	    (format eform-result-regexp-format
		    (if (not (null name)) name
		      (concat eform-char "+"))) nil t))
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

(defun eform-find-all-names (&optional regexp)
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
  (message (concat "Found names: "
		   (reduce 'concat 
			   (mapcar (lambda (x) (concat x " ")) 
				(eform-find-all-names)))))
  (if eform-mode 
      (dolist (result (eform-find-all-names))
	(message (concat "Working on " result))
	(eform-update-result result))))
  
(provide 'eform)
