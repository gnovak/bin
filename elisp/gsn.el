;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun r-filter (pred lst &optional result)   
  "Filter <lst> with function <pred>"
  (cond ((null lst) (reverse result))
	((funcall pred (car lst)) 
	 (filter pred (cdr lst) (cons (car lst) result)))
	(t (filter pred (cdr lst) result))))

(defun filter (pred lst)   
  "Filter <lst> with function <pred>"
  (let (result) 
    (dolist (elt lst (reverse result)) 
      (if (funcall pred elt) 
	  (setq result (cons elt result))))))

(defun reduce (f lst &optional r)
  "Reduce <lst> with binary function <f> in a left-associative manner"
  (cond ((null lst) r)
	((null r) (reduce f (cdr lst) (car lst)))
	(t (reduce f (cdr lst) (funcall f r (car lst))))))


(defun recursively-find-directories (root)    
  "Start at given directory and recursively descend, building a list of all directories below"
  ;; Add in our own name
  (cons 
   root 
   ;; Each call to recursively-find-directories returns a list of the
   ;; direcotry name plus all subdirectory names.  Splice them all
   ;; together so that we return a single list (not a nested list) to
   ;; the next higher level.
   (reduce 
    'append 
    ;; Recursively descend directories, 
    (mapcar 
     'recursively-find-directories
     (filter 
      ;; Filter out non-directories and ".", "..", and CVS entries
      (lambda (file) (and (file-directory-p (concat file))
			  (not (member (file-name-nondirectory file)
				       '("." ".." "CVS")))))
      ;; Get files in current directory
      (directory-files (expand-file-name root) t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Crap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't actually need this, keep it around to remind
;; myself how to write macros
(defmacro preserving-point (&rest code)
  (let ((previous-point (make-symbol "--previous-point--"))
	(result (make-symbol "--result--")))
    `(let* ((,previous-point (point-marker))
	    (,result (progn ,@code)))
       (goto-char ,previous-point)
       ,result)))


(provide 'gsn)