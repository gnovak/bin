;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun filter (pred lst &optional result)   
  "Filter <lst> with function <pred>"
  (cond ((null lst) (reverse result))
	((funcall pred (car lst)) 
	 (filter pred (cdr lst) (cons (car lst) result)))
	(t (filter pred (cdr lst) result))))

(defun reduce (f lst &optional r)
  "Reduce <lst> with binary function <f> in a left-associative manner"
  (cond ((null lst) r)
	((null r) (reduce f (cdr lst) (car lst)))
	(t (reduce f (cdr lst) (funcall f r (car lst))))))

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