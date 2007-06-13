;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun any (lst)
  (cond ((null lst) nil)
	((car lst) (car lst))
	(t (any (cdr lst)))))

(defun all (lst)
  (cond ((null lst) t)
	((not (car lst)) nil)
	(t (all (cdr lst)))))

(defun range (min &optional max)
  (when (null max)
    (setq max min)
    (setq min 0))
  (let (result)
    (dotimes (i (1+ (- max min)))
      (setq result (cons (+ min i)
			 result)))
    (reverse result)))

(defun zip (&rest seqs)  
  (if (null (car seqs)) 
      nil
    (cons (mapcar 'car seqs)
	  (apply 'zip (mapcar 'cdr seqs)))))

(defun filter (pred lst &optional success-list)   
  "Filter <lst> with function <pred>"
  (if (null success-list) 
      (filter pred lst lst)
      (let (result)
	(dotimes (i (length lst) (reverse result))
	  (when (funcall pred (nth i lst))
	      (push (nth i success-list) result))))))

(defun reduce (f lst &optional r)
  "Reduce <lst> with binary function <f> in a left-associative manner"
  (cond ((null lst) r)
	((null r) (reduce f (cdr lst) (car lst)))
	(t (reduce f (cdr lst) (funcall f r (car lst))))))


(defun make-alist (l1 l2)
  (let (result)
    (while (not (null l1))
      (setq result (cons (cons (car l1) (car l2)) result))
      (setq l1 (cdr l1))
      (setq l2 (cdr l2)))
    (reverse result)))
      
(defun buffer-names ()
  (mapcar 'buffer-name (buffer-list)))

;; Bike log convenience functions
(defun to-hour-minute (total)
  "from number of minutes to hour minute"
  (let ((hour (/ total 60))
	(min (mod total 60)))
    (format "%d:%02d" hour min)))

(defun from-hour-minute (str)
  "from hour minute to number of minutes"
  (let ((split (split-string str ":")))
    (if (= (length split) 1)
	;; only minutes
	(string-to-number str)
      (+ (* 60 (string-to-number (car split))) 
	 (string-to-number (cadr split))))))


;; Take date, time, distance, elev on current line,
;; calculate average speed/hour, average elev/hour and
;; insert at end of current line
(defun gsn/bike-log-calculate-averages () 
  (interactive)
  (let ((line (split-string (thing-at-point 'line))))
    (save-excursion 
      (end-of-line)
      (insert 
       (format 
	"%5.1f %5.0f" 
	(* 60.0 (/ (float (string-to-number (nth 1 line)))
		   (float (from-hour-minute (nth 2 line)))))
	(* 60.0 (/ (float (string-to-number (nth 3 line)))
		   (float (from-hour-minute (nth 2 line))))))))))


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

(defun string-op (op &rest args)
  (number-to-string 
   (apply op 
	  (mapcar (lambda (x) (if (stringp x) 
				  (string-to-number x)
				x))		  
		  args))))

(defun string+ (&rest args) (apply 'string-op '+ args))
(defun string- (&rest args) (apply 'string-op '- args))

(setq gsn/ses-depreciation-date (format-time-string "%m/%Y" (current-time))
      safe-functions '(gsn/ses-depreciation gsn/ses-filter gsn/ses-contains))
      
(defun gsn/ses-contains (name date)
  (zoom-contains name
		 (calendar-absolute-from-gregorian 
		  (mapcar 'string-to-number 
			  (split-string date "/")))))

(defun gsn/ses-filter (f filter-list result-list)
  (filter f 
	  (filter (lambda (x) (not (null x))) filter-list filter-list)
	  (filter (lambda (x) (not (null x))) filter-list result-list)))

(defun gsn/ses-string-to-months (date-string)
  "Convert strings of the form '12/2006' to the number of months since 2000"
  (string-match "\\([0-9][0-9]*\\)/\\([0-9][0-9][0-9][0-9]\\)" date-string)
  (let ((month (string-to-number (match-string 1 date-string)))
	(year (string-to-number (match-string 2 date-string))))
    (float (1- (+ (* 12 (- year 2000)) month)))))

(defun gsn/ses-depreciation (date-string value lifetime)
  "date-string is of the form mm/yyyy, value is original price of
  item, lifetime is time for object to halve its value, in years.
  Returns current value of object.  Can calculate this for
  different times by rebinding gsn/ses-depreciation-date"
  (let ((t1 (gsn/ses-string-to-months gsn/ses-depreciation-date))
	(t2 (gsn/ses-string-to-months date-string)))
    (* (float value) (expt 2.0 (/ (- t2 t1) (* 12.0 lifetime))))))

(defun gsn/py-function-defs-in-buffer (&optional buffer with-line-numbers)
  "Return a list of the names of all python functions defined in
the current buffer"
  (if (null buffer) 
      (setq buffer (buffer-name)))
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let ((result nil))
      (while (re-search-forward "def \\([A-z0-9_]+\\)(" nil t nil)
	(setq result (cons (if (not with-line-numbers)
			       (match-string-no-properties 1) 
			     (list (match-string-no-properties 1)
				   (line-number-at-pos)))
			   result)))
      (remove-duplicates result :test 'equal))))

(defun gsn/py-function-calls-in-region (&optional low high)
  "Return a list of the names of all python functions called
between low and high.  If low is nil, set it to (point-min).  If
high is nil, set it to point-max"
  (if (null low) 
      (setq low (point-min)))
  (let ((result nil))
    (save-excursion
      (goto-char low)
      (while (re-search-forward "\\([A-z0-9_]+\\)(" high t nil)
	(setq result (cons (match-string-no-properties 1) result))))
    (remove-duplicates result :test 'equal)))
    
(defun gsn/py-def-or-class-region (&rest args)
  "Return a region containing the current def or class"
  (list (save-excursion (apply 'py-beginning-of-def-or-class args) (point))
	(save-excursion (apply 'py-end-of-def-or-class args) (point))))
  
(defun gsn/py-find-untested-functions-in-region (defbuf usebuf low high)
  "Find untested function definitions.

defbuf = buffer with definitions
low, high = region with tests

without prefix arg use defbuf = current buffer and region is
  enclosing class at point

with single prefix arg use defbuf = read from minibuffer and
region is enclosing class at point

with double prefix arg use defbuf = read from minibuffer and
region is entirety of another buffer"
  (interactive 
   (cond ((equal current-prefix-arg nil) 
	  (append (list (buffer-name) (buffer-name)) 
		  (gsn/py-def-or-class-region t)))
	 ((equal current-prefix-arg '(4))
	  (append 
	   (list (read-buffer "Buffer with definitions:" (buffer-name) t) 
		 (buffer-name))
	   (gsn/py-def-or-class-region t)))

	 ((equal current-prefix-arg '(16))
	  (list (read-buffer "Buffer with definitions:" (buffer-name) t)
		(read-buffer "Buffer with tests:" (buffer-name) t)
		nil nil)))) ; can't get limits of test buffer yet

  (let ((fdefs (gsn/py-function-defs-in-buffer defbuf t))
	(fcalls (save-excursion 
		  (set-buffer usebuf)
		  (gsn/py-function-calls-in-region low high))))
    (while fcalls
      (setq fdefs (remove-if (lambda (x) (equal (car x) (car fcalls)))
			     fdefs))
      (setq fcalls (cdr fcalls)))
    
    (if (get-buffer "*Untested Functions*")
	(kill-buffer "*Untested Functions*"))
    (switch-to-buffer "*Untested Functions*")      
    (insert "*** Untested functions ***\n\n")
    (let ((outputs (reverse fdefs)))
      (while outputs
	(insert (format "%s:%d: %s\n" defbuf (cadar outputs) (caar outputs)))
	(setq outputs (cdr outputs))))
    (compilation-mode)))

(defun gsn/bbdb-find-address (location)
  (let (result)
    (dolist (addr (bbdb-record-addresses (bbdb-current-record)) result)
      (if (string= location (bbdb-address-location addr))
	  (setq result addr)))))

(defun gsn/bbdb-envelope (location)
  (interactive "sLocation:")
  (let ((rec (bbdb-current-record))
	(addr (gsn/bbdb-find-address location)))
    (with-temp-buffer
      (gsn/bbdb-address-to-buffer rec addr)
      ;; Used this for high envelopes.  Note axis inversion.
      ; (shell-command-on-region (point-min) (point-max) "envelope -y 315"))))
      (shell-command-on-region (point-min) (point-max) "envelope"))))

(defun gsn/bbdb-latex-letter (location)
  (interactive "sLocation:")
  (let ((rec (bbdb-current-record))
	(addr (gsn/bbdb-find-address location)))
    (switch-to-buffer (generate-new-buffer 
		       (generate-new-buffer-name "letter.tex")))
    (gsn/bbdb-address-to-buffer rec addr)
    ;; (query-replace-regexp "$" "\\\\")
    (goto-char (point-min))
    (dotimes (i  (1- (count-lines (point-min) (point-max))))
      (goto-char (line-end-position))
      (insert " \\\\")
      (forward-line 1))
    (goto-char (point-min))
    (insert "\\begin{letter}{\n")
    (goto-char (point-max))
    (insert "}\n\n\\end{letter}")))

(defun gsn/bbdb-address-to-buffer (rec addr)
  (when (bbdb-record-firstname rec)
    (insert (bbdb-record-firstname rec))
    (insert " "))
  (insert (bbdb-record-lastname rec))
  (insert "\n")
  (dolist (line (bbdb-address-streets addr))
    (insert line)
    (insert "\n"))
  (insert (bbdb-address-city addr))
  (insert ", ")
  (insert (bbdb-address-state addr))
  (insert "  ")
  (insert (bbdb-address-zip addr))
  (insert (bbdb-address-country addr)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IDL

(defun gsn/idlwave-help-shell () 
  "Start IDL with decent online help.  

IDL 6.1 has shitty (and I mean really shitty) online help, so we
want to use a newer version of IDL.  However, I have an unlimited
license with 6.1, and pyIDL currently depends on 6.1, so there
are some environment variables that have to be set to 6.1 for it
to work.  Here we change the vars that will cause 6.3 to be
started, and then change them back."
  (interactive)
  (let ((orig-v (getenv "IDL_VERSION"))
        (orig-d (getenv "IDL_DIR")))
    (setenv "IDL_VERSION" "6.3") 
    (setenv "IDL_DIR" "/Applications/rsi/idl_6.3") 
    (idlwave-shell) 
    (setenv "IDL_VERSION" orig-v)
    (setenv "IDL_DIR" orig-d)))

(provide 'gsn)