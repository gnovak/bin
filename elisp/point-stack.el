(defvar point-stack nil)

(defun point-stack-push ()
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (list (current-buffer) (point)) point-stack)))

(defun point-stack-pop ()
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar point-stack))
    (goto-char (cadar point-stack))
    (setq point-stack (cdr point-stack))))

(provide 'point-stack)
