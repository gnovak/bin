;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BBDB Customizations

;; What actually needs to be done:

;; Creation date can be invisible
;; Multi-line notes fields
;; Edit notes in full fledged buffer, not minibuffer.

;; Division of work b/t planner and bbdb: Should DB have fields w/
;;   "contact after" or "open contact"?

;; Should log be handled as lots of specialized fields, or one field w/
;; many entries (like address)

;; How does searching work?  Based on fields, search whole record, or
;; what?  Implementation of history log will depend on how to integrate
;; with searching.

;; search on open contacts

;; search on pending contacts

;; facilities for undated to-do stuff

;; states - closed, open-waiting on them, open-waiting on me, priority field.

;; edit fields in a full buffer

(defadvice bbdb-prompt-for-new-field-value (around dont-ask)
  "Force BBDB to accept new fields w/o asking"
  (flet ((bbdb-y-or-n-p (&rest args) t))
    ad-do-it))

(defun bbdb-add-interaction (date)
  "Add an interaction with a person"
  ;; date, default today, and message
  (interactive "sDate (today) :")
  ;; default date
  (if (string= date "") (setq date (format-time-string 
				    bbdb-time-internal-format 
				    (current-time))))
  ;; save context
  (setq bbdb-gsn-date date)
  ;; disable annotation functions
  (setq bbdb-gsn-annotation-functions remember-annotation-functions)
  (setq remember-annotation-functions nil)
  ;; add hook to catch results
  (add-hook 'remember-handler-functions 'bbdb-remember-interaction)
  ;; create buffer for interaction notes
  (remember))

(defun bbdb-remember-interaction ()
  (let ((date bbdb-gsn-date)
	(message (buffer-string)))
    ;; Add date to property names if necessary
    (unless (assoc date (bbdb-propnames))
      (bbdb-set-propnames
       (append (bbdb-propnames) (list (list date)))))
    ;(erase-buffer)
    ;; swicth to bbdb buffer, hopefully landing on same record
    (switch-to-buffer "*BBDB*")    
    (bbdb-insert-new-field (make-symbol date) message)
    ;; switch back to allow remember to restore window config
    (switch-to-buffer "*Remember*")
    ;; restore annotation functions
    (setq remember-annotation-functions bbdb-gsn-annotation-functions)
    ;; remove hook to allow remember to work elsewhere
    (remove-hook 'remember-handler-functions 'bbdb-remember-interaction)))

;  (ad-enable-advice 'bbdb-prompt-for-new-field-value 'around 'dont-ask)
;  (ad-activate 'bbdb-prompt-for-new-field-value)
;  (unwind-protect 
;      (call-interactively 'bbdb-insert-new-field)
;    (ad-disable-advice 'bbdb-prompt-for-new-field-value 'around 'dont-ask)
;    (ad-activate 'bbdb-prompt-for-new-field-value)))

(setq bbdb-display-layout-alist
  '((one-line   (order     . (notes net mail-alias phones ))
                (name-end  . 24)
                (toggle    . t))
    (multi-line (indention . 14)
                (toggle    . t))
    (pop-up-multi-line (indention . 14)))
