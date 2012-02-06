;;; point-stack.el
;; This file is free software.
;; Copyright Greg Novak (novak@ucolick.org) December 2004
;; Released under the GPL, available at http://www.gnu.org
;;
;; Provide a stack of buffer names and point locations
;;
;; I find this useful because I often need to do something elsewhere
;; in a file and then return to what I was doing.  Ie, I need to run
;; around in the file to find a function definition or something.
;; Then I want to be taken right back to where I was before the
;; "context switch"
;;
;; You'll probably want the following in your .emacs file.  My
;; mnemonic for the two commands is "Mark location" and "go Back"
;;
;; (require 'point-stack) 
;; (global-set-key "\C-cm" 'point-stack-push)
;; (global-set-key "\C-cb" 'point-stack-pop)
;;
;; Feb 6, 2012: Piggy-back onto bookmark.el to allow bookmarks into
;; more formats (e.g. PDF documents in DocView to work with
;; point-stack.el).  This would be easier to do if bookmark.el
;; provided "clean" ways to remember and jump to places, but instead
;; it assumes that everything you want to bookmark is associated with
;; a file, and that whenever you jump to a bookmark you want to
;; remember history, apply hooks, etc.  I am forced to provide the
;; "clean" versions that I want myself, below.
;;
;; However, in spite of this, the great advantage of using the
;; bookmarks.el format is that major mode writers already take care to
;; provide handlers to store and jump to bookmarks if the defaults
;; don't work.  This is the case with, e.g. Info and DocView buffers.

(require 'bookmark)

(defvar point-stack nil)

(defun point-stack-push ()
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (point-stack-remember) point-stack)))

(defun point-stack-pop ()
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (point-stack-jump (car point-stack))
    (setq point-stack (cdr point-stack))))

(defun point-stack-remember ()
  "bookmark.el is fussy about requiring that bookmarks correspond
to files, because it wants to save everything and have bookmarks
be usable across sessions.  Try to ensure reasonable behavior
even if the buffer doesn't correspond to a file."
  (condition-case nil
      (bookmark-make-record)
    (error (append (list "name"
                         (cons 'buffer (current-buffer)))
                   (bookmark-make-record-default t)))))

(defun point-stack-jump (bookmark)
  "bookmark.el does not seem to provide a function that just
jumps to a bookmark and does nothing else---doesn't run hooks,
doesn't try to relocate bookmarks, etc.  The interactive command
is bookmark-jump, which calls bookmark--jump-via, which calls
bookmark-handle-bookmark.  Even this last one tries to do
something with popping dialog boxes, relocating bookmarks, etc.
So just copy the code from the relevant functions."
  ;; code snippet from bookmark-handle-bookmark 
  (funcall (or (bookmark-get-handler bookmark)
               'bookmark-default-handler)
           (bookmark-get-bookmark bookmark))
  ;; code snippet from bookmark--jump-via 
  (save-current-buffer
    (switch-to-buffer (current-buffer)))
  (let ((win (get-buffer-window (current-buffer) 0)))
    (if win (set-window-point win (point)))))

(provide 'point-stack)
;;; point-stack.el ends here
