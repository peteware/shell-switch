;;;
;;; shell-switch - make it easy to switch between *shell* buffers.
;;;
;;; 
;;;
;;; (1) make sure shell-switch is on your path.  You may need to
;;;     add something like this to your ~/.emacs (or ~/.xemacs/init.el):
;;;
;;;     (add-to-list 'load-path (concat (getenv "HOME") "/.xemacs"))
;;;
;;; (2) In your ~/.emacs (or ~/.xemacs/init.el):
;;;
;;;     (require 'shell-switch)
;;;
;;; (3) I prefer to use this binding but it's up to you to put this
;;;     in your ~/.emacs (or ~/.xemacs/init.el):
;;;     (global-set-key [(control c) (s)] 'shell-switch)
;;;
(provide 'shell-switch)
(require 'cl)
;(require 'cl-seq)

(defvar shell-switch-last nil
  "The last buffer switched to.")

(defvar shell-switch-history nil
  "List of shell buffers that have been switched to.")

(defun shell-switch()
  "Switch to a shell-mode buffer.  If there are no
shell-mode buffers, then create one with ``\\[shell]''.  If there is exactly
one such buffer then switch to it.  If there are several, prompt for
the buffer name.  You can use history to switch between your shells
\\<minibuffer-local-map>``\\[previous-history-element]'' for the previous shell
and ``\\[next-history-element]'' for the next shell.

If you don't select a name (i.e. hit return immediately) then if the current
buffer is a shell-mode buffer use it otherwise switch to the one you most
recently  used.

If the shell-mode buffer you choose is the current buffer then go to
the end of the buffer.
"
  (interactive)
  (shell-switch-internal nil))

(defun shell-switch-other-window()
  (interactive)
  (shell-switch-internal 'other))

(defun shell-switch-cmp-buffers (buf1 buf2)
  (string< (buffer-name buf1) (buffer-name buf2))
  )

(defun shell-switch-internal(other-window)
  "Implements shell switching but with flag to indicate if other-window."
  (let* ((shells (sort (remove-if-not 'shell-switch-modep (buffer-list)) 'shell-switch-cmp-buffers))
	 (count (length shells)))
    ;; make sure current shell is first
    (if (shell-switch-modep (current-buffer))
        (setq shells (append (list (current-buffer)) (remove (current-buffer) shells))))
    (cond ((= 0 count)
	   ;; Create a new shell
	   (shell))
	  ((= 1 count)
	   ;; Only one so just go to it
	   (shell-switch-this (buffer-name (car shells)) other-window))
	  ((and (> count 1)
		(shell-switch-modep (current-buffer))
		(eobp))
	   ;; If > 1 shell and in shell buffer and at end of buffer then go to next shell
	   (let ((pos (position (current-buffer) shells)))
	     (if (eq pos nil)
		 (setq pos 0)
	       (setq pos (mod (1+ pos) count))
	     (shell-switch-this (buffer-name (nth pos shells)) other-window))
	   ))
	  (t
	   (let* ((shell-completions (mapcar 'shell-switch-prep shells))
		  (names (mapcar 'buffer-name shells))
		  (shell-last (first names))
		  (hist shell-switch-history)
		  (history-delete-duplicates t)
 		  (buf nil)
                  (ivy-sort-functions-alist nil)
		  )
             (message (format "%s" shell-completions))
	     (setq buf (completing-read
			(concat "shell-switch (default \"" shell-last "\"): ")
			shell-completions nil t nil 'hist nil))
	     (shell-switch-this buf other-window)
	     )
	   )
	  )
    )
  )

(defun shell-switch-local (symbol buffer)
  "Return value of local variable SYMBOL in BUFFER.  Same as Xemacs's 'symbol-value-in-buffer' and Emacs's 'buffer-local-value'."
  (if (fboundp 'symbol-value-in-buffer)
      (symbol-value-in-buffer symbol buffer)
    (buffer-local-value symbol buffer)))

(defun shell-switch-modep (buffer)
  "Return if buffer is in shell mode."
  (interactive "b")
  (if (not (bufferp buffer))
      nil
      (or (equal 'shell-mode (shell-switch-local 'major-mode buffer))
          (equal 'term-mode (shell-switch-local 'major-mode buffer)))
      ))

(defun shell-switch-this (name other-window)
  "Switch to buffer NAME.  If NAME is the current buffer, then
go to the end of the buffer.  If NAME is empty and the current
buffer is in shell-mode, then go to the end of the current buffer.
If NAME is empty and the current buffer is not shell-mode then use (shell)
to create a new shell buffer or switch to *shell*."
  (if (string= name (buffer-name (current-buffer)))
      (goto-char (point-max))
    (if (= (length name) 0)
	(if (shell-switch-modep (current-buffer))
	    (goto-char (point-max))
	  ; else
	  (progn
	    (if (and shell-switch-last (shell-switch-modep (get-buffer shell-switch-last)))
		(if other-window
		    (switch-to-buffer-other-window shell-switch-last)
		  (switch-to-buffer shell-switch-last))
	      ; else
	      (shell))))
      ; else
      (if other-window
	  (switch-to-buffer-other-window name)
	(switch-to-buffer name))))
  (setq shell-switch-last (buffer-name (current-buffer)))
  (add-to-history 'shell-switch-history shell-switch-last)
  )

(defun shell-switch-prep (buffer)
  "Return an alist with (buffer-name . buffer)."
  (cons (buffer-name buffer) buffer))

