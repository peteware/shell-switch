;;; shell-switch.el --- Easily switch between *shell* buffers
     
;; Copyright (C) 2018 Pete Ware

;; Author: Pete Ware <ware@peteware.com>
;; Maintainer: Pete Ware <ware@peteware.com>
;; Created: 1 Jan 2001
;; Version: 1.0
;; Keywords: processes terminals
;; Homepage: https://github.com/peteware/shell-switch

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; along with this file.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; This adds two commands ``shell-switch'' and
;; ``shell-switch-other-window'' that makes it easy to switch between
;; multiple `*shell*' buffers.
;;
;; If there are no shell-mode buffers, then create one with
;; ``\\[shell]''.
;;
;; If there is exactly one shell-mode buffer then jump to the end of
;; the buffer.  You can create multiple shell-mode buffers by using
;; ``M-x rename-buffer'' to rename `*shell*' to something else then
;; use ``M-x shell'' to create a new buffer called `*shell*'.  Repeat
;; as desired.
;;
;; If there is more then one shell-mode buffer then prompt for the
;; shell-mode buffer to choose with the most recent one first and
;; switch to the selected buffer.  If the buffer selected is the
;; current buffer, then switch to the end of the buffer.
;;
;; (use-package shell-switch
;;   :commands (shell-switch shell-switch-other-window)
;;   :init
;;   (progn
;;     (bind-key* "C-c s" 'shell-switch)
;;     (bind-keys* :prefix-map clt-c-4-keymap
;;                 :prefix "C-c 4"
;;                 ("s" . shell-switch-other-window))))

;;; Code:
(provide 'shell-switch)
(require 'cl)

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

;;; shell-switch.el ends here
