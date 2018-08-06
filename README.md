# shell-switch

Easily switch between *shell* buffers.

This adds two commands ``shell-switch'' and
``shell-switch-other-window'' that makes it easy to switch between
multiple `*shell*' buffers.

If there are no shell-mode buffers, then create one with
``\\[shell]''.  If there is exactly one shell-mode buffer then jump to
the end of the buffer.  You can create multiple shell-mode buffers
by using ``M-x rename-buffer'' to rename `*shell*' to something else
then use ``M-x shell'' to create a new buffer called `*shell*'.
Repeat as desired.

If there is more then one shell-mode buffer then prompt for the
shell-mode buffer to choose with the most recent one first and switch
to the selected buffer.  If the buffer selected is the current buffer,
then switch to the end of the buffer.


Suggested setup:

    (use-package shell-switch
      :commands (shell-switch shell-switch-other-window)
      :init
      (progn
        (bind-key* "C-c s" 'shell-switch)
        (bind-keys* :prefix-map clt-c-4-keymap
                    :prefix "C-c 4"
                    ("s" . shell-switch-other-window))))

