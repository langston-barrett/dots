;; -*- lexical-binding: t; -*-
;;; vterm

(with-eval-after-load 'vterm
  (spacemacs/set-leader-keys-for-major-mode 'vterm-mode
    "ih" 'my/insert-from-zsh-history
    "c"  'vterm-copy-mode
    "n" 'multi-vterm-next
    "p" 'multi-vterm-prev
    "t" 'multi-vterm-dedicated-toggle
    )
  (spacemacs/set-leader-keys-for-major-mode 'vterm-copy-mode
    "c"  'vterm-copy-mode-done)

  (defun my/vterm-mode-hook ()
    (evil-local-set-key 'normal "P" 'vterm-yank)
    (evil-local-set-key 'normal "p" 'vterm-yank)
    (evil-local-set-key 'insert "C-k" 'vterm-send-up)
    (evil-local-set-key 'insert "C-j" 'vterm-send-down)
    (evil-local-set-key 'normal "C-k" 'vterm-send-up)
    (evil-local-set-key 'normal "C-j" 'vterm-send-down)
    (olivetti-tentative-mode -1))

  (add-hook 'vterm-mode-hook 'my/vterm-mode-hook))
