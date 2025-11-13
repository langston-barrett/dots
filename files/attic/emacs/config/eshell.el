;; -*- lexical-binding: t; -*-
;;; eshell

(with-eval-after-load 'eshell
  (setq eshell-prompt-function
        (lambda ()
          (concat
           "┌─["
           "]──["
           (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "yellow"))
           "]──["
           (replace-regexp-in-string "^/home/my/" "~/" (eshell/pwd))
           "]\n"
           "└─"
           (if (= (user-uid) 0) "# " "> ")
           )))

  (setq eshell-aliases-file "~/.emacs.d/eshell/alias") ; TODO: change this

  (if (boundp 'eshell-visual-commands)
      (add-to-list 'eshell-visual-commands "ghcid")
    (setq eshell-visual-commands
          '("ghcid" "matterhorn" "mpw" "less" "more" "top" "htop")))


  ;; Use C-r for backwards terminal search, even in evil mode
  (defun my/eshell-hook ()
    (when (require 'helm-shell-history nil 'noerror)
      (spacemacs/set-leader-keys-for-major-mode 'eshell-mode
        "h"  'helm-shell-history))
    (evil-local-set-key 'insert (kbd "C-r") 'helm-eshell-history))
  (add-hook 'eshell-mode-hook 'my/eshell-hook))
