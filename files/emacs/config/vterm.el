;; -*- lexical-binding: t; -*-
;;; vterm

;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun my/read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

(defun my/insert-from-zsh-history ()
  (interactive)
  (kill-new
   (helm :sources (helm-build-sync-source "zsh history"
                    :candidates (my/read-lines "~/.zsh_history")
                    :fuzzy-match t)
         :buffer "*zsh history*")))

(with-eval-after-load 'vterm
  (spacemacs/set-leader-keys-for-major-mode 'vterm-mode
    "ih" 'my/insert-from-zsh-history
    "c"  'vterm-copy-mode)
  (spacemacs/set-leader-keys-for-major-mode 'vterm-copy-mode
    "c"  'vterm-copy-mode-done)

  (defun my/vterm-mode-hook ()
    (evil-local-set-key 'normal "P" 'vterm-yank)
    (evil-local-set-key 'normal "p" 'vterm-yank)
    (olivetti-tentative-mode -1))

  (add-hook 'vterm-mode-hook 'my/vterm-mode-hook))
