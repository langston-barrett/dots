;; -*- lexical-binding: t; -*-
;;; EXWM

;;;; exwm-randr

(use-package exwm-randr
  :config
  (setq exwm-randr-workspace-monitor-plist '(0 "eDP1" 1 "DP1"))
  (setq exwm-randr-screen-change-hook nil)
  (exwm-randr-enable))

;;;; exwm-systemtray

(use-package exwm-systemtray
  :config
  (exwm-systemtray-enable))

;;;; exwm

(use-package exwm
  :config
  (defun my/exwm-async-run (name)
    "Run a process asynchronously"
    (interactive)
    (start-process name nil name))

  ;; (defun my/exwm-init-hook ()
  ;;   (interactive))
  ;; (add-hook 'exwm-init-hook #'my/exwm-init-hook)

  ;; Escape to Emacs-land!
  (add-to-list 'exwm-input-prefix-keys (kbd "M-SPC"))
  (add-to-list 'exwm-input-prefix-keys (kbd "C-SPC"))
  (add-to-list 'exwm-input-prefix-keys (kbd "<super>-SPC"))

  ;; Make class name the buffer name
  (add-hook
   'exwm-update-class-hook
   (lambda ()
     (exwm-workspace-rename-buffer exwm-class-name)))

  (defun my/start (command)
    "Launch applications with a shell command"
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command command nil command))

  (defun my/ws0 () (interactive) (exwm-workspace-switch 0))
  (defun my/ws1 () (interactive) (exwm-workspace-switch 1))

  (setq exwm-input-global-keys
        `((,(kbd "M-b") . switch-to-buffer)
          (,(kbd "M-<return>") . vterm)
          (,(kbd "M-h") . evil-window-left)
          (,(kbd "M-j") . evil-window-down)
          (,(kbd "M-k") . evil-window-up)
          (,(kbd "M-l") . evil-window-right)
          (,(kbd "M-c") . kill-this-buffer)
          (,(kbd "M-f") . my/ws0)
          (,(kbd "M-s") . my/ws1)
          (,(kbd "M-r") . my/start)))
  (setq exwm-workspace-show-all-buffers t)
  (setq exwm-layout-show-all-buffers t)
  (setq exwm-workspace-number 2)
  (setq exwm-workspace-minibuffer-position nil)
  (setq exwm-workspace-display-echo-area-timeout 1)

  ;; (push (aref (kbd "<escape>") 0) exwm-input-prefix-keys)
  ;; (push (aref (kbd "<return>") 0) exwm-input-prefix-keys)
  (display-battery-mode 1)
  (exwm-enable))
