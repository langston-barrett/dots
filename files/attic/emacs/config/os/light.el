;; Light --- Backlight control -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Using Emacs as an application launcher
;;
;; https://www.mattduck.com/emacs-fuzzy-launcher.html

;;; Code:

(defun my/backlight-up ()
  (interactive)
  (shell-command "light -A 20"))

(defun my/backlight-down ()
  (interactive)
  (shell-command "light -U 20"))

(defhydra my/backlight-hydra (:hint nil)
  "\n[_+_/_=_/_k_] up [_-_/___/_j_] down [_q_] quit"
  ("+" my/backlight-up)
  ("=" my/backlight-up)
  ("k" my/backlight-up)
  ("-" my/backlight-down)
  ("_" my/backlight-down)
  ("j" my/backlight-down)
  ("q" nil :exit t))

