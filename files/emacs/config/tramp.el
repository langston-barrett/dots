;; -*- lexical-binding: t; -*-
;;; TRAMP

(defconst
  my/hosts
  '("big"))

(defconst
  my/host-username-alist
  '(("big" . "langston")))

(defun my/choose-host ()
  (interactive)
  (completing-read "Host:" my/hosts))
