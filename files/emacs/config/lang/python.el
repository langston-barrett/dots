;; -*- lexical-binding: t; -*-
;;; Python

(with-eval-after-load 'python

  ;; TODO: only do this if typing is imported? or something
  ;; (setq-default flycheck-enabled-checkers '(flycheck-mypy))
  ;; (when (require 'flycheck-mypy nil 'noerror)
  ;;   (flycheck-add-next-checker 'python-flake8 'python-mypy t))

  (setq lsp-pyls-plugins-pyflakes-enabled nil)
  (setq lsp-pyls-plugins-pycodestyle-enabled nil)
  (defun my/set-flycheck-error-level ()
    (setq flycheck-navigation-minimum-level 'error)
    (setq flycheck-error-list-minimum-level 'error))
  (add-hook 'python-mode-hook 'my/set-flycheck-error-level)

  ;; TODO: Why doesn't this work?
  ;; (flycheck-select-checker 'python-pycompile)

  (custom-set-variables
    '(flycheck-python-flake8-executable "python3")
    '(flycheck-python-pycompile-executable "python3")
    '(flycheck-python-pylint-executable "python3"))

  (defun my/insert-print ()
    (interactive)
    (insert "print(\"~\"*40, _)"))
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "ip" 'my/insert-print))

;; Jump to top-level definitions
;; (spacemacs/set-leader-keys-for-major-mode 'python-mode
;;   (completing-read prompt candidates nil t)
;;   "gt"  '(lambda () (interactive) (helm-ag-this-file)))
