;; -*- lexical-binding: t; -*-
;;; Haskell

;; TODO: ag with prefix: save excursion, write "data" or "newtype" at end of file
;; select it with a region, call helm-do-ag, delete text, return to origin

(with-eval-after-load 'haskell

  ;; Dante
  (setq dante-command-line '("nix-shell" "nix/shell.nix" "--pure" "--run" "cabal repl --builddir=dist/dante"))
  (setq dante-repl-command-line '("nix-shell" "nix/shell.nix" "--pure" "--run" "cabal repl --builddir=dist/dante"))
  (defun my/dante-mode-hook ()
    (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))
  (add-hook 'dante-mode-hook 'my/dante-mode-hook)

  ;; Set the Haskell mode outline header syntax to be "-- *"
  ;; https://gist.github.com/alphapapa/0f76ffe9792fffecb017
  (defun my/haskell-mode-outline-hook ()
    (setq outshine-preserve-delimiter-whitespace t)
    (setq outline-regexp
          (rx
           ;; Outline headings
           (and (* space)
                (one-or-more (syntax comment-start))
                (* space)
                (group (one-or-more "\*"))
                (* space)))))

  (defun my/haskell-mode-pragma-hook ()
    ;; (spacemacs/toggle-whitespace-cleanup-off)
    (when (require 'haskell-pragma nil 'noerror)
      (haskell-pragma-mode)
      (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
        "ip"  'haskell-pragma-add-other-extension)))

  (add-hook 'haskell-mode-hook 'my/haskell-mode-outline-hook)
  (add-hook 'haskell-mode-hook 'my/haskell-mode-pragma-hook)
  ;; (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
  ;;   "gg"  'dumb-jump-go)

  ;; More align rules for Haskell
  ;; https://bit.ly/2Rn92GA
  ;; https://bit.ly/2Oh0lvt
  (with-eval-after-load 'align
    ;; Align EOL comments TODO: moves comment-only lines
    ;; (add-to-list 'align-rules-list
    ;;              '(haskell-eol-comment
    ;;                (regexp . "\\(\\s-+\\)--\\s-+")
    ;;                (modes . haskell-modes)))
    ;; Align arrow operators
    (add-to-list 'align-rules-list
                 '(haskell-eol-comment
                   (regexp . "\\(\\s-+\\)\\(>>>\\|<<<\\|&&&\\|\\*\\*\\*\\)\\s-+")
                   (modes . haskell-modes)))
    ;; Align if/then/else
    (add-to-list 'align-rules-list
                 '(haskell-ite
                   (regexp . "\\(\\s-+\\)\\(if\|then\|else\\)\\s-+")
                   (modes . haskell-modes)))
    ;; Align on applicative delimiters
    (add-to-list 'align-rules-list
                 '(haskell-applicative
                   (regexp . "\\(\\s-+\\)\\(<$>\\|<\\*>\\)\\s-+")
                   (modes . haskell-modes)))))
