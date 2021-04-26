;; -*- lexical-binding: t; -*-
;;; Lisp

(use-package lispy
  :commands lispy-mode
  :hook ((emacs-lisp-mode . lispy-mode)
         (racket-mode . lispy-mode))
  :config
  ;; Delete keymap
  (define-key lispy-mode-map (kbd "d") nil) ;; delete old binding
  (lispy-define-key lispy-mode-map-lispy "d" 'lispy-delete)
  (use-package lispyville
    :commands lispyville-mode
    :hook ((lispy-mode . lispyville-mode))
    :config
    ;; TODO: Attempt transient state:
    ;; TODO: https://github.com/abo-abo/hydra/wiki/Nesting-Hydras#visiting-other-hydras-temporarily
    (spacemacs|define-transient-state lispy
      :title "Lispy Transient State"
      :foreign-keys run
      :doc "
    ^Navigate^               ^Edit^            ^Mark^            ^Other
    ^^^^──────────────────────────────────────────────────────────────────────────────────────
    [_b_] undo move          [_c_] clone       [_m_] mark        [_q_] exit transient state
    [_>_] slurp               ^ ^              [_i_] mark-car
    [_<_] barf                ^ ^              [_l_] mark-list
    [_f_] forward             ^ ^              [_s_] mark-symbol
    [_h_] left
    [_j_] down
    [_k_] up
    [_l_] right"

      :bindings
      ("q" nil :exit t)

      ;; Navigate
      (">" sp-forward-slurp-sexp)
      ("<" lispy-barf)
      ("f" lispy-flow)
      ("b" lispy-back)
      ("h" lispy-left)
      ("j" lispy-down)
      ("k" lispy-up)
      ("l" lispy-right)

      ;; Edit
      ("c" lispy-clone)

      ;; Mark
      ("m" lispy-mark)
      ("i" lispy-mark-car)
      ("l" lispy-mark-list)
      ("s" lispy-mark-symbol))))

(dolist (mode (list 'racket-mode 'emacs-lisp-mode))
  (spacemacs/set-leader-keys-for-major-mode mode
    ">"   #'sp-forward-slurp-sexp
    "<"   #'lispy-barf
    "rr"  #'emr-show-refactor-menu
    "ic"  #'lispy-clone
    "xf"  #'lispy-delete
    "xb"  #'lispy-delete-backward))

(dolist (hook (list 'racket-mode-hook 'emacs-lisp-mode-hook))
  (add-hook hook #'eros-mode))

(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
