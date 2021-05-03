;; -*- lexical-binding: t; -*-
;;; Lisp

(setq
 my/lispy-binds
 '(("<" "lispy-barf" "")
   ("A" "lispy-beginning-of-defun" "")
   ("j" "lispy-down" "")
   ("Z" "lispy-edebug-stop" "")
   ("B" "lispy-ediff-regions" "")
   ("G" "lispy-goto-local" "")
   ("h" "lispy-left" "")
   ("N" "lispy-narrow" "")
   ("y" "lispy-occur" "")
   ("o" "lispy-other-mode" "")
   ("J" "lispy-outline-next" "")
   ("K" "lispy-outline-prev" "")
   ("P" "lispy-paste" "")
   ("l" "lispy-right" "")
   ("I" "lispy-shifttab" "")
   (">" "lispy-slurp" "")
   ("SPC" "lispy-space" "")
   ("xB" "lispy-store-region-and-buffer" "")
   ("u" "lispy-undo" "")
   ("k" "lispy-up" "")
   ("v" "lispy-view" "")
   ("V" "lispy-visit" "")
   ("W" "lispy-widen" "")
   ("D" "pop-tag-mark" "")
   ("x" "see" "")
   ("L" "unbound" "")
   ("U" "unbound" "")
   ("X" "unbound" "")
   ("Y" "unbound" "")
   ("H" "lispy-ace-symbol-replace" "Edit")
   ("c" "lispy-clone" "Edit")
   ("C" "lispy-convolute" "Edit")
   ("n" "lispy-new-copy" "Edit")
   ("O" "lispy-oneline" "Edit")
   ("r" "lispy-raise" "Edit")
   ("R" "lispy-raise-some" "Edit")
   ("\\" "lispy-splice" "Edit")
   ("S" "lispy-stringify" "Edit")
   ("i" "lispy-tab" "Edit")
   ("xj" "lispy-debug-step-in" "Eval")
   ("xe" "lispy-edebug" "Eval")
   ("xT" "lispy-ert" "Eval")
   ("e" "lispy-eval" "Eval")
   ("E" "lispy-eval-and-insert" "Eval")
   ("xr" "lispy-eval-and-replace" "Eval")
   ("p" "lispy-eval-other-window" "Eval")
   ("q" "lispy-ace-paren" "Move")
   ("z" "lispy-knight" "Move")
   ("s" "lispy-move-down" "Move")
   ("w" "lispy-move-up" "Move")
   ("t" "lispy-teleport" "Move")
   ("Q" "lispy-ace-char" "Nav")
   ("-" "lispy-ace-subword" "Nav")
   ("a" "lispy-ace-symbol" "Nav")
   ("b" "lispy-back" "Nav")
   ("d" "lispy-different" "Nav")
   ("f" "lispy-flow" "Nav")
   ("F" "lispy-follow" "Nav")
   ("g" "lispy-goto" "Nav")
   ("xb" "lispy-bind-variable" "Refactor")
   ("xf" "lispy-flatten" "Refactor")
   ("xc" "lispy-to-cond" "Refactor")
   ("xd" "lispy-to-defun" "Refactor")
   ("xi" "lispy-to-ifs" "Refactor")
   ("xl" "lispy-to-lambda" "Refactor")
   ("xu" "lispy-unbind-variable" "Refactor")
   ("M" "lispy-multiline" "Other")
   ("xh" "lispy-describe" "Other")
   ("m" "lispy-mark-list" "Other")))

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
(require 'doctest)
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
  "td"  #'doctest
  "hf"  #'helpful-callable
  "hv"  #'helpful-variable)

;;; Edebug

;; TODO: Try out a REPL while debugging by adding/removing advice in the edebug-mode-hook:
;; (defun my/ielm-edebug-eval-input (orig-func input-string &rest args)
;;   "Wrapper function for evaluating expressions in an edebug context."
;;   (apply orig-func (format "(edebug-eval-expression (quote %s))" input-string) args))
;; (advice-add 'ielm-eval-input :around #'my/ielm-edebug-eval-input)

;; TODO: Hydra for edebug-mode
