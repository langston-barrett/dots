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

(with-eval-after-load 'edebug
  (defhydra my/edebug-hydra (:hint t :foreign-keys run)
    ("q" nil "quit")
    ("b" #'edebug-backtrace "backtrace" :column "common")
    ("-" #'negative-argument "neg argument" :column "common")

    ;; breaking
    ("I" #'edebug-instrument-callee "instrument callee" :column "break")
    ("x" #'edebug-set-breakpoint "set breakpoint" :column "break")
    ("X" #'edebug-unset-breakpoint "unset breakpoint" :column "break")
    ("N" #'edebug-next-breakpoint "next breakpoint" :column "break")
    ("c" #'edebug-set-conditional-breakpoint "conditional bp" :column "break")
    ("C" #'edebug-set-global-break-condition "global conditional bp"
     :column "break")

    ;; navigation
    ("w" #'edebug-where "where" :column "common")
    ("z" #'edebug-bounce-point "bounce point" :column "common")

    ;; stepping
    ("h" #'edebug-goto-here "continue until point" :column "step")
    ("s" #'edebug-stop "stop" :column "step")
    ("o" #'edebug-step-out "step out" :column "step")
    ("i" #'edebug-step-in "step in" :column "step")
    ("f" #'edebug-forward "forward" :column "step")

    ;; sexp oriented
    ("l" #'edeug-forward-sexp "forward sexp" :column "sexp")
    ("e" #'edebug-eval-expression "eval expression" :column "sexp")
    ("E" #'edebug-eval-last-sexp "eval expression" :column "sexp")
    ("r" #'edebug-previous-result "previous result" :column "sexp")
    (";" #'edebug-visit-eval-list "visit eval list" :column "sexp")

    ;; exiting
    ("Q" #'edebug-top-level-nonstop "toplevel non stop" :column "common")
    ("S" #'edebug-stop "edebug stop" :column "common")

    ;; modes
    ("1" #'edebug-Go-nonstop-mode "go nonstop" :column "modes")
    ("2" #'edebug-go-mode "go until break" :column "modes")
    ("3" #'edebug-step-mode "step mode" :column "modes")
    ("4" #'edebug-next-mode "next mode" :column "modes")
    ("5" #'edebug-continue-mode "continue" :column "modes")
    ("6" #'edebug-Continue-fast-mode "continue fast" :column "modes")
    ("7" #'edebug-trace-mode "trace" :column "modes")
    ("8" #'edebug-Trace-fast-mode "trace fast" :column "modes")))

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
