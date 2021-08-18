;; -*- lexical-binding: t; -*-
;; Emacs configuration

;; These are also in ~/.spacemacs:
;; (defconst my/emacs-dir (concat user-home-directory "code/dots/files/emacs/"))
;; (defconst my/config-dir (concat my/emacs-dir "config/"))
;; (defun my/load (path) (load-file (concat my/config-dir path)))

;; TODO Why is this necessary?
(unless (featurep 'contract)
  (load "~/code/contract.el/contract.el"))

(add-to-list 'load-path (concat my/emacs-dir "local/"))
(my/load "idle.el")
(my/load "org.el")
(my/load "functions.el")
(my/load "ui.el")
(my/load "lang/c.el")
(my/load "lang/lisp.el")
(my/load "lang/haskell.el")
(my/load "lang/souffle.el")
(my/load "projects/mate.el")
;; (my/load "eshell.el")
(my/load "tramp.el")
(my/load "shell.el")
(my/load "vterm.el")
(my/load "os/launcher.el")
(my/load "os/light.el")
;; (my/load "exwm.el")

;;; General

(setq-default tab-width 4)
(setq evil-want-abbrev-expand-on-insert-exit nil)
(setq evil-want-minibuffer t)
(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "qutebrowser")

(setq helm-ag-use-agignore t)
(setq comp-async-report-warnings-errors nil)

;; Show human-readable sizes in dired
(setq dired-listing-switches "-alh")

;; Pretty sure these are about RAM usage
(setq history-length 2048)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 50)

;; Always reload log files from disk
(add-to-list 'auto-mode-alist '("log" . auto-revert-mode))

;; https://github.com/bbatsov/projectile/issues/1174
(setq projectile-git-submodule-command "")

(my/when-idle-very-low #'marginalia-mode)

;; Set escape keybinding to "hj"
(setq-default evil-escape-key-sequence "hj")

(spacemacs/set-leader-keys
  "o SPC" #'repeat-complex-command
  ;; 'o' is the "user key"
  ;; 'b' for 'buffer'
  ;; 'r' for 'revert'
  "obr"  'my/revert-buffer-no-confirm
  "obR"  'my/revert-buffer-ansi-colors)

;; Use C-r for backwards terminal search, even in evil mode
(defun my/setup-term-mode ()
  (evil-local-set-key 'normal (kbd "<prior>") 'scroll-up)
  (evil-local-set-key 'normal (kbd "<next>") 'scroll-down)
  (evil-local-set-key 'insert (kbd "C-r") 'my/send-C-r))

(defun my/send-C-r ()
  (interactive)
  (term-send-raw-string "\C-r"))

(add-hook 'term-mode-hook 'my/setup-term-mode)

(defun my/ansi-term-zsh ()
  (interactive)
  (ansi-term "/run/current-system/sw/bin/zsh"))

;; saw-script
(use-package saw-script
  :mode "\\.saw\\'")

;; Shift-Backspace does a hungry delete
(when (require 'smart-hungry-delete)
  ;; (smart-hungry-delete-add-default-hooks) ; I'm suspicious of this.
  (global-set-key (kbd "S-<backspace>") 'smart-hungry-delete-backward-char))

(with-eval-after-load 'grep
  '(progn
     (dolist (v '("dist-newstyle/"
                  "dist/"))
       (add-to-list 'grep-find-ignored-directories v))
     (dolist (v '("*log"))
       (add-to-list 'grep-find-ignored-files v))))

(spacemacs/set-leader-keys
  ;; 'o' is the "user key"
  ;; 'p' for 'projectile'
  ;; 'a' for 'ag'
  "opa"  'my/projectile-ag-in-other-project)

(with-eval-after-load 'projectile
  (dolist (dir (list "dist" "dist-newstyle"))
    (add-to-list 'projectile-globally-ignored-directories dir)))

(when (require 'diff-hl-mode nil 'noerror)
  (global-diff-hl-mode 1))

;; TODO:
;; (defun my/message-filter (&rest args)
;;   (message "ARGS %s" args))
;; (advice-add
;;  #'message
;;  :before-while
;;  #'my/message-filter)
;; (defadvice message (around my/message-filter activate)
;;   (when-let* ((arg (ad-get-arg 0))
;;               (_ (string-match
;;                   (regexp-quote "Unable to load color \"unspecified-fg\"")
;;                   arg)))
;;     ()
;;     ))

;;; abbrevs

(add-hook 'prog-mode-hook 'abbrev-mode)
(add-hook 'text-mode-hook 'abbrev-mode)
(setq abbrev-suggest t)
(setq abbrev-suggest-hint-threshold 4)
(read-abbrev-file (concat my/emacs-dir "config/abbrevs.el"))

;;; scratch

(defun my/scratch (mode)
  (interactive
   (list
    (completing-read "Which? " '("markdown" "elisp" "fundamental"))))
  (switch-to-buffer mode)
  (cond
   ((equal mode "markdown") (markdown-mode))
   ((equal mode "elisp") (emacs-lisp-mode))
   (t (fundamental-mode))))

;;; helm-dash

;; TODO: Use actual dash layer

(with-eval-after-load 'helm-dash
  (setq helm-dash-browser-func 'eww)
  (require 'xdg)
  (setq helm-dash-docsets-path
        (concat
         (xdg-data-home)
         "/docsets"))
  (setq my/helm-dash-user-docsets
        (list
         ;; "Beautiful_Soup" ;;
         "Clang"
         ;; "Coq" ;;
         "emacs"
         ;; "GNU_Make"
         "HTTP"
         "Hy"
         ;; "Hypothesis" ;;
         ;; "IntelASMx86"
         "LLVM"
         ;; "Linux_Man_Pages"
         ;; "Org_Mode"
         ;; "R5RS" ;;
         "Requests"
         ;; "Sphinx"
         "Zsh"
         "glibc"
         "pytest"))
  (setq my/helm-dash-official-docsets
        (list
         "Haskell"
         "SQLAlchemy"
         ;; "JavaScript"
         "Python 3"
         "HTML"))

  (setq helm-dash-common-docsets
        (append my/helm-dash-official-docsets
                my/helm-dash-user-docsets)))

(defun my/helm-dash-single-docset (docset)
  (when (boundp 'dash-docs-docsets)
    (setq-local dash-docs-docsets-backup dash-docs-docsets))
  (setq-local dash-docs-docsets (list docset))
  (funcall-interactively helm-dash)
  (when (boundp 'dash-docs-docsets)
    (setq-local dash-docs-docsets dash-docs-docsets-backup)))

(defun my/helm-dash-python ()
  (interactive)
  (my/helm-dash-single-docset "Python_3"))

(defun my/helm-dash-haskell ()
  (interactive)
  (my/helm-dash-single-docset "Haskell"))

(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "ohd"  'my/helm-dash-python)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode
  "ohd"  'my/helm-dash-haskell)


;;; tramp

(with-eval-after-load 'tramp
  (defun my/tramp-buffer-hook ()
    (when (file-remote-p default-directory)
      (setq-local explicit-shell-file-name "/bin/sh")
      (setq-local shell-file-name "/bin/sh")))
  (add-hook 'buffer-list-update-hook #'my/tramp-buffer-hook))

;;; tramp git gutter

;; https://github.com/nonsequitur/git-gutter-plus/issues/42

(with-eval-after-load 'git-gutter+
  (defun git-gutter+-remote-default-directory (dir file)
    (let* ((vec (tramp-dissect-file-name file))
           (method (tramp-file-name-method vec))
           (user (tramp-file-name-user vec))
           (domain (tramp-file-name-domain vec))
           (host (tramp-file-name-host vec))
           (port (tramp-file-name-port vec)))
      (tramp-make-tramp-file-name method user domain host port dir)))

  (defun git-gutter+-remote-file-path (dir file)
    (let ((file (tramp-file-name-localname (tramp-dissect-file-name file))))
      (replace-regexp-in-string (concat "\\`" dir) "" file))))

;;; minimap

(with-eval-after-load 'minimap
  (setq minimap-minimum-width 10)
  (setq minimap-maximum-width 13)
  (setq minimap-automatically-delete-window nil)
  (add-to-list 'minimap-major-modes 'org-mode))

(custom-set-faces
 '(minimap-active-region-background ((t (:background "gray25")))))

;;; outline-toc-mode

(setq my/outline-modes (list 'org-mode 'markdown-mode))
(defun my/outline-toc-mode-window-configuration-change-hook ()
  (if (member major-mode my/outline-modes)
      (outline-toc-mode 1)
    (outline-toc-mode -1)))
(add-hook 'window-configuration-change-hook
          'my/outline-toc-mode-window-configuration-change-hook
          'append
          'global)

;;; avy

(with-eval-after-load 'avy
  ;; https://github.com/abo-abo/hydra/wiki/avy
  (defhydra my/avy-hydra (:exit t :hint nil)
    "
 Line^^       Region^^        Goto
----------------------------------------------------------
 [_y_] yank   [_Y_] yank      [_j_] timed char  [_C_] char
 [_m_] move   [_M_] move      [_w_] word        [_W_] any word
 [_k_] kill   [_K_] kill      [_l_] line        [_L_] end of line"
    ("j" avy-goto-char-timer)
    ("C" avy-goto-char)
    ("w" avy-goto-word-1)
    ("W" avy-goto-word-0)
    ("l" avy-goto-line)
    ("L" avy-goto-end-of-line)
    ("m" avy-move-line)
    ("M" avy-move-region)
    ("k" avy-kill-whole-line)
    ("K" avy-kill-region)
    ("y" avy-copy-line)
    ("Y" avy-copy-region))
  (spacemacs/set-leader-keys
    "j SPC" #'my/avy-hydra/body))

;;; dired

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  ;; https://github.com/abo-abo/hydra/wiki/Dired
  (defhydra my/dired-hydra (:hint nil :color pink)
    "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)         ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)        ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)        ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)   ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))

  (spacemacs/set-leader-keys-for-major-mode 'dired-mode
    "." #'my/dired-hydra/body)
  ;; (add-hook 'dired-mode-hook #'my/dired-hydra/body)
  )

(defun my/dired-run-shell-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "MRun shell command on marked files: ")
  (shell-command (string-join (cons command (dired-get-marked-files)) " "))
  (revert-buffer))

(defun my/dired-trash-marked ()
  (interactive)
  (my/dired-run-shell-command "trash-put"))

(defun my/dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

;;; Occur

;; https://oremacs.com/2015/01/26/occur-dwim/
(defun occur-dwim ()
  "Call `occur' with a sane default, chosen as the thing under point or selected region"
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

(with-eval-after-load 'replace
  (defhydra my/occur-hydra ()
    ("e" occur-edit-mode "edit")
    ("E" occur-cease-edit "stop editing")
    ("n" occur-next "next")
    ("p" occur-prev "previous")
    ("r" occur-rename-buffer "rename buffer")
    ("RET" occur-mode-display-occurrence "go")
    ("q" nil "quit" :exit t))
  (spacemacs/set-leader-keys-for-major-mode 'occur-mode
    "." #'my/occur-hydra/body
    "e" #'occur-edit-mode
    "E" #'occur-cease-edit
    "n" #'occur-next
    "p" #'occur-prev
    "r" #'occur-rename-buffer
    "RET" #'occur-mode-display-occurrence)
  ;; (add-hook 'occur-mode-hook #'my/occur-hydra/body)
  ;; (remove-hook 'occur-mode-hook #'my/occur-hydra/body)
  )

;;; prism

(with-eval-after-load 'prism
  (prism-set-colors
   :lightens '(0 5 10)
   :desaturations '(-2.5 0 2.5)
   :colors (-map #'doom-color
                 '(red orange yellow green blue violet))))
(add-hook 'json-mode-hook 'prism-mode)

;;; symbol-overlay

(spacemacs/set-leader-keys
  ;; 'o' is the "user key"
  ;; 's' for 'show' or 'symbol'
  ;; 'p' for 'put'
  "osp"  'symbol-overlay-put
  "osr"  'symbol-overlay-remove-all
  "osM"  'symbol-overlay-mode)

(with-eval-after-load 'symbol-overlay

  (require 'cursor-sensor)
  ;; (add-hook 'prog-mode-hook 'symbol-overlay-mode)
  (add-hook 'prog-mode-hook 'cursor-sensor-mode)

  ;; TODO: enter a transient state
  ;; Do something when the cursor is on a highlighted symbol
  (defun my/cursor-function (window oldpos entered-or-left)
    (when (equal entered-or-left 'entered)
      (message "entered")))
  (defun my/symbol-overlay-overlay-created-function (ov)
    (overlay-put ov 'cursor-sensor-functions '(my/cursor-function)))

  (when (boundp 'symbol-overlay-overlay-created-functions)
    (add-to-list 'symbol-overlay-overlay-created-functions
                 'my/symbol-overlay-overlay-created-function))

  (define-key symbol-overlay-map (kbd "g") 'symbol-overlay-jump-to-definition)
  (define-key symbol-overlay-map (kbd "?") 'symbol-overlay-map-help)

  ;; Don't override delete or word movement, it's confusing
  (define-key symbol-overlay-map (kbd "w") nil)
  (define-key symbol-overlay-map (kbd "d") nil)

  (defun saw-script-ignore-function (symbol)
    (symbol-overlay-match-keyword-list symbol saw-script-keywords))
  (defun haskell-ignore-function (symbol)
    (symbol-overlay-match-keyword-list
     symbol
     '("let" "rec" "do" "in" "where" "module" "import")))
  (add-to-list 'symbol-overlay-ignore-functions
               '(saw-script-mode . saw-script-ignore-function))
  (add-to-list 'symbol-overlay-ignore-functions
               '(haskell-mode . haskell-ignore-function)))

;;; (Ma)git

;; (add-to-list 'forge-alist '("gitlab-ext.galois.com" "gitlab-ext.galois.com/api/v4" "gitlab-ext.galois.com" forge-gitlab-repository))
(setq git-link-default-remote "upstream")
(setq magithub-clone-default-directory "~/code")
(setq magit-repository-directories '(("~/code" . 2)))
(setq auth-sources '("~/.authinfo.gpg"))
(setq magit-wip-merge-branch t)
(setq projectile-switch-project-action 'magit)
;; (add-hook 'prog-mode-hook 'magit-wip-mode)

;; Fix rebase keybindings
(with-eval-after-load 'magit
  (spacemacs/set-leader-keys-for-major-mode 'git-rebase-mode
    "C-k" 'git-rebase-move-line-down
    "C-j" 'git-rebase-move-line-down))

;;; Keybindings

;; Find good functions to put here with (list-command-history)
(spacemacs/set-leader-keys
  ;; 'o' is the "user key"
  ;; 'e' for 'elisp'
  "oev"  'eval-region
  "oeb"  'eval-buffer

  "om"   'evil-scroll-line-to-center

  "o>"   'sp-forward-slurp-sexp
  "o<"   'lispy-barf

  "glh"  'git-link-homepage

  ;; 'i' for insert
  "ois"  'math-symbols-insert

  ;; fonts
  "oF"  'text-scale-adjust

  ;; emoji
  "oEi"  'emojify-insert-emoji
  "oEd"  'emojify-describe-emoji
  "oEl"  'emojify-list-emoji

  ;; 'h' for help
  "ohd"  'helm-dash

  ;; 'x' for text
  "oxa"  'ialign-interactive-align)

;; TODO: Work in terminal?
(evil-define-key 'insert prog-mode-map
  (kbd "<S-return>") 'hippie-expand)
(evil-define-key 'normal prog-mode-map
  (kbd "RET") 'hippie-expand)

;;; Ediff

(with-eval-after-load 'ediff
  ;; https://emacs.stackexchange.com/questions/13019/how-to-undo-on-ediff-a-b-changes
  (add-hook 'ediff-mode-hook
            (lambda ()
              (progn
                ;; use undo-tree.el
                (defun my-ediff-undo-A ()
                  (interactive)
                  (save-window-excursion (with-selected-window ediff-window-A (undo-tree-undo))))
                (defun my-ediff-undo-B ()
                  (interactive)
                  (save-window-excursion (with-selected-window ediff-window-B (undo-tree-undo))))
                (defun my-ediff-redo-A ()
                  (interactive)
                  (save-window-excursion (with-selected-window ediff-window-A (undo-tree-redo))))
                (defun my-ediff-redo-B ()
                  (interactive)
                  (save-window-excursion (with-selected-window ediff-window-B (undo-tree-redo))))))))

;;; Prettify-symbols

;; https://bit.ly/2SVwzji
;; https://bit.ly/2QukKiD

(when (display-graphic-p)

  (setq prettify-symbols-unprettify-at-point t)

  (defun my/two-spaces (symb)
    `(?\s (Br . Bl) ?\s (Bc . Bc) ,symb))

  (defun my/three-spaces (symb)
    `(?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ,symb))

  ;; lexical-let lets us define a closure
  (defun mapcdr (f)
    "Map a function over the second element of a pair"
    (lexical-let ((g f))
      (lambda (p) (cons (car p) (funcall g (cdr p))))))

  (defun mapvalues (f seq &optional seqtype)
    "Map a function over the values stored in an alist"
    (lexical-let ((g f))
      (map (if seqtype seqtype 'list) (mapcdr g) seq)))

  ;; TODO: get this working
  ;; https://bit.ly/2zyR508
  ;; (defun make-spaces (el)
  ;;   (let ((space-width (string-width (car el))))
  ;;     (cons (car el)
  ;;           (append (make-list (- space-width 1) '(?\s (Br . Bl)))
  ;;                   '(?\s (Br . Br))
  ;;                   (cons (cdr el) nil)))))
  (setq comparison-prettify-symbols-alist
        (mapvalues 'my/two-spaces
                   '((">=" . ?≥)
                     ("<=" . ?≤)
                     ("==" . ?≟)
                     ("/=" . ?≠)
                     ("!=" . ?≠))))

  ;; The commented-out versions are the "short" arrows, active are "long"
  (setq arrow-prettify-symbols-alist
        (mapvalues 'my/two-spaces
                   '(("=>" . ?⟹) ;; ("=>" . ?⇒)
                     ("<=" . ?⟸)
                     ("->" . ?⟶) ;; ("->" . ?→)
                     ("<-" . ?⟵) ;; ("<-" . ?←)
                     )))

  ;; Not working :/
  ;; https://en.wikipedia.org/wiki/Percent-encoding#Percent-encoding_reserved_characters
  ;; (setq percent-encoding-prettify-symbols
  ;;       (mapvalues 'my/three-spaces
  ;;                  '(("\%20" . " ")
  ;;                    ("\%21" . "!"))))

  (setq bool-prettify-symbols-alist
        (mapvalues 'my/two-spaces
                   '(("&&" . ?∧)
                     ("||" . ?∨))))

  (setq my/prettify-symbols-mode-alist
        `((haskell-mode
           . (append
              comparison-prettify-symbols-alist
              arrow-prettify-symbols-alist
              bool-prettify-symbols-alist
              ;; (mapvalues 'my/three-spaces
              ;;            '(("<*>" . ?⊛)))
              (mapvalues 'my/two-spaces
                         '(("<|" . ?⊲)     ; sequences/application in flow
                           ("|>" . ?⊳)     ; sequences/application in flow
                           (".>" . ?⋖)     ; composition in flow
                           ("<." . ?⋗)     ; composition in flow
                           ("::" . ?∷)))))
          (org-mode
           . ,(append
               ;; percent-encoding-prettify-symbols
               comparison-prettify-symbols-alist
               arrow-prettify-symbols-alist
               bool-prettify-symbols-alist
               (mapvalues 'my/two-spaces '(("::" . ?∷)))))
          (saw-script-mode . arrow-prettify-symbols-alist)))

  (defun my/prettify-symbols ()
    (interactive)
    (prettify-symbols-mode -1)
    (let ((mode-prettify-symbols-alist
           (alist-get major-mode my/prettify-symbols-mode-alist)))
      (when (not (equal nil mode-prettify-symbols-alist))
        (setq-local prettify-symbols-alist
                    mode-prettify-symbols-alist)
        (prettify-symbols-mode))))

  (add-hook 'haskell-mode-hook 'my/prettify-symbols)
  (add-hook 'org-mode-hook 'my/prettify-symbols)
  (add-hook 'saw-script-mode-hook 'my/prettify-symbols))


;;; Elfeed

(with-eval-after-load 'elfeed
  (spacemacs|define-transient-state elfeed
    :title "Elfeed Transient State"
    :foreign-keys run
    :doc "
    ^Navigate^               ^Filtering^          ^Other
    ───────────────────────────────────────────────────────────
    [_RET_] show entry       [_s_] live filter    [_g_] update
    [_r_] mark read          [_S_] set filter     [_q_] exit transient state
    [_b_] open browser        ^ ^                 [_Q_] exit elfeed
    [_j_] next line
    [_k_] previous line"
    :bindings
    ("q" nil :exit t)
    ("Q" elfeed-search-quit-window :exit t)

    ("RET" elfeed-search-show-entry)
    ("r" elfeed-search-untag-all-unread)
    ("b" elfeed-search-browse-url)
    ("k" evil-previous-visual-line)
    ("j" evil-next-visual-line)

    ("g" elfeed-update)

    ("s" elfeed-search-live-filter)
    ("S" elfeed-search-set-filter))

  (spacemacs/set-leader-keys-for-major-mode 'elfeed-search-mode
    "." 'spacemacs/elfeed-transient-state/body)

  (add-hook 'elfeed-search-mode-hook
            #'spacemacs/elfeed-transient-state/body)
  (add-hook 'elfeed-search-mode-hook
            #'spacemacs/toggle-centered-point-on)

  ;; (add-to-list 'elfeed-search-face-alist '(haskell (:foreground "red")))
  )

;; https://github.com/iqbalansari/dotEmacs/blob/master/config/rss.org
;; (defun my/elfeed-open-in-w3m ()
;;   (interactive)
;;   (let ((browse-url-browser-function (cond ((locate-library "w3m") #'w3m-browse-url)
;;                                            ((locate-library "eww") #'eww-browse-url)
;;                                            (t (progn (message "`w3m' not installed, falling back to system browser")
;;                                                      #'browse-url-default-browser)))))
;;     (elfeed-search-untag-all 'unread)
;;     (save-excursion
;;       (forward-line -1)
;;       (elfeed-search-browse-url))))

;; (defun my/elfeed-mode-kbd ()
;;   (progn
;;     (require 'elfeed)
;;     (define-key elfeed-search-mode-map (kbd "3") 'my/elfeed-open-in-w3m)))

;; TODO: is this the right mode hook?
;; (require 'elfeed

;;; Klister

(with-eval-after-load 'klister-mode
  (setq klister-command "../dist-newstyle/build/x86_64-linux/ghc-8.6.5/klister-0.1/x/klister-repl/build/klister-repl/klister-repl")
  (spacemacs/set-leader-keys-for-major-mode 'klister-mode
    "c"  'klister-run-current-buffer))

;;; C/C++
;;; Rust

(with-eval-after-load 'rust
  ;; Align EOL comments
  (add-to-list 'align-rules-list
               '(rust-eol-comment
                 (regexp . "//")
                 (modes . rust-mode)))
  ;; Align match
  (add-to-list 'align-rules-list
               '(rust-match
                 (regexp . "=>")
                 (modes . rust-mode))))

;;; Java

;; Formatting really screws things up in weird ways.
;; e.g.: https://github.com/emacs-lsp/lsp-java/issues/75
(with-eval-after-load 'java
  (progn
    (setq path-to-lombok (concat (getenv "HOME") "/.m2/repository/org/projectlombok/lombok/1.18.10/lombok-1.18.10.jar"))
    (setq dap-java-java-command (concat "java " (concat "-javaagent:" path-to-lombok) " "
                                        (concat "-Xbootclasspath/a:" path-to-lombok)))
    (setq lsp-java-format-comments-enabled nil)))

;; https://github.com/emacs-lsp/lsp-java/issues/26
;; (setq path-to-lombok (concat (getenv "HOME") "/.m2/repository/org/projectlombok/lombok/1.18.10/lombok-1.18.10.jar"))
;; (setq lsp-java-vmargs
;;       (list
;;        "-noverify"
;;        "-Xmx1G"
;;        "-XX:+UseG1GC"
;;        "-XX:+UseStringDeduplication"
;;        (concat "-javaagent:" path-to-lombok)
;;        (concat "-Xbootclasspath/a:" path-to-lombok)))

;;; Scala

(with-eval-after-load 'scala
  (setq ensime-startup-notification nil)
  (defun my/sbt-hydra-hook ()
    (when (equal major-mode 'sbt-mode)
      (sbt-hydra)))
  (add-hook 'focus-in-hook 'my/sbt-hydra-hook)
  (spacemacs/set-leader-keys-for-major-mode 'scala-mode
    "ee" 'ensime-print-errors-at-point))

;;; Python

(with-eval-after-load 'python
  (setq lsp-python-ms-executable (executable-find "pyls"))
  (with-eval-after-load 'lsp
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-tramp-connection "pyls")
      :major-modes '(python-mode)
      :remote? t
      :server-id 'pyls-remote))))

;;; Agda

(when (require 'agda2-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode)))

;;; Coq

;; (with-eval-after-load 'company-coq
;;    (add-to-list 'company-coq-disabled-features 'prettify-symbols))

;; Coq key bindings:
;; - Use SPC-m-SPC instead of SPC-m-. for less pinky strain
;; - Use SPC-gE to process to the end of the file. Good for finding out where one left-arrow off.
(spacemacs/set-leader-keys-for-major-mode 'coq-mode
  "gE"  '(lambda () (interactive) (progn (evil-goto-line) (proof-goto-point)))
  "SPC" 'proof-goto-point
  "miH" 'my-surround-region-hide)
(with-eval-after-load 'coq
  (setq coq-one-command-per-line nil) ; https://github.com/ProofGeneral/PG/issues/53
  (setq proof-electric-terminator-enable t)) ; https://github.com/ProofGeneral/PG/issues/53

(with-eval-after-load 'coq
  ;; surround a region with a hide command for coqdoc
  (defun my/surround-region-hide ()
    (interactive)
    (progn
      (goto-char (region-end))
      (insert "(* end hide *)")
      (goto-char (region-beginning))
      (insert "(* begin hide *)\n")))

  ;; undo-tree and proof-general do NOT work together, this makes the breakage more tolerable
  (setq undo-tree-enable-undo-in-region nil)

  ;; Cobbled together from:
  ;; https://github.com/abo-abo/helm-make/blob/e72cdacecb46421dfbde9febdc352a5f06425176/helm-make.el#L377
  ;; https://github.com/ProofGeneral/PG/blob/c54b17d14e57eb77093ec0006d2db5e9e3754902/coq/coq.el#L1800
  ;; TODO: n is void?
  (defun my/coq-compile-projectile ()
    (interactive)
    (require 'projectile)
    (let ((makefile (concat (projectile-project-root) "/Makefile"))
          (n (buffer-name))
          (l (string-match ".v" n)))
      (if (not makefile)
          (error "No build file found for project %s" (projectile-project-root)))
      (compile (concat "make " (substring n 0 l) ".vo")))))


;;; Shell

(add-hook 'sh-mode-hook '(lambda () (setq sh-basic-offset 2
                                          sh-indentation 2)))


;;; LaTeX

(with-eval-after-load 'latex
  (add-hook 'doc-view-mode-hook 'auto-revert-mode) ; see latex layer docs
  ;; (spacemacs/set-leader-keys-for-major-mode 'TeX-mode "mim" 'math-symbols-helm-insert-char)

  ;; make backslash easier in TeX, use Agda input mode
  ;; TODO: make backslash semicolon
  (add-hook 'org-mode-hook   '(lambda () (local-set-key (kbd ";") "\\")))
  (add-hook 'TeX-mode-hook   '(lambda () (local-set-key (kbd ";") "\\")))
  (add-hook 'LaTeX-mode-hook '(lambda () (local-set-key (kbd ";") "\\")))
  (setq TeX-view-program-selection '((output-pdf "Zathura"))))

;;; Mutt

(defun my/msg-mode ()
  ;; Map j/k to move visual lines.
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
  (visual-line-mode)
  )
(add-hook 'message-mode-hook 'msg-mode)
(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))

;;; eacl

(eval-after-load 'grep
  '(progn
     (dolist (v '("node_modules"
                  "bower_components"
                  ".sass_cache"
                  ".cache"
                  ".npm"
                  ".out"
                  ".git"
                  "__pycache__"))
       (add-to-list 'grep-find-ignored-directories v))
     (dolist (v '("*.min.js"
                  "*.bundle.js"
                  "*.min.css"
                  "*.json"
                  "*.bc"
                  "*.log"))
       (add-to-list 'grep-find-ignored-files v))))

;; This doesn't work as a 'let' binding??
;; TODO: only consider files of the appropriate suffices
(eval-when-compile
  (defsubst my/eacl-complete (s)
    (insert s)
    (eacl-complete-line)))

(defun my/smart-insert-import ()
  (interactive)
  (evil-open-below 1)
  (evil-beginning-of-line)
  (cond ((eq major-mode #'haskell-mode) (my/eacl-complete "import          "))
        ((eq major-mode #'emacs-lisp-mode) (my/eacl-complete "\\(require "))
        ((eq major-mode #'scala-mode) (my/eacl-complete "import "))
        ((eq major-mode #'java-mode) (my/eacl-complete "import "))
        ((eq major-mode #'python-mode) (my/eacl-complete "import "))
        ((eq major-mode #'coq-mode) (my/eacl-complete "Require "))
        ((eq major-mode #'rust-mode) (my/eacl-complete "use "))
        ((eq major-mode #'c++-mode) (call-interactively 'my/c++-include))
        ((eq major-mode #'c-mode) (call-interactively 'my/libc-include))))

;; 'i' for 'insert'
;; 'i' for 'import'
(spacemacs/set-leader-keys-for-major-mode 'c-mode
  "ii" 'my/smart-insert-import)
(spacemacs/set-leader-keys-for-major-mode 'c++-mode
  "ii" 'my/smart-insert-import)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode
  "ii" 'my/smart-insert-import)
(spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode
  "ii" 'my/smart-insert-import)
(spacemacs/set-leader-keys-for-major-mode 'coq-mode
  "ii" 'my/smart-insert-import)
(spacemacs/set-leader-keys-for-major-mode 'scala-mode
  "ii" 'my/smart-insert-import)
(spacemacs/set-leader-keys-for-major-mode 'java-mode
  "ii" 'my/smart-insert-import)
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "ii" 'my/smart-insert-import)

;;; Helpful

(spacemacs/set-leader-keys
  "hdf"  #'helpful-callable
  "hdv"  #'helpful-variable)

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
