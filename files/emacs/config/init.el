;; Emacs configuration
;;; General

(setq-default tab-width 4)

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "qutebrowser")

;; Always reload log files from disk
(add-to-list 'auto-mode-alist '("log" . auto-revert-mode))

;; https://github.com/bbatsov/projectile/issues/1174
(setq projectile-git-submodule-command "")

;; fonts
;; http://ergoemacs.org/emacs/emacs_list_and_set_font.html
;; (when (member "Symbola" (font-family-list))
;;   (set-fontset-font t 'unicode "Symbola" nil 'prepend))
;; (when (member "Noto Emoji" (font-family-list))
;;   (set-fontset-font t 'unicode "Noto Emoji" nil 'prepend))

;; Set escape keybinding to "hj"
(setq-default evil-escape-key-sequence "hj")
;; (define-key evil-motion-state-map "j" 'evil-backward-char)
;; (define-key evil-motion-state-map ";" 'evil-forward-char)
;; (define-key evil-motion-state-map "k" 'evil-next-line)
;; (define-key evil-motion-state-map "l" 'evil-previous-line)

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun my/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))
(defun my/revert-buffer-ansi-colors ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm)
  (my/display-ansi-colors))

(spacemacs/set-leader-keys
  ;; 'o' is the "user key"
  ;; 'b' for 'buffer'
  ;; 'r' for 'revert'
  "obr"  'my/revert-buffer-no-confirm
  "obR"  'my/revert-buffer-ansi-colors)

(setq history-length 100)
(put 'minibuffer-history 'history-length 50)
(put 'evil-ex-history 'history-length 50)
(put 'kill-ring 'history-length 25)

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

(defun my/increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (unless (looking-at "[0-9]+") (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(setq helm-ag-use-agignore t)

;; bigger fringes
(setq-default left-fringe-width 16)
(setq-default right-fringe-width 16)
;; fake header line for space between top of frame and buffer text
(setq header-line-format " ")

;; saw-script
(add-to-list 'load-path "/home/siddharthist/.config/spacemacs/private/local/")
(use-package saw-script
  :mode "\\.saw\\'")

;; https://bit.ly/2OrfDO0
;; (when (require 'hungry-delete nil 'noerror)
;;   (global-hungry-delete-mode))

;; Shift-Backspace does a hungry delete
(when (require 'smart-hungry-delete)
  ;; (smart-hungry-delete-add-default-hooks) ; I'm suspicious of this.
  (global-set-key (kbd "S-<backspace>") 'smart-hungry-delete-backward-char))

(when (require 'evil-fringe-mark nil 'noerror)
  (global-evil-fringe-mark-mode))

(defun my/number-at-point-to-hex ()
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'word))
            (front (car bounds))
            (back (cdr bounds))
            (num (string-to-number (buffer-substring-no-properties front back))))
      (delete-region front back)
      (insert "0x")
      (insert (format "%.6x" num)))))

(defun my/find-replace-hex ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp (rx (+ digit)))
      (goto-char (- (point) 1))
      (my/number-at-point-to-hex)
      (forward-word))))

(when (require 'dimmer nil 'noerror)
  (setq dimmer-fraction 0.03)
  (setq dimmer-adjustment-mode :background)
  (dimmer-mode))

(with-eval-after-load 'golden-ratio-mode
  (when (boundp 'golden-ratio-exclude-modes)
    (add-to-list 'golden-ratio-exclude-modes "pdf-view-mode"))
  (golden-ratio-mode)
  (setq golden-ratio-adjust-factor .90
        golden-ratio-wide-adjust-factor .90))

(with-eval-after-load 'grep
  '(progn
      (dolist (v '("dist-newstyle/"
                  "dist/"))
        (add-to-list 'grep-find-ignored-directories v))
      (dolist (v '("*log"))
        (add-to-list 'grep-find-ignored-files v))))

(defun my/projectile-ag-in-other-project ()
  (interactive)
  (setq bak projectile-switch-project-action)
  (setq projectile-switch-project-action '(lambda () (call-interactively 'helm-projectile-ag)))
  (projectile-switch-project)
  (setq projectile-switch-project-action bak))

(spacemacs/set-leader-keys
  ;; 'o' is the "user key"
  ;; 'p' for 'projectile'
  ;; 'a' for 'ag'
  "opa"  'my/projectile-ag-in-other-project)

;; (with-eval-after-load 'darkroom
;;   (setq darkroom-text-scale-increase 0.5)
;;   (add-hook 'markdown-mode-hook 'darkroom-tentative-mode))

(with-eval-after-load 'projectile
  (dolist (dir (list "dist" "dist-newstyle"))
    (add-to-list 'projectile-globally-ignored-directories dir)))

(when (require 'diff-hl-mode nil 'noerror)
  (global-diff-hl-mode 1))

(defun my/display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))


;;; spaceline

(setq powerline-scale 1.0)
(setq spaceline-purpose-p nil)
(setq spaceline-major-mode-p nil)
(setq spaceline-minor-modes-p nil)
(setq spaceline-buffer-size-p nil)
(setq spaceline-buffer-position-p nil)
(setq spaceline-buffer-encoding-abbrev-p nil)

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

;;; font size

(defun my/compute-font-size (frame)
  "Inspired by https://emacs.stackexchange.com/a/44930/17066. FRAME is ignored."
  ;; Using display names is unreliable...switched to checking the resolution
  (let* ((attrs (frame-monitor-attributes)) ;; gets attribs for current frame
          (width-mm (nth 1 (nth 3 attrs)))
          (width-px (nth 3 (nth 2 attrs)))
          (size nil)) ;; default size
    (cond ((eq width-px 1920) (setq size 8))
          ((eq width-px 2560) (setq size 8))
          (t (setq size 12)))
    (set-frame-font (format "Hack %s" size))))

(defun my/recompute-font-size ()
  (interactive)
  (my/compute-font-size nil))

(defun my/frame-width-px ()
  (interactive)
  (message "%d"
    (let* ((attrs (frame-monitor-attributes))
          (width-px (nth 3 (nth 2 attrs))))
      width-px)))


(add-hook 'window-size-change-functions #'my/compute-font-size)

;;; minimap

(with-eval-after-load 'minimap
  (setq minimap-minimum-width 10)
  (setq minimap-maximum-width 13)
  (setq minimap-automatically-delete-window nil)
  (add-to-list 'minimap-major-modes 'org-mode))

(custom-set-faces
  '(minimap-active-region-background ((t (:background "gray25")))))

;;; speedbar/sb-imenu

;; (when (require 'sb-imenu nil 'noerror)
;;   (setq speedbar-initial-expansion-list-name "sb-imenu")
;;   (defun my/speedbar-buffer-change-hook ()
;;     (sb-imenu-refresh))
;;   (add-hook 'buffer-list-update-hook #'my/speedbar-buffer-change-hook))

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

;;; centered-window-mode

(with-eval-after-load 'centered-window-mode
  (setq cwm-centered-window-width 100))

;;; olivetti

(setq olivetti-body-width 100)
(when (require 'olivetti nil 'noerror)

  (require 'dash)

  (setq my/excluded-buffer-names
        (list " *OUTLINE-TOC*" "*SPEEDBAR*" "*Help*" "*org-roam*"))
  (setq my/excluded-buffer-prefixes
        (list "org-sidebar" "<tree>" "HELM"))

  (defun my/excluded-windows-filter (window)
    "Return true for any window that should be excluded"
    (or (member (buffer-name (window-buffer window)) my/excluded-buffer-names)
        (-any (lambda (prefix) (equal (+ 1 (length prefix))
                                      (with-temp-buffer
                                        (insert (buffer-name (window-buffer window)))
                                        (goto-char 0)
                                        (search-forward prefix 100 t))))
              my/excluded-buffer-prefixes)))

  (defun my/count-windows ()
    (length
      (-filter
      (lambda (window) (not (my/excluded-windows-filter window)))
      (window-list-1))))

  ;; Enter olivetti mode whenever there is only one window (horizontally)
  (defun my/single-window-mode-on ()
    (interactive)
    (setq olivetti-body-width
          (if (eq major-mode 'org-mode) 100 100))
    (olivetti-mode 1)
    (spacemacs/toggle-fill-column-indicator-on)
    (spacemacs/toggle-truncate-lines-off))

  (defun my/single-window-mode-off ()
    (interactive)
    (olivetti-mode -1)
    (spacemacs/toggle-fill-column-indicator-off)
    (spacemacs/toggle-truncate-lines-off))

  (defun my/window-configuration-change-hook ()
    (set-window-margins (selected-window) 2 2)
    (if (and (= (my/count-windows) 1) (> (window-body-width) olivetti-body-width))
        (my/single-window-mode-on)
        (my/single-window-mode-off)))

  ;; Stolen from darkroom
  (define-minor-mode olivetti-tentative-mode
    "Enters `olivetti-mode' when all other windows are deleted."
    nil                                 ; init-value
    nil                                 ; lighter
    nil                                 ; keymap
    ;; always begin by removing the hook
    (remove-hook 'window-configuration-change-hook
                  'my/window-configuration-change-hook
                  'global)
    (when olivetti-mode
      (display-warning
      'olivetti
      (concat "Turning off `olivetti-mode' first. "
              "It doesn't go with `olivetti-tentative-mode'.")
      (let ((olivetti-tentative-mode nil))
        (olivetti-mode -1))))
    ;; turn olivetti on or off according to window state
    ;;
    (if olivetti-tentative-mode
        ;; re-add the hook when we are turning ourselves on
        (progn
          (add-hook 'window-configuration-change-hook
                    'my/window-configuration-change-hook
                    'append
                    'global)
          ;; call this right away if we're supposed to turn olivetti on
          ;; immediately.
          (my/window-configuration-change-hook))
      (my/single-window-mode-off))))

(define-global-minor-mode
  global-olivetti-tentative-mode
  olivetti-tentative-mode
  olivetti-tentative-mode)

(global-olivetti-tentative-mode)

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

;;; eshell

(with-eval-after-load 'eshell
  (setq eshell-prompt-function
        (lambda ()
          (concat
            "┌─["
            "]──["
            (propertize (format-time-string "%H:%M" (current-time)) 'face `(:foreground "yellow"))
            "]──["
            (replace-regexp-in-string "^/home/my/" "~/" (eshell/pwd))
            "]\n"
            "└─"
            (if (= (user-uid) 0) "# " "> ")
            )))

  (setq eshell-aliases-file "~/.emacs.d/eshell/alias") ; TODO: change this

  (if (boundp 'eshell-visual-commands)
      (add-to-list 'eshell-visual-commands "ghcid")
    (setq eshell-visual-commands
          '("ghcid" "matterhorn" "mpw" "less" "more" "top" "htop")))


  ;; Use C-r for backwards terminal search, even in evil mode
  (defun my/eshell-hook ()
    (when (require 'helm-shell-history nil 'noerror)
      (spacemacs/set-leader-keys-for-major-mode 'eshell-mode
        "h"  'helm-shell-history))
    (evil-local-set-key 'insert (kbd "C-r") 'helm-eshell-history))
  (add-hook 'eshell-mode-hook 'my/eshell-hook))

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
    (evil-local-set-key 'normal "p" 'vterm-yank))
  (add-hook 'vterm-mode-hook 'my/vterm-mode-hook))

;;; cheatsheet

(use-package cheatsheet
  :commands cheatsheet-show
  :init
  (spacemacs/set-leader-keys
    ;; 'o' is the "user key"
    ;; 'C' for 'cheatsheet'
    "oC"  'cheatsheet-show)
  (require 'my-cheatsheet))

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

(evil-define-key 'insert prog-mode-map
  (kbd "<S-return>") 'hippie-expand)

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
            'spacemacs/elfeed-transient-state/body)
  (add-hook 'elfeed-search-mode-hook
            'spacemacs/toggle-centered-point-on)

  ;; (add-to-list 'elfeed-search-face-alist '(haskell (:foreground "red")))

  (defun my/elfeed-hydra-hook ()
    (when (equal major-mode 'elfeed-search-mode)
      (spacemacs/elfeed-transient-state/body)))
  (add-hook 'focus-in-hook 'my/elfeed-hydra-hook))

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

;;; Org

;; "This variable needs to be set before org.el is loaded."
(setq org-export-backends '(ascii html md latex))
(setq org-adapt-indentation nil)

(when (require 'org-link-minor-mode nil 'noerror)
  (add-hook 'prog-mode-hook 'org-link-minor-mode)
  (defun my/no-org-links () (org-link-minor-mode -1))
  (add-hook 'sh-mode-hook 'my/no-org-links))

(setq org-indent-indentation-per-level 2)
(when (require 'org-indent-mode nil 'noerror)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-indent-indentation-per-level 2))

(setq org-directory "~/org")
(setq org-wiki-location org-directory
      org-default-notes-file (concat org-directory "/todo.org"))

(with-eval-after-load 'org-brain
  (defun aa2u-buffer () (aa2u (point-min) (point-max)))
  (setq org-brain-show-resources nil
        org-brain-path "~/org"))

(with-eval-after-load 'org

  ;; variable-pitch-mode
  ;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'org-pretty-table-mode)
  (setq org-hide-emphasis-markers t)

  (set-face-attribute 'variable-pitch nil :family "Open Sans" :inherit '(default))
  ;; (set-face-attribute 'variable-pitch nil :family "DejaVu Sans" :inherit '(default))
  ;; (set-face-attribute 'variable-pitch nil :family "Lato" :inherit '(default))

  ;; https://stackoverflow.com/questions/3758139/variable-pitch-for-org-mode-fixed-pitch-for-tables#3761399
  (defun my/adjoin-to-list-or-symbol (element list-or-symbol)
    (let ((list (if (not (listp list-or-symbol))
                    (list list-or-symbol)
                  list-or-symbol)))
      (require 'cl-lib)
      (cl-adjoin element list)))

  (mapc
    (lambda (face)
      (set-face-attribute
      face nil
      :inherit
      (my/adjoin-to-list-or-symbol
        'fixed-pitch
        (face-attribute face :inherit))))
    (list 'org-code
          'org-link
          'org-block
          'org-table
          'org-block-begin-line
          'org-block-end-line
          'org-meta-line
          'org-document-info-keyword))
  ;; (let ((font (cond
  ;;              ((x-list-fonts "Overpass") "Overpass")
  ;;              ((x-list-fonts "Lucida") "Lucida")
  ;;              ((x-list-fonts "Libre Caslon Text") "Libre Caslon Text")
  ;;              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro.")))))
  ;;   (mapc
  ;;    (lambda (face) (set-face-attribute face nil :font font))
  ;;    (list 'org-level-1
  ;;          'org-level-2
  ;;          'org-level-3
  ;;          'org-level-4
  ;;          'org-level-5
  ;;          'org-level-6
  ;;          'org-level-7
  ;;          'org-level-8)))

  (defun my/org-insert-url-from-kill-ring ()
    (interactive)
    (save-excursion
      (org-insert-property-drawer)
      (org-set-property "URL" (current-kill 0))))
  (spacemacs/set-leader-keys-for-major-mode 'org
    "iu" 'my/org-insert-url-from-kill-ring)

  ;; TODO: More
  ;; (setq org-refile-targets
  ;;       (maplist
  ;;        (lambda (item)
  ;;          `(,(concat org-directory item) . (:regexp "Cards")))
  ;;        (list "arm.org" "x86.org")))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)
                              ("~/org/todo.org" :maxlevel . 3)))
  ;; TODO: This makes it not show all the options?
  ;; (setq org-refile-use-outline-path nil)

;;;; capture

  (setq org-todo-keywords
        '((sequence "TODO(t)" "ACTIVE(a!)" "WAIT(w!/!)" "|" "DONE(!)")
          (sequence "|" "CANCELLED(c!/!)")))

  (defun my/hide ()
    (interactive)
    (org-cycle-hide-drawers 'all))
  (add-hook 'org-mode-hook 'my/hide)

  (setq meta-org (concat org-directory "/meta.org"))
  (setq vocab-org (concat org-directory "/vocab.org"))
  (setq cards-org (concat org-directory "/cards.org"))
  (setq org-capture-templates
        (doct '(("todo"
                  :keys "t"
                  :file org-default-notes-file
                  :hook my/hide
                  :template ("* TODO [#%^{Priority|C|A|B|C}] %?"
                            ":PROPERTIES:"
                            ":CREATED: %U"
                            ":CREATED_IN_FILE: %a"
                            ":END:")
                  :children (("unsorted"
                              :headline "Unsorted"
                              :keys "t")
                            ("work"
                              :keys "w"
                              :headline "Work"
                              :children (("work"
                                          :keys "w"
                                          :headline "General")
                                        ("jedi"
                                          :keys "j"
                                          :headline "Jedi")
                                        ("underconstrained"
                                          :keys "u"
                                          :headline "Underconstrained Symbolic Execution IR&D")
                                        ("chess"
                                          :keys "c"
                                          :headline "CHESS")
                                        ("vulnerability triage"
                                          :keys "v"
                                          :headline "Vulnerability Triage")
                                        ))
                            ("personal"
                              :keys "p"
                              :headline "Generic")
                            ("dots"
                              :keys "d"
                              :headline "Dots")
                            ("meta"
                              :keys "m"
                              :headline "Meta")))
                ("pending"
                  :keys "p"
                  :file org-default-notes-file
                  :headline "Pending"
                  :hook my/hide
                  ;; TODO: Only grab X clipboard if it's a link
                  :template ("* TODO [[%x][Check status]]"
                            "DEADLINE: %t"
                            ":PROPERTIES:"
                            ":CREATED: %U"
                            ":CREATED_IN_FILE: %a"
                            ":END:"))
                ("email"
                  :keys "e"
                  :file org-default-notes-file
                  :headline "Email"
                  :hook my/hide
                  :template ("* TODO [#%^{Priority|C|A|B|C}] [[][%{verb} email about %?]]"
                            ":PROPERTIES:"
                            ":CREATED: %U"
                            ":CREATED_IN_FILE: %a"
                            ":END:")
                  :children (("reply"
                              :keys "r"
                              :verb "Reply to")
                            ("compose"
                              :keys "c"
                              :verb "Compose")
                            ("read"
                              :keys "e"
                              :verb "Read")))
                ("anki"
                  :keys "a"
                  :hook my/hide
                  :children (("basic"
                            :keys "b"
                            :file cards-org
                            :headline "Cards"
                            :template ("* %U"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":CREATED_IN_FILE: %a"
                                      ":ANKI_DECK: Default"
                                      ":ANKI_NOTE_TYPE: Basic"
                                      ":END:"
                                      "** Front"
                                      "%?"
                                      "** Back"
                                      ""))
                            ("cloze"
                            :keys "c"
                            :file cards-org
                            :headline "Cards"
                            :template ("* %U"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":CREATED_IN_FILE: %a"
                                      ":ANKI_DECK: Default"
                                      ":ANKI_NOTE_TYPE: Cloze"
                                      ":END:"
                                      "** Text"
                                      "%?"
                                      ""))
                            ("vocab"
                            :keys "v"
                            :file vocab-org
                            :headline "Vocab"
                            :hook my/hide
                            :template ("* %?"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":CREATED_IN_FILE: %a"
                                      ":ANKI_DECK: Default"
                                      ":ANKI_NOTE_TYPE: Basic"
                                      ":ANKI_TAGS: vocab"
                                      ":END:"
                                      "** Front"
                                      "Define:"
                                      "** Back"
                                      "Wiktionary:")))))))


;;;; org-gcal

  (setq org-gcal-file-alist
        '(("langston@galois.com" .  "~/org/calendars/galois.org")
          ("bbarrett909@gmail.com" . "~/org/calendars/personal.org")))

;;;; publish

  (defun my/org-publish-to-markdown (plist filename pub-dir)
    "A variant of `org-html-publish-to-html' for publishing markdown."
    (org-publish-org-to 'md filename
                        (concat "." (or (plist-get plist :markdown-extension)
                                        "md"))
                        plist pub-dir))

;;;; roam

  ;; "This variable needs to be set before any calls to Org-roam functions,
  ;; including enabling org-roam-mode."
  (setq org-roam-directory org-directory)

  (with-eval-after-load 'org-roam
    (require 's)

    (setq my/org-header
          (concat
            "#+TITLE: ${title}\n"
            "#+SETUPFILE: meta/setup.org\n"
            "#+BRAIN_PARENTS:\n"
            "#+BRAIN_CHILDREN:\n"
            "#+BRAIN_FRIENDS:\n"))

    (setq org-roam-capture-templates
          `(("d" "default" plain (function org-roam--capture-get-point)
              "\n* ${title}\n\n%?"
              :file-name "%(s-replace \"_\" \"-\" \"${slug}\")"
              :head ,my/org-header
              :unnarrowed t)))

    ;; For some reason, this makes the agenda very expensive:
    (setq org-roam-link-use-custom-faces nil))

;;;; noter

;;;; sidebar

  (setq org-sidebar-tree-side 'right)
  (defun my/org-sidebar-window-configuration-change-hook ()
    (if (equal major-mode 'org-mode)
        (org-sidebar-tree)
        (org-sidebar-tree-toggle)))
  ;; (add-hook 'window-configuration-change-hook
  ;;           'my/org-sidebar-window-configuration-change-hook
  ;;           'append
  ;;           'global)

;;;; archiving

  (defun my/org-archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (when (not (string-match-p (regexp-quote "archive") buffer-file-name))
      (org-map-entries 'org-archive-subtree "/DONE" 'file)
      (org-map-entries 'org-archive-subtree "/CANCELLED" 'file)))

  (defun my/archive-org-mode-hook ()
    (add-hook 'after-save-hook 'my/org-archive-done-tasks nil t))
  (add-hook 'org-mode-hook 'my/archive-org-mode-hook)
  (setq org-archive-location "~/org/archive/%s::")

;;;; agenda

  (with-eval-after-load 'org-agenda
    (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
      "."  'spacemacs/org-agenda-transient-state/body
      "tc" 'org-agenda-columns)

    (setq org-agenda-hide-tags-regexp (rx (* anything)))

    (setq org-agenda-files (list (concat org-directory "/practices.org")
                                  (concat org-directory "/todo.org")))

    ;; TODO move to myorg layer
    (require 'langston-org)

    (setq
      org-agenda-prefix-format
      '((agenda . " %i %-5e %?-12t% ")
        (todo . " %-5e %?-12t%?s") ;; this one applies to super-agenda
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

    ;; (setq org-agenda-remove-times-when-in-prefix nil)
    (setq org-columns-default-format-for-agenda
          "%40ITEM %3PRIORITY %TODO %5EFFORT %17DEADLINE"))

;;;; org-ref

  (setq reftex-default-bibliography (list (concat org-directory "/bib/references.bib")))

  ;; see org-ref for use of these variables
  (setq org-ref-bibliography-notes (concat org-directory "/bibliography.org")
        org-ref-default-bibliography reftex-default-bibliography
        org-ref-pdf-directory (concat org-directory "/bib/"))

  (setq bibtex-completion-bibliography reftex-default-bibliography
        bibtex-completion-library-path org-ref-pdf-directory
        bibtex-completion-notes-path org-ref-bibliography-notes)

  (with-eval-after-load 'org-ref
    (spacemacs/set-leader-keys
      "orr" 'org-ref
      "orn" 'org-ref-open-notes-at-point))

;;;; latex

  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines))

;;;; ob

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((coq . t)
      (dot . t)
      (emacs-lisp . t)
      (haskell . t)
      (java . t)
      (plantuml . t)
      (python . t)
      (shell . t)
      (sql . t)))

;;;; spellchecking

  (defun my/spell-check-all-org-files ()
    (interactive)
    (-each (org-roam--list-files org-roam-directory)
      (save-excursion
        (lambda
          (file)
          (find-file file)
          (goto-char 0)
          (org-mode)
          (flyspell-buffer)
          (while (not (equal (point) (point-max)))
            (flyspell-goto-next-error)
            (flyspell-correct-at-point))
          (save-buffer)
          (kill-this-buffer)))))

;;;; misc

  (when (require 'emojify nil 'noerror)
    (add-hook 'org-mode-hook #'emojify-mode))

  (defun my/org-mode-hook ()

    (when (require 'agda-input nil 'noerror) (set-input-method "Agda"))
    ;; (add-hook 'after-save-hook
    ;;           '(lambda () (progn (org-babel-tangle)
    ;;                              (message "%s tangled" buffer-file-name))))

    ;; (add-hook 'org-babel-post-tangle-hook
    ;;           '(lambda () (indent-region (point-min) (point-max))))

    ; (fci-mode nil) ; Disable fill column indicator, it looks hectic
    (setq fill-column 80) ; Set fill column to 80
    (auto-fill-mode t) ; Automatic line wrapping at fill column

    (add-to-list 'write-file-functions 'delete-trailing-whitespace))

  (add-hook 'org-mode-hook 'my/org-mode-hook)

  (setq org-link-file-path-type 'relative)
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively   t
        org-src-tab-acts-natively  t)

;;;; clock

  (setq org-clock-persist t)
  (setq org-clock-history-length 20)
  (setq org-agenda-clock-consistency-checks
        (quote (:max-duration "6:00"
                              :min-duration 0
                              :max-gap 2
                              :gap-ok-around ("4:00"))))

  ;; Add CREATED property (https://bit.ly/2OyePrR)
  (defvar org-created-property-name "CREATED"
    "The name of the org-mode property that stores the creation date of the entry")

  (defun org-set-created-property (&optional active NAME)
    "Set a property on the entry giving the creation time.

By default the property is called CREATED. If given the `NAME'
argument will be used instead. If the property already exists, it
will not be modified."
    (interactive)
    (let* ((created (or NAME org-created-property-name))
            (fmt (if active "<%s>" "[%s]"))
            (now  (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
      (unless (org-entry-get (point) created nil)
        (org-set-property created now))))


;;;; functions

  ;; https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
  (defun my/org-remove-link ()
    "Replace an org link by its description or if empty its address"
    (interactive)
    (if (org-in-regexp org-bracket-link-regexp 1)
        (save-excursion
          (let ((remove (list (match-beginning 0) (match-end 0)))
                (description (if (match-end 3)
                                  (org-match-string-no-properties 3)
                                (org-match-string-no-properties 1))))
            (apply 'delete-region remove)
            (insert description)))))

    (defun my/org-insert-link ()
      "Insert a link from the kill ring"
      (interactive)
      (save-excursion
        (let ((bounds (if (or (not (region-active-p))
                              (region-noncontiguous-p))
                          (bounds-of-thing-at-point 'word)
                          (region-bounds))))
          (goto-char (cdr bounds))
          (insert "]]")
          (goto-char (car bounds))
          (insert "[[")
          (insert
            (helm :sources
                  (helm-build-sync-source "Kill Ring"
                    :candidates #'helm-kill-ring-candidates
                    :filtered-candidate-transformer #'helm-kill-ring-transformer
                    :fuzzy-match t
                    :multiline 'helm-kill-ring-max-offset
                    :group 'helm-ring)
                  :buffer "*helm kill ring*"
                  :resume 'noresume
                  :allow-nest t))
          (insert "]["))))

;;;; keybindings

  (spacemacs/set-leader-keys
    "oil"  'my/org-insert-link
    "orl"  'my/org-remove-link
    ;; org-noter
    "on"   'org-noter)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode

    ;; org-time-stamp
    "oac" 'anki-editor-cloze-region

    ;; dynamic blocks
    "Du"  'org-dblock-update
    "Da"  'org-update-all-dblocks

    ;; org-wiki
    ;; "wI"  'org-wiki-index
    ;; "wil" 'org-wiki-insert-link
    ;; "win" 'org-wiki-insert-new
    ;; "wn"  'org-wiki-nav
    ;; "wf"  'org-wiki-helm
    ;; "wh"  'org-wiki-panel
    ;; "ws"  'org-wiki-switch-root

    ;; org-time-stamp
    "oTi" 'org-time-stamp
    "oTI" 'org-time-stamp-inactive

    ;; CREATED property
    "iPC" 'org-set-created-property

    ;; helm-org-rifle (added upstream)
    ;; "r"   'helm-org-rifle

    ;; org-subtree
    "sc"   'org-cut-subtree
    "sy"   'org-copy-subtree
    "sp"   'org-paste-subtree
    "sm"   'org-mark-subtree))

;;; C/C++

(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-include-path
                            (list (expand-file-name "~/code/mate/submodules/souffle/include")
                                  (expand-file-name "~/code/mate/llvm/PointerAnalysis")))))

(require 'llvm-mode nil 'noerror)

;; TODO: not sure the below works

;; https://bit.ly/2QqJLvw

;; (defun my/clang-format-buffer-smart ()
;;   "Reformat buffer if .clang-format exists in the projectile root."
;;   (interactive)
;;   (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
;;     (clang-format-buffer)))

;; (defun my/clang-format-buffer-smart-on-save ()
;;   "Add auto-save hook for clang-format-buffer-smart."
;;   (add-hook 'before-save-hook 'my/clang-format-buffer-smart nil t))

;; (spacemacs/add-to-hooks '(lambda () (add-hook 'before-save-hook 'clang-format-buffer nil t))
;;                         '(c-mode-hook c++-mode-hook))

(when (require 'cflow-mode nil 'noerror)
  (spacemacs/set-leader-keys-for-major-mode 'cflow-mode
    "qg" 'cflow-find-function
    "qs" 'cflow-find-function
    "qr" 'cflow-find-recursion-root
    "qR" 'cflow-find-recursion-next
    "qx" 'cflow-goto-expand
    "qE" 'cflow-edit-out-full)
  (autoload 'cflow-mode "cflow-mode")
  (setq auto-mode-alist (append auto-mode-alist
                                '(("\\.cflow$" . cflow-mode)))))

(defconst libc-headers
  '("<assert.h>"
    "<complex.h>"
    "<ctype.h>"
    "<errno.h>"
    "<fenv.h>"
    "<float.h>"
    "<inttypes.h>"
    "<iso646.h>"
    "<limits.h>"
    "<locale.h>"
    "<math.h>"
    "<setjmp.h>"
    "<signal.h>"
    "<stdalign.h>"
    "<stdarg.h>"
    "<stdatomic.h>"
    "<stdbool.h>"
    "<stddef.h>"
    "<stdint.h>"
    "<stdio.h>"
    "<stdlib.h>"
    "<stdnoreturn.h>"
    "<string.h>"
    "<tgmath.h>"
    "<threads.h>"
    "<time.h>"
    "<uchar.h>"
    "<wchar.h>"
    "<wctype.h>"))

(defconst stl-headers
  '("<concepts>"
    "<cstdlib>"
    "<csignal>"
    "<csetjmp>"
    "<cstdarg>"
    "<typeinfo>"
    "<typeindex>"
    "<type_traits>"
    "<bitset>"
    "<functional>"
    "<utility>"
    "<ctime>"
    "<chrono>"
    "<cstddef>"
    "<initializer_list>"
    "<tuple>"
    "<any>"
    "<optional>"
    "<variant>"
    "<compare>"
    "<version>"
    "<new>"
    "<memory>"
    "<scoped_allocator>"
    "<memory_resource>"
    "<climits>"
    "<cfloat>"
    "<cstdint>"
    "<cinttypes>"
    "<limits>"
    "<exception>"
    "<stdexcept>"
    "<cassert>"
    "<system_error>"
    "<cerrno>"
    "<contract>"
    "<cctype>"
    "<cwctype>"
    "<cstring>"
    "<cwchar>"
    "<cuchar>"
    "<string>"
    "<string_view>"
    "<charconv>"
    "<array>"
    "<vector>"
    "<deque>"
    "<list>"
    "<forward_list>"
    "<set>"
    "<map>"
    "<unordered_set>"
    "<unordered_map>"
    "<stack>"
    "<queue>"
    "<span>"
    "<iterator>"
    "<ranges>"
    "<algorithm>"
    "<execution>"
    "<cmath>"
    "<complex>"
    "<valarray>"
    "<random>"
    "<numeric>"
    "<ratio>"
    "<cfenv>"
    "<bit>"
    "<iosfwd>"
    "<ios>"
    "<istream>"
    "<ostream>"
    "<iostream>"
    "<fstream>"
    "<sstream>"
    "<syncstream>"
    "<strstream>"
    "<iomanip>"
    "<streambuf>"
    "<cstdio>"
    "<locale>"
    "<clocale>"
    "<codecvt>"
    "<regex>"
    "<atomic>"
    "<thread>"
    "<mutex>"
    "<shared_mutex>"
    "<future>"
    "<condition_variable>"
    "<filesystem>"
    ))

(defun my/libc-include (header-name)
  "An interactive prompt for the extension (EXT-NAME) to add."
  (interactive (list (completing-read "libc header: " libc-headers)))
  (evil-open-below 1)
  (evil-beginning-of-line)
  (insert (concat "#include " header-name)))

(defun my/c++-include (header-name)
  "An interactive prompt for the extension (EXT-NAME) to add."
  (interactive (list (completing-read "libc header: "
                                      (append libc-headers stl-headers))))
  (evil-open-below 1)
  (evil-beginning-of-line)
  (insert (concat "#include " header-name)))

(spacemacs/set-leader-keys-for-major-mode 'c-mode
  "ih" 'my/libc-include)

(spacemacs/set-leader-keys-for-major-mode 'c++-mode
  "ih" 'my/c++-include)

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

;;; Souffle

(use-package souffle-mode
  :mode "\\.dl\\'")

;;; Racket

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
  ("s" lispy-mark-symbol))

(dolist (mode (list 'racket-mode 'emacs-lisp-mode))
  (spacemacs/set-leader-keys-for-major-mode mode
    "n"   'my/toggle-structural-nav
    ">"   'sp-forward-slurp-sexp
    "<"   'lispy-barf
    "ic"  'lispy-clone
    "xf"  'lispy-delete
    "xb"  'lispy-delete-backward))

(with-eval-after-load 'lispy
  (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'racket-mode-hook (lambda () (lispy-mode 1)))
  (add-hook 'lispy-mode-hook #'lispyville-mode)

  ;; Delete keymap
  (define-key lispy-mode-map (kbd "d") nil) ;; delete old binding
  (lispy-define-key lispy-mode-map-lispy "d" 'lispy-delete)) ;; new binding

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

;;; Agda

(when (require 'agda2-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.lagda.md\\'" . agda2-mode)))

;;; Coq

;; (with-eval-after-load 'company-coq
;;    (add-to-list 'company-coq-disabled-features 'prettify-symbols))

;; Coq key bindings:
;; - Use SPC-m-SPC instead of SPC-m-. for less pinky strain
;; - Use SPC-gE to process to the end of the file. Good for finding out where one left off.
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

                                      ; undo-tree and proof-general do NOT work together, this makes the breakage more tolerable
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
      (compile (concat "make " (substring n 0 l) ".vo"))))

  )


;;; Shell

(add-hook 'sh-mode-hook '(lambda () (setq sh-basic-offset 2
                                          sh-indentation 2)))

;;; Python

(with-eval-after-load 'python

  (defun my/search-replace (find-str replacement-str)
    (save-excursion
      (goto-char 0)
      (while (not (equal nil (search-forward find-str nil t)))
        (replace-match replacement-str))))
  (defun my/python-dict-to-json ()
    (interactive)
    (my/search-replace "{'" "{\"")
    (my/search-replace "'}" "\"}")
    (my/search-replace "['" "[\"")
    (my/search-replace "']" "\"]")
    (my/search-replace "'," "\",")
    (my/search-replace "':" "\":")
    (my/search-replace ": '" ": \"")
    (my/search-replace ", '" ", \"")
    (my/search-replace ": False" ": false")
    (my/search-replace ": True," ": true,"))

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

;;; projects/compile

;;;; mate

(defun mate-shake (tgt)
  (let ((default-directory (concat "/sudo::" (projectile-project-root))))
    (compile
      (mapconcat
      'identity
      `("docker run --rm --net=host"
        "--mount type=bind,src=$PWD,dst=/x"
        "--workdir /x"
        "mate-dev"
        "./shake.sh -j4"
        ,tgt)
      " "))))

(defun my/mate-lint ()
  (interactive)
  (mate-shake "lint"))

(defun my/mate-format ()
  (interactive)
  (mate-shake "format"))

(defun my/mate-build ()
  (interactive)
  (mate-shake "build"))

(defun my/mate-lint-jump ()
  (interactive)
  (mate-shake "lint")
  (switch-to-buffer (compilation-find-buffer))
  (goto-char 0)
  (y-or-n-p-with-timeout "Ready to go to error?" 5 nil)
  (compilation-next-error 1)
  (compile-goto-error))

(defun my/next-compilation-error ()
  (interactive)
  (switch-to-buffer (compilation-find-buffer))
  (goto-char 0)
  (compilation-next-error 1)
  (compile-goto-error))

(defun my/mate-lint-jump-no-prompt ()
  (interactive)
  (mate-shake "lint")
  (switch-to-buffer (compilation-find-buffer))
  (while (not (equal nil compilation-in-progress))
    (sleep-for 0.25))
  (if (equal nil (list 0))
      (progn
        (message "Compilation was successful!")
        (kill-buffer))
      (my/next-compilation-error)))

(defun my/rack-mypy ()
  (interactive)
  (direnv-mode 1)
  (compile "mypy .")
  (switch-to-buffer (compilation-find-buffer))
  (while (not (equal nil compilation-in-progress))
    (sleep-for 0.25))
  (if (equal nil (list 0))
      (progn
        (message "Compilation was successful!")
        (kill-buffer))
    (my/next-compilation-error)))

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
(defun my/eacl-complete (s)
  (progn (insert s)
          (eacl-complete-line)))

(defun my/smart-insert-import ()
  (interactive)
  (evil-open-below 1)
  (evil-beginning-of-line)
  (cond ((eq major-mode 'haskell-mode) (my/eacl-complete "import          "))
        ((eq major-mode 'scala-mode) (my/eacl-complete "import "))
        ((eq major-mode 'java-mode) (my/eacl-complete "import "))
        ((eq major-mode 'python-mode) (my/eacl-complete "import "))
        ((eq major-mode 'coq-mode) (my/eacl-complete "Require "))
        ((eq major-mode 'rust-mode) (my/eacl-complete "use "))
        ((eq major-mode 'c++-mode) (call-interactively 'my/c++-include))
        ((eq major-mode 'c-mode) (call-interactively 'my/libc-include))))

;; 'i' for 'insert'
;; 'i' for 'import'
(spacemacs/set-leader-keys-for-major-mode 'c-mode
  "ii" 'my/smart-insert-import)
(spacemacs/set-leader-keys-for-major-mode 'c++-mode
  "ii" 'my/smart-insert-import)
(spacemacs/set-leader-keys-for-major-mode 'haskell-mode
  "ii" 'my/smart-insert-import)
(spacemacs/set-leader-keys-for-major-mode 'coq-mode
  "ii" 'my/smart-insert-import)
(spacemacs/set-leader-keys-for-major-mode 'scala-mode
  "ii" 'my/smart-insert-import)
(spacemacs/set-leader-keys-for-major-mode 'java-mode
  "ii" 'my/smart-insert-import)
(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "ii" 'my/smart-insert-import)
