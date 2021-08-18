;; -*- lexical-binding: t; -*-
;;; shell

(require 'contract)
(require 'ring)

;; TODO(lb): Try out https://github.com/mbriggs/emacs-pager

;;; Directory Tracking

(defun add-mode-line-dirtrack ()
  (add-to-list
   'mode-line-buffer-identification
   '(:propertize (" " default-directory " ") face dired-directory)))

;;;; Procfs

;; NOTE: This has been difficult to get to work with TRAMP, so I use the prompt
;; method below.
;;
;; TODO: Make TRAMP procfs directory tracking into a MELPA package

(defvar my/shell-procfs-dirtrack-tramp-prefix "")
(defvar my/shell-procfs-dirtrack-shell-pid nil)
(make-variable-buffer-local 'my/shell-procfs-dirtrack-tramp-prefix)
(make-variable-buffer-local 'my/shell-procfs-dirtrack-shell-pid)

(contract-defun
 my/get-local-shell-pid ()
 :contract (contract-> (contract-maybe-c contract-nat-number-c))
 (let ((proc (get-buffer-process (current-buffer))))
   (if proc
       (process-id proc)
     nil)))

(defun my/get-remote-shell-pid ()
  (interactive)
  (comint-simple-send (get-buffer-process (current-buffer)) "echo \" $$\"\n")
  (save-excursion
    (search-backward-regexp " [[:digit:]]+")
    (string-to-number
     (buffer-substring (match-beginning 0) (match-end 0)))))

(defun my/shell-procfs-dirtrack-refresh-pid ()
  (interactive)
  (setq my/shell-procfs-dirtrack-shell-pid
        (if (tramp-tramp-file-p default-directory)
            (my/get-remote-shell-pid)
          (my/get-local-shell-pid))))

(defun shell-procfs-dirtrack (str)
  (prog1 str
    (when (string-match comint-prompt-regexp str)
      (let ((directory (tramp-handle-file-symlink-p
                        (format (concat my/shell-procfs-dirtrack-tramp-prefix
                                        "/proc/%s/cwd")
                                my/shell-procfs-dirtrack-shell-pid))))
        (when (tramp-handle-file-directory-p directory)
          (cd directory))))))

(define-minor-mode shell-procfs-dirtrack-mode
  "Track shell directory by inspecting procfs."
  nil nil nil

  (defvar-local my/shell-procfs-dirtrack-tramp-prefix "")
  (defvar-local my/shell-procfs-dirtrack-shell-pid "")

  (cond (shell-procfs-dirtrack-mode
         (setq my/shell-procfs-dirtrack-tramp-prefix
               (if (tramp-tramp-file-p default-directory)
                   ;; (with-parsed-tramp-file-name default-directory parsed
                   ;;   ;; TODO: How to go from TRAMP struct to string?
                   ;;   )
                   (my/get-tramp-prefix default-directory)
                 ""))
         (my/shell-procfs-dirtrack-refresh-pid)
         (when (bound-and-true-p shell-dirtrack-mode)
           (shell-dirtrack-mode 0))
         (when (bound-and-true-p dirtrack-mode)
           (dirtrack-mode 0))
         (add-hook 'comint-preoutput-filter-functions
                   'shell-procfs-dirtrack nil t))
        (t
         (remove-hook 'comint-preoutput-filter-functions
                      'shell-procfs-dirtrack t))))

;; (add-hook 'shell-mode-hook #'shell-procfs-dirtrack-mode)

;;;; Prompt

;; In ZSH, try:
;;
;;     NEWLINE=$'\n'
;;     PROMPT="%30000<<${NEWLINE}[/ssh:%n@%m:%0d]${NEWLINE}> "
;;
;; In Bash:
;;
;;     NEWLINE=$'\n'
;;     PS1="${NEWLINE}[/ssh:\u@\h:\w]${NEWLINE}> "
;;
;; In Docker (TODO: Not working, docker-tramp hangs):
;;
;;     NEWLINE=$'\n'
;;     CID=$(basename $(cat /proc/1/cpuset))
;;     CID=${CID:0:12}
;;     USER=$(whoami)
;;     PS1="${NEWLINE}[/ssh:langston@big|sudo:root@big|docker:${USER}@${CID}:\w] ${NEWLINE}> "
;;
;; In Podman:
;;
;;     NEWLINE=$'\n'
;;     CID=${CID:0:12}
;;     USER=$(whoami)
;;     PS1="${NEWLINE}[/ssh:langston@big|docker:${USER}@${CID}:\w] ${NEWLINE}> "
;;
;; Then:

(setq dirtrack-list '("^\\[\\(/.+\\)\\]" 1 nil))
(add-hook 'shell-mode-hook #'dirtrack-mode)
(setq docker-tramp-docker-executable "podman")

(defun my/local-bash-prompt ()
  (interactive)
  (insert
   "NEWLINE=$'\\n'\n"
   "PS1=\"${NEWLINE}[\\w] ${NEWLINE}> \"\n"))

(defun my/local-zsh-prompt ()
  (interactive)
  (insert
   "NEWLINE=$'\\n'\n"
   "PROMPT=\"%30000<<${NEWLINE}[%0d]${NEWLINE}> \""))

(defun my/ssh-bash-prompt ()
  (interactive)
  (insert
   "NEWLINE=$'\\n'\n"
   "PS1=\"${NEWLINE}[/ssh:langston@big:\\w] ${NEWLINE}> \"\n"))

(defun my/ssh-zsh-prompt ()
  (interactive)
  (insert
   "NEWLINE=$'\\n'\n"
   "PROMPT=\"%30000<<${NEWLINE}[/ssh:%n@%m:%0d]${NEWLINE}> \""))

(defun my/podman-bash-prompt ()
  (interactive)
  (insert
   "NEWLINE=$'\\n'\n"
   "CID=${CID:0:12}\n"
   "USER=$(whoami)\n"
   "PS1=\"${NEWLINE}[/ssh:langston@big|docker:${USER}@${CID}:\w] ${NEWLINE}> \"\n"))

;;; Completion

;; Ensure
;;
;;     export HISTCONTROL=ignoreboth
;;
(with-eval-after-load 'shell
  ;; This does weird things to the ZSH prompt
  (native-complete-setup-bash))

(defun my/shell-completion-hook ()
  (company-mode 1)
  (setq-local company-backends '(company-native-complete company-capf)))

(add-hook 'shell-mode-hook #'my/shell-completion-hook)

;;; Variables, Keybindings, and Hooks

(setq
 candidate-shells
 '("/run/current-system/sw/bin/bash"
   "/bin/bash"
   "/bin/sh"))

(dolist (shell (reverse candidate-shells))
  (when (file-exists-p shell)
    (setq shell-file-name shell)))

(defun my/new-shell ()
  (interactive)
  (let* ((muh (my/choose-method-user-host))
         (default-directory
           (concat
            (my/squash-method-user-host muh)
            ":/home/"
            (my/method-user-host-user muh)))
         (current-prefix-arg 1))
    (cd default-directory)
    (call-interactively #'shell)))

(defun my/new-shell-big ()
  (interactive)
  (let ((host (my/choose-host))
        (default-directory "/ssh:big:/home/langston/code/"))
    (cd "/ssh:big:/home/langston/code/")
    (shell)))

(add-hook 'shell-mode-hook #'add-mode-line-dirtrack)

(spacemacs/set-leader-keys-for-major-mode 'shell-mode
  "N"  #'my/new-shell
  "V"  #'vterm)

(defun my/shell-mode-hook ()
  (setq-local olivetti-body-width 140)
  ;; TODO: Make the following conditional on default-directory for TRAMP
  (if (string-suffix-p "zsh" shell-file-name)
      (my/local-zsh-prompt)
    (my/local-bash-prompt)))

(add-hook 'shell-mode-hook #'my/shell-mode-hook)

;;; Interception

;; TODO: Fun stuff like interpolate values of lisp variables or results of
;; calling lisp functions.

;; TODO: TRAMP awareness!

(defun my/comint-intercept-man (str)
  (pcase (split-string str)
    (`("man" ,page) (prog1 "" (woman page)))
    (_ str)))

(defun my/comint-intercept-git-status (str)
  (pcase str
    ("git status" (prog1 "" (magit-status)))
    (_ str)))

(defun my/comint-intercept-ff (str)
  (pcase (split-string str)
    (`("ff" ,arg) (prog1 "" (find-file arg)))
    (_ str)))

;; TODO Doesn't quite work?
(defun my/comint-intercept-lisp (str)
  (pcase (split-string str)
    (`(":eval" ,rest) (prog1 "" (eval rest)))
    (_ str)))

(setq
  my/comint-transformer-list
  (list
   #'my/comint-intercept-man
   #'my/comint-intercept-git-status
   #'my/comint-intercept-ff
   #'my/comint-intercept-lisp))

(defun my/comint-intercept (args)
  (pcase args
    (`(,proc ,str)
     (list proc (funcall (apply #'-compose my/comint-transformer-list) str)))
    (_
     (progn
       (warn "Bad args to comint-input-sender: %s" args)
       args))))

;; TODO: Maybe apply to (the value of) comint-input-sender instead?
(advice-add #'comint-simple-send :filter-args #'my/comint-intercept)
;; (advice-remove #'comint-simple-send #'my/comint-intercept)

;;; Autosuggest

(defun comint-autosuggest--input-bounds ()
  (save-excursion
    (goto-char (point-max))
    (let ((last (point)))
      (comint-bol)
      (cons (point) last))))

(defun comint-autosuggest--prefix ()
  "Get current comint input."
  (let ((bounds (comint-autosuggest--input-bounds)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

;; NOTE: Real "autosuggest" behavior of showing inline suggestions could be
;; copied from `auto-complete'.
(defun comint-autosuggest-completion-at-point ()
  "Completion-at-point function.

May be used with Company using the `company-capf' backend.

TODO: seems to only work after a space."
  (interactive)
  (let ((bounds (comint-autosuggest--input-bounds)))
    (when bounds
      (list (car bounds)
            (cdr bounds)
            (ring-elements comint-input-ring)
            :exclusive 'no))))

(add-hook 'completion-at-point-functions
          #'comint-autosuggest-completion-at-point
          'append)
