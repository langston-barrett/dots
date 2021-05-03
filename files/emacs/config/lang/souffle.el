;; -*- lexical-binding: t; -*-
;;; Souffle

(use-package souffle-mode
  :mode "\\.dl\\'"
  :config

;;;; Compilation

  (add-to-list
   'compilation-error-regexp-alist-alist
   `(souffle-error
     ,souffle-error-regexp
     2 ;; Which match is the file?
     ;; Which match is the line (can be dotted pair of (start . end))
     ,souffle--error-regexp-line-match
     nil ;; Which match is the column?
     2   ;; 2 = error, 1 = warning, 0 = info
     2   ;; Which match should have the hyperlink face applied
     (1 "compilation-error"))) ;; Additional faces to apply to matches
  (cl-pushnew 'souffle-error compilation-error-regexp-alist)

;;;; TODO: eldoc

;;;; Go-to-definition

  (with-eval-after-load 'dumb-jump
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
    (add-to-list
     'dumb-jump-find-rules
     '(:language
       "souffle"
       :type "function"
       :supports ("ag" "grep" "rg" "git-grep")
       :regex "\\\.decl\\b\\s*JJJ\\j\\("
       :tests (".decl foo")
       :not (".type bar")))
    (add-to-list
     'dumb-jump-find-rules
     '(:language
       "souffle"
       :type "type"
       :supports ("ag" "grep" "rg" "git-grep")
       :regex "\\\.type\\b\\s*JJJ\\j\\s*\\="
       :tests (".type foo")
       :not (".decl bar")))
    (spacemacs/set-leader-keys-for-major-mode 'souffle-mode
      "gg" 'xref-find-definitions))

;;;; Completion: Company-mode

  (defconst
    souffle-io-keywords
    '("stdin"
      "stdout"
      "json"
      "file"))

  (defconst
    souffle-other-keywords
    '("nil"
      "choice-domain"))

  (defconst
    souffle-keywords
    (append
     souffle-dot-keywords               ; defined in souffle-mode
     souffle-string-functions           ; defined in souffle-mode
     souffle-aggregation-functions      ; defined in souffle-mode
     souffle-types                      ; defined in souffle-mode
     souffle-io-keywords
     souffle-other-keywords))

  (defun my/souffle-company-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))

    (cl-case command
      (interactive (company-begin-backend 'my/souffle-company-backend))
      (prefix (and (eq major-mode 'souffle-mode)
                   (company-grab-symbol)))
      (candidates
       (cl-remove-if-not
        (lambda (c) (string-prefix-p arg c))
        souffle-keywords))))

  ;; TODO: capf > company-mode
  ;; (defun souffle-completion-at-point ()
  ;;   "Function for `completion-at-point-functions' in `souffle-mode'."
  ;;   (my/souffle-company-backend
  ;;    'candidates
  ;;    (my/souffle-company-backend 'prefix)))
  ;; (add-hook 'completion-at-point-functions
  ;;           #'souffle-completion-at-point nil 'local)

  ;; Hook
  (defun my/souffle-mode-hook ()
    (add-to-list 'company-backends 'my/souffle-company-backend)
    (require 'dumb-jump))
  (add-hook 'souffle-mode-hook #'my/souffle-mode-hook))
