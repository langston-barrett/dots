;; -*- lexical-binding: t; -*-
;;; Org

;;;; Imports

(with-eval-after-load 'org
  (my/load "org/functions.el")
  (my/load "org/keys.el")
  (my/load "org/prettify.el")
  (with-eval-after-load 'org-capture
    (my/load "org/capture.el")))

;;;; Variables

(setq org-directory "~/org")
(setq org-wiki-location org-directory
      org-default-notes-file (concat org-directory "/todo.org"))
;; "This variable needs to be set before org.el is loaded."
(setq org-export-backends '(ascii html md latex))
(setq org-adapt-indentation nil) ;; don't indent text to match headline
;; "This variable needs to be set before any calls to Org-roam functions,
;; including enabling org-roam-mode."
(setq org-roam-directory org-directory)

;; Don't auto-indent babel code blocks
;; https://github.com/syl20bnr/spacemacs/issues/13255
(setq org-src-preserve-indentation t)
(setq org-edit-src-content-indentation 0)

;;;; Miscellaneous Packages

(use-package org-link-minor-mode
  :disabled
  :config
  (add-hook 'prog-mode-hook 'org-link-minor-mode)
  (defun my/no-org-links () (org-link-minor-mode -1))
  (add-hook 'sh-mode-hook 'my/no-org-links))

(use-package org-indent-mode
  :disabled
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-indent-indentation-per-level 2))

(use-package org-brain
  :config
  (defun aa2u-buffer () (aa2u (point-min) (point-max)))
  (setq org-brain-show-resources nil
        org-brain-path org-directory))

;;;; General Configuration

(with-eval-after-load 'org

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

;;;; publish

  (defun my/org-publish-to-markdown (plist filename pub-dir)
    "A variant of `org-html-publish-to-html' for publishing markdown."
    (org-publish-org-to
     'md
     filename
     (concat "." (or (plist-get plist :markdown-extension)
                     "md"))
     plist
     pub-dir))

;;;; roam

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

;;;; archiving

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

    (require 'ox-extra)
    (ox-extras-activate '(ignore-headlines))

    ;; (fci-mode nil) ; Disable fill column indicator, it looks hectic
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
  )
