;; -*- lexical-binding: t; -*-

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

  "iu" 'my/org-insert-url-from-kill-ring

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
  "sm"   'org-mark-subtree)
