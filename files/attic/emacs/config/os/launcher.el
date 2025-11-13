;; Launcher --- Using Emacs as an application launcher -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Using Emacs as an application launcher
;;
;; https://www.mattduck.com/emacs-fuzzy-launcher.html

;;; Code:

(require 'helm)

;;; Sources

;;;; System

(defun my/launcher-source-system ()
  (helm-build-sync-source "System"
    :multimatch nil
    :requires-pattern nil
    :candidates '(("Lock" . "xset dpms force off") ;; turns laptop screen off and triggers i3lock
                  ("Sleep" . "systemctl suspend -i")
                  ("Restart" . "systemctl reboot -i")
                  ("Shutdown" . "systemctl poweroff -i"))
    :action '(("Execute" . (lambda (candidate)
                             (shell-command (concat candidate " >/dev/null 2>&1 & disown") nil nil))))))

;;;; Executables on PATH

(defvar my/cached-executables '())

(contract-defun
 my/collect-executables
 (&optional path)
 :contract (contract-> contract-sequence-c) ;; TODO more specific: list of abs paths
 (apply
   #'append
   (cl-loop
    for path in (split-string (if path path (getenv "PATH")) ":")
    if (file-directory-p path)
    ;; TODO: Only executable files
    collect (directory-files path))))

(defun my/launcher-source-path ()
  (helm-build-sync-source "Executables"
    :multimatch nil
    :requires-pattern nil
    :candidates
    (lambda ()
      (unless my/cached-executables
        (setq my/cached-executables (my/collect-executables)))
      my/cached-executables)
    :action
    '(("Launch" .
       (lambda (candidate)
         (shell-command (concat candidate " >/dev/null 2>&1 & disown") nil nil))))))

;;;; Applications

(defun my/seq-has-suffix (seq suf)
  "Test if SEQ has suffix SUF.

>> (my/seq-has-suffix \"foo\" \"oo\")
=> t
>> (my/seq-has-suffix \"foo\" \"oof\")
=> nil"
  (let ((len (length seq))
        (suf-len (length suf)))
    (equal suf (seq-subseq seq (- len suf-len)))))

(defun my/seq-drop-suffix (seq suf)
  (if (my/seq-has-suffix seq suf)
      (seq-subseq seq (- (length seq) (length suf)))
    seq))

;; TODO: Use .desktop files
(defun my/launcher-source-apps ()
  (helm-build-sync-source "Apps"
    :multimatch nil
    :requires-pattern nil
    :candidates
    (lambda ()
      (mapcar
       (lambda (item) (my/seq-drop-suffix item ".desktop"))
       (cl-remove-if
        (lambda (d) (or (string= d ".") (string= d "..")))
        (append
         (directory-files "/run/current-system/sw/share/applications")
         (directory-files "~/.nix-profile/share/applications")))))
    :action
    '(("Launch" .
       (lambda (candidate)
         (shell-command (concat "gtk-launch " candidate " >/dev/null 2>&1 & disown") nil nil))))))

;;; Launcher

(defun my/launcher ()
  (interactive)
  (with-current-buffer (get-buffer-create "*launcher*")
    (let ((frame (make-frame '((name . "Launcher")
                               (window-system . x)
                               (auto-raise . t) ; focus on this frame
                               (height . 10)
                               (internal-border-width . 20)
                               (left . 0.33)
                               (left-fringe . 0)
                               (line-spacing . 3)
                               (menu-bar-lines . 0)
                               (right-fringe . 0)
                               (tool-bar-lines . 0)
                               (top . 48)
                               (undecorated . nil) ; enable to remove frame border
                               (unsplittable . t)
                               (vertical-scroll-bars . nil)
                               (width . 110))))
          (alert-hide-all-notifications t)
          (inhibit-message t)
          (mode-line-format nil)
          (helm-mode-line-string nil)
          (helm-full-frame t)
          (helm-display-header-line nil)
          (helm-use-undecorated-frame-option nil))
      (switch-to-buffer-other-frame (current-buffer))
      (my/recompute-font-size)
      (helm :sources (list (my/launcher-source-path))
            :prompt ""
            :buffer "*launcher*")
      (delete-frame frame)
      ;; If we don't kill the buffer it messes up future state.
      (kill-buffer "*launcher*")
      ;; I don't want this to cause the main frame to flash
      (when (functionp #'x-urgency-hint)
        (x-urgency-hint (selected-frame) nil)))))
