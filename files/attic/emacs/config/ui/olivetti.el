;; -*- lexical-binding: t; -*-

(defun my/activate-olivetti ()

  (setq olivetti-body-width 100)
  (when (require 'olivetti nil 'noerror)

    (require 'dash)

    (setq my/excluded-buffer-names
          (list " *OUTLINE-TOC*" "*SPEEDBAR*" "*Help*" "*org-roam*"))
    (setq my/excluded-buffer-prefixes
          (list
           "org-sidebar"
           "<tree>"
           "HELM"
           " *Minibuf"))

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
      nil                               ; init-value
      nil                               ; lighter
      nil                               ; keymap
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

  (defun my/display-olivetti-tentative-mode ()
    (olivetti-tentative-mode -1))
  (defun my/disable-olivetti-tentative-mode ()
    (olivetti-tentative-mode -1))
  (add-hook 'magit-log-mode-hook #'my/disable-olivetti-tentative-mode))

(my/when-idle-med 'my/activate-olivetti)

