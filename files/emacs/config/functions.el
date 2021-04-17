;; -*- lexical-binding: t; -*-

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

(defun my/increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (unless (looking-at "[0-9]+") (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

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

(defun my/projectile-ag-in-other-project ()
  (interactive)
  (setq bak projectile-switch-project-action)
  (setq projectile-switch-project-action '(lambda () (call-interactively 'helm-projectile-ag)))
  (projectile-switch-project)
  (setq projectile-switch-project-action bak))

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
