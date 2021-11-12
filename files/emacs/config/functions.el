;; -*- lexical-binding: t; -*-

;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun my/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun my/yank-paragraph ()
  (interactive)
  (mark-paragraph)
  (evil-yank (mark) (point)))

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

;; http://ergoemacs.org/emacs/elisp_read_file_content.html
(defun my/read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (split-string (buffer-string) "\n" t)))

(defsubst my/helm-file-lines (&rest paths)
  (helm :sources (helm-build-sync-source (file-name-base (car paths))
                   :candidates (seq-mapcat #'my/read-lines paths)
                   :fuzzy-match t)
        :buffer (string-join (list "*" (car paths) "*"))))

;; TODO: Work with TRAMP
;; TODO: Extract path to constant
(defun my/insert-from-zsh-history ()
  "Insert a line from ~/.zsh_history."
  (interactive)
  (insert (my/helm-file-lines "~/.zsh_history")))

(defun my/insert-from-bash-history ()
  "Insert a line from ~/.bash_history."
  (interactive)
  (insert (my/helm-file-lines "~/.bash_history")))

(defun my/insert-from-shell-history ()
  "Insert a line from ~/.bash_history or ~/.zsh_history."
  (interactive)
  (insert (my/helm-file-lines "~/.bash_history" "~/.zsh_history")))

(defun my/insert-from-small-shell-history ()
  "Insert a line from ~/.bash_history or ~/.zsh_history."
  (interactive)
  (insert (my/helm-file-lines
           "/ssh:siddharthist@small:/home/siddharthist/.bash_history"
           "/ssh:siddharthist@small:/home/siddharthist/.zsh_history")))

(defalias 'my/title-case 'upcase-initials-region)
