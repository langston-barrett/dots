;;; sb-imenu.el

(require 'imenu)
(require 'speedbar)

(defvar sb-imenu-populate-tag-function 'sb-imenu-populate-tag)

(defvar sb-imenu-key-map nil
  "speedbar imenu keymap.")

(defun sb-imenu-install-speedbar-variables ()
  "Install speedbar variables."
  (setq sb-imenu-key-map (speedbar-make-specialized-keymap))
  (define-key sb-imenu-key-map (kbd "RET") 'speedbar-edit-line)
  (define-key sb-imenu-key-map (kbd "TAB") 'sb-imenu-toggle-line-expansion))

(defun sb-imenu-toggle-line-expansion ()
  "Toggle line expansion and stay in place."
  (interactive)
  (save-excursion
    (speedbar-toggle-line-expansion)))

(if (featurep 'speedbar)
    (sb-imenu-install-speedbar-variables)
  (add-hook 'speedbar-load-hook 'sb-imenu-install-speedbar-variables))

(speedbar-add-expansion-list '("sb-imenu" nil sb-imenu-key-map sb-imenu-buttons))

;; prettify-symbols-mode doesn't work in speedbar, so compose characters ourself
(defun sb-imenu-prettify-text (start length)
  "Prettify speedbar in text mode."
  (unless speedbar-use-images
    (save-excursion
      (goto-char start)
      (cond ((looking-at " =>")
             (compose-region start (+ start length) ?•))
            ((looking-at ".\\+.")
             (compose-region start (+ start length) ?▶))
            ((looking-at ".-.")
             (compose-region start (+ start length) ?▼))))))

(advice-add #'speedbar-insert-image-button-maybe :after #'sb-imenu-prettify-text)

(defvar sb-imenu-active-buffer nil)
(defvar sb-imenu-active-buffer-mode nil)

(defun sb-imenu-buttons (dir depth)
  "Show imenu tags for current buffer."
  (let (tags buf-name)
    (sb-imenu-get-active-buffer)
    (when sb-imenu-active-buffer
      (with-current-buffer sb-imenu-active-buffer
        (setq buf-name (buffer-name))
        (setq imenu--index-alist nil)
        (condition-case nil
            (imenu--make-index-alist t)
          (error nil))
        (setq tags (sb-imenu-make-tree imenu--index-alist)))
      (setq-local header-line-format buf-name)
      (when tags
        (when (string= (caar tags) "*Rescan*")
          (setq tags (cdr tags)))
        (when (and tags (not (null (car tags))))
          (sb-imenu-populate tags 0))))))

(defun sb-imenu-make-tree (tag-alist)
  "If TAG-ALIST is flat, turn it into a alist tree using class separators."
  (catch 'is-tree
    (let (tree)
      (dolist (el tag-alist)
        (let ((loc (cdr el)))
          (if (not (numberp loc))
              (throw 'is-tree (copy-alist tag-alist))
            (setq tree (sb-imenu-recursive-make-tree (split-string (car el) "::\\|[.]") loc tree)))))
      tree)))

(defun sb-imenu-recursive-make-tree (pieces loc tree)
  "Recursively insert into alist tree."
  (let* ((piece (car pieces))
         (new-pieces (cdr pieces))
         (new-tree (assoc piece tree)))
    (if new-pieces
        (progn
          (if new-tree
              (when (numberp (cdr new-tree))
                (setcdr new-tree nil))
            (add-to-list 'tree (cons piece nil) t)
            (setq new-tree (assoc piece tree)))
          (setf (cdr new-tree) (sb-imenu-recursive-make-tree new-pieces loc (cdr new-tree))))
      (add-to-list 'tree (cons piece loc) t))
    tree))

(defun sb-imenu-get-active-buffer ()
  "Get the active buffer."
  (setq sb-imenu-active-buffer nil
        sb-imenu-active-buffer-mode nil)
  (condition-case nil
      (with-selected-frame (dframe-select-attached-frame (speedbar-current-frame))
        (sb-imenu-get-interesting-buffer))
    (error nil))
  (unless sb-imenu-active-buffer
    (sb-imenu-get-interesting-buffer)))

(defun sb-imenu-get-interesting-buffer ()
  "Get an interesting buffer."
  (catch 'done
    (dolist (buffer (buffer-list))
      (unless (string-match "^[ *]" (buffer-name buffer))
        (setq sb-imenu-active-buffer buffer)
        (with-current-buffer buffer
          (setq sb-imenu-active-buffer-mode major-mode))
        (throw 'done buffer)))))

(defun sb-imenu-populate (tags level)
  "Populate speedbar from imenu tags."
  (let (is-expandable tag-info)
    (dolist (item tags)
      (setq is-expandable (imenu--subalist-p item))
      (setq tag-info (funcall sb-imenu-populate-tag-function (car item) is-expandable))
      (if is-expandable
          (progn
            (speedbar-make-tag-line 'curly
                                    ?- 'sb-imenu-expand-line
                                    (cdr item)
                                    (car tag-info) 'sb-imenu-expand-line (cdr item)
                                    (cdr tag-info) level)
            (sb-imenu-populate (cdr item) (1+ level)))
        (speedbar-make-tag-line 'statictag
                                nil nil
                                nil
                                (car tag-info) 'sb-imenu-go (cdr item)
                                (cdr tag-info) level)))))

(defun sb-imenu-populate-tag (tag is-expandable)
  "Take a tag and return a list with the transformed tag and a face to use."
  (list tag (if is-expandable 'font-lock-keyword-face 'font-lock-variable-name-face)))

(defun sb-imenu-expand-line (text token indent)
  "Expand/contract the item under the cursor."
  (interactive)
  (if (save-excursion (beginning-of-line)
                      (looking-at "[0-9]+:\\s-*{[+]}"))
      ;; Expand
      (progn
        (speedbar-change-expand-button-char ?-)
        (forward-line)
        (speedbar-with-writable
          (save-excursion
            (sb-imenu-populate token (1+ indent)))))
    ;; Contract
    (speedbar-change-expand-button-char ?+)
    (speedbar-with-writable
      (save-excursion
        (forward-line)
        (while (and (not (eobp))
                    (looking-at "\\([0-9]+\\):")
                    (> (string-to-number (match-string-no-properties 1)) indent))
          (delete-region (point-at-bol) (1+ (point-at-eol))))))))

(defun sb-imenu-go (text node indent)
  "Goto the current tag."
  (interactive)
  (condition-case nil
      (progn
        (speedbar-select-attached-frame)
        (raise-frame)
        (select-frame-set-input-focus (selected-frame)))
    (error nil))
 (switch-to-buffer sb-imenu-active-buffer)
  (goto-char node))

(defun sb-imenu-refresh ()
  "Refresh the speedbar."
  (let ((buf sb-imenu-active-buffer))
    (sb-imenu-get-active-buffer)
    (unless (equal buf sb-imenu-active-buffer)
      (speedbar-refresh))))

(add-hook 'speedbar-timer-hook 'sb-imenu-refresh)

(provide 'sb-imenu)
