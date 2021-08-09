;; -*- lexical-binding: t; -*-

(defun my/hide ()
  (interactive)
  (org-cycle-hide-drawers 'all))

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

(contract-defun
 my/org-select-image
 ()
 :contract (contract-> contract-string-c)
 (interactive)
 ()
 (completing-read
  "Select image: "
  (directory-files-recursively (concat org-directory "/img") ".")))

(contract-defun
 my/org-insert-image
 ()
 :contract (contract-> contract-any-c)
 (interactive)
 (insert "[[file:")
 (insert (my/org-select-image))
 (insert "]]"))

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

(defun my/org-archive-done-tasks ()
  "Archive all done tasks."
  (interactive)
  (when (not (string-match-p (regexp-quote "archive") buffer-file-name))
    (org-map-entries 'org-archive-subtree "/DONE" 'file)
    (org-map-entries 'org-archive-subtree "/CANCELLED" 'file)))

;; TODO: If current-kill is empty/not a URL, select from kill ring
(defun my/org-insert-url-from-kill-ring ()
  (interactive)
  (save-excursion
    (org-insert-property-drawer)
    (org-set-property "URL" (current-kill 0))))

(defun my/display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
