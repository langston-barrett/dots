(defconst myorg-packages
  '(org-board ; link archiving
    org-brain
    org-make-toc
    org-noter
    org-super-agenda
    ;; ox-gfm       ; github markdown export for org
    ;; link-hint ; TODO: keybindings
    ;; org-gcal
    ;; org-sidebar
    ))

(defun myorg/init-org-board ()
  (use-package org-board
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "Ba"   'org-board-archive
      "BA"   'org-board-archive-dry-run)))

(defun myorg/init-org-brain ()
  (use-package org-brain
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "rac" 'org-brain-add-child
      "rap" 'org-brain-add-parent
      "raf" 'org-brain-add-friendship)))

(defun myorg/init-org-make-toc ()
  (use-package org-make-toc
    :defer t))


(defun myorg/init-org-noter ()
  (use-package org-noter
    :defer t))

(defun myorg/init-org-super-agenda ()
  (use-package org-super-agenda
    :defer t
    :init
    (progn
      (defun before-four-p (item)
        (>= 16 (string-to-number (format-time-string "%H"))))
      (org-super-agenda-mode)
      (setq
       org-super-agenda-groups
       '(;; Each group has an implicit boolean OR operator between its selectors.
         (:discard (:category ("Dots" "Meta")))
         (:discard (:and (:category "Start" :scheduled future)))
         (:discard (:and (:category "End" :scheduled future)))
         (:discard (:and (:scheduled today :category "End" :pred before-four-p)))
         (:discard (:and (:scheduled past :category "End" :pred before-four-p)))
         (:name "Personal"
                :transformer my/org-agenda-append-deadline
                :category "Personal"
                :order 9)
         (:name "(Waiting)"
                :transformer my/org-agenda-append-deadline
                :todo "WAIT"
                :order 8)
         (:name "Start"
                :transformer my/org-agenda-append-deadline
                :and (:scheduled today :category "Start")
                :and (:scheduled past :category "Start")
                :order 1)
         (:name "End"
                :transformer my/org-agenda-append-deadline
                :and (:scheduled today :category "End")
                :and (:scheduled past :category "End")
                :order 5)
         (:name "(Scheduled)"
                :transformer my/org-agenda-append-deadline
                :and (:scheduled t :not (:scheduled today))
                :order 7)
         (:name "Today"
                :transformer my/org-agenda-append-deadline
                :scheduled today
                :scheduled past
                :deadline past
                :deadline today
                :order 2)
         (:name "Reply"
                :tag "reply"
                :order 4)
         (:name "Important!"
                :and (:priority "A"
                                :scheduled nil)
                :order 3)
         (:auto-parent t
                       :order 5)
         (:name "Low effort"
                :effort< "0:20"
                :order 8))))))
