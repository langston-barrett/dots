(defconst myorg-packages
  '(org-board ; link archiving
    org-brain
    org-make-toc
    org-mind-map
    org-noter
    org-super-agenda
    org-roam-server
    (org-roam-server :toggle myorg-enable-roam-server)
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

(defun myorg/init-org-mind-map ()
  (use-package org-mind-map
    :init
    (require 'ox-org)
    :defer t
    :config
    (setq org-mind-map-engine "dot")       ; Default. Directed Graph
    ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
    ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
    ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
    ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
    ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
    ))

(defun myorg/init-org-make-toc ()
  (use-package org-make-toc
    :defer t))

(defun myorg/init-org-noter ()
  (use-package org-noter
    :defer t))

(defun myorg/init-org-roam-server ()
  (use-package org-roam-server
    :defer t
    :config
    (setq org-roam-server-host "127.0.0.1"
          org-roam-server-port 8080
          org-roam-server-authenticate nil
          org-roam-server-export-inline-images t
          org-roam-server-serve-files nil
          org-roam-server-served-file-extensions '("pdf")
          org-roam-server-network-poll t
          org-roam-server-network-arrows nil
          org-roam-server-network-label-truncate t
          org-roam-server-network-label-truncate-length 60
          org-roam-server-network-label-wrap-length 20)))

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
         (:name "Meta"
                :category "Meta"
                :order 11)
         (:name "Dots"
                :category "Dots"
                :order 12)
         (:auto-parent t
                       :order 5)
         (:name "Low effort"
                :effort< "0:20"
                :order 8))))))
