(require 'buttercup)
(require 'langston-org)

(describe
 "my/org-element-map-file"
 (it
  "works"
  (expect
   (my/org-element-map-file
    "test/agenda.org"
    'headline
    (lambda (elt) (org-element-property :priority elt)))
   :to-equal
   (list ?C ?A))))

(describe
 "my/org-element-map-agenda"
 (it
  "works"
  (expect
   (progn
     (setq-local org-agenda-files (list "test/agenda.org"))
     (my/org-element-map-agenda
      'headline
      (lambda (elt) (org-element-property :priority elt))))
   :to-equal
   (list ?C ?A))))

(describe
 "Finding items 0"
 (it
  "works"
  (expect
   (my/find-agenda-item "TODO Do sub-thing" 'identity (list "test/agenda.org"))
   :not
   :to-be
   nil)))

;; (describe
;;  "Finding items"
;;  (it
;;   "works"
;;   (expect
;;    (progn
;;      (setq-local org-agenda-files (list "test/agenda.org"))
;;      (my/find-agenda-item-2 "TODO Do sub-thing" 'identity))
;;    :not
;;    :to-be
;;    nil)))

(describe
 "Fetching deadlines"

 (it
  "works for a headline with a deadline"
  (expect
   (progn
     (setq-local org-agenda-files (list "test/agenda.org"))
     (my/get-item-deadline "TODO Do sub-thing"))
   :not
   :to-be
   nil))

 (it
  "works for a headline with a deadline and priority"
  (expect
   (progn
     (setq-local org-agenda-files (list "test/agenda.org"))
     (my/get-item-deadline "TODO [#C] Do a thing"))
   :not
   :to-be
   nil))

 (it
  "doesn't work for headlines without a deadline"
  (expect
   (progn
     (setq-local org-agenda-files (list "test/agenda.org"))
     (my/get-item-deadline "TODO [#A] No deadline"))
   :to-be
   nil)))
