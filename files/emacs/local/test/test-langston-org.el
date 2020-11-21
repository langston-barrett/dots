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
 "Finding items"
 (it
  "works"
  (expect
   (my/find-agenda-item "TODO Do sub-thing" (list "test/agenda.org"))
   :not
   :to-be
   nil)))

(describe
 "Applying transformers"

 (it
  "works for a headline with a deadline"
  (expect
   (progn
     (setq-local org-agenda-files (list "test/agenda.org"))
     (my/org-apply-agenda-transformers "TODO Do sub-thing"))
   :not
   :to-be
   nil)))
