(require 'buttercup)
(require 'langston-org)

(describe
 "Finding items"
 (it
  "works"
  (expect
   (my/find-agenda-item "TODO Do sub-thing" 'identity (list "test/agenda.org"))
   :not
   :to-be
   nil)))

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
