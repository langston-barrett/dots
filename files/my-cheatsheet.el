;;; Cheatsheet

(require 'cheatsheet)

;;;; Common

(cheatsheet-add-group
 'Common
 '(:key "SPC g t" :description "git timemachine transient state")
 '(:key "gf" :description "go to relative path at cursor"))

;;;; Favorites

(cheatsheet-add-group
 'Favorites
 '(:key "SPC x a a" :description "align"))

(provide 'my-cheatsheet)
