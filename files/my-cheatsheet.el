;;; Cheatsheet

(require 'cheatsheet)

;;;; common

(cheatsheet-add-group
 'common
 '(:key "alt+p"          :description "spacmacsanywhere.sh")
 '(:key "SPC g t"        :description "git timemachine transient state")
 '(:key "gf"             :description "go to relative path at cursor")
 '(:key "SPC i s"        :description "yas")
 '(:key "SPC i S"        :description "auto-yasnippet")
 '(:key "SPC j i"        :description "jump in buffer")
 '(:key "SPC j I"        :description "jump in all buffers of same major mode")
 '(:key "SPC v"          :description "expand-region")
 '(:key "CTRL x 8 ENTER" :description "enter a unicode symbol (in search bar)")
 '(:key "SPC a u"        :description "undo-tree-visualize"))

;;;; favorites

(cheatsheet-add-group
 'favorites
 '(:key "SPC x a a" :description "align")
 '(:key "SPC s e"   :description "iedit!"))

;;;; functions

(cheatsheet-add-group
 'functions
 '(:key "helm-top" :description ""))

;;;; org-agenda

(cheatsheet-add-group
 'org-agenda
 '(:key "SPC a o a" :description "open agenda")
 '(:key "t"         :description "cycle todo status")
 '(:key "p"         :description "cycle priority")
 '(:key "s"         :description "schedule")
 '(:key "d"         :description "deadline")
 '(:key "TAB"       :description "go-to")
 '(:key "f*"        :description "filter")
 '(:key "v*"        :description "view")
 '(:key "\\"        :description " filter items by tag")
 '(:key "-"         :description "exclude items by tag"))

;;;; mine

(cheatsheet-add-group
 'org-agenda
 '(:key "SPC a o a" :description "open agenda")
 '(:key "t"         :description "cycle todo status")
 '(:key "p"         :description "cycle priority")
 '(:key "s"         :description "schedule")
 '(:key "d"         :description "deadline")
 '(:key "TAB"       :description "go-to")
 '(:key "f*"        :description "filter")
 '(:key "v*"        :description "view")
 '(:key "\\"        :description " filter items by tag")
 '(:key "-"         :description "exclude items by tag"))

(provide 'my-cheatsheet)
