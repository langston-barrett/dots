;;; Cheatsheet

(require 'cheatsheet)

;;;; vim combinators

(cheatsheet-add-group
 'vim-combinators
 '(:key "aa"  :description "(spacemacs) text object: an argument")
 '(:key "a\"" :description "text object: a string")
 '(:key "a`"  :description "text object: backquoted")
 '(:key "a)"  :description "text object: parens"))

;;;; common

(cheatsheet-add-group
 'common
 '(:key "alt+p"          :description "spacmacsanywhere.sh")
 '(:key "SPC g t"        :description "git timemachine transient state")
 '(:key "SPC g r"        :description "smerge transient state")
 '(:key "gf"             :description "go to relative path at cursor")
 '(:key "SPC i s"        :description "yas")
 '(:key "SPC i S"        :description "auto-yasnippet")
 '(:key "SPC j i"        :description "jump in buffer")
 '(:key "SPC j I"        :description "jump in all buffers of same major mode")
 '(:key "SPC v"          :description "expand-region")
 '(:key "CTRL x 8 ENTER" :description "enter a unicode symbol (in search bar)")
 '(:key "SPC a u"        :description "undo-tree-visualize"))

;;;; helm-projectile

(cheatsheet-add-group
 'helm-projectile
 '(:key "SPC+CTRL"       :description "After SPC p f, mark files")
 '(:key "CTRL-z"         :description "After SPC p f, file actions")
 '(:key "SPC p h"        :description "helm-projectile combo function")
 '(:key "SPC p p CTRL-z" :description "switch projectile project, open magit"))

;;;; favorites

(cheatsheet-add-group
 'favorites
 '(:key "SPC x a a" :description "align")
 '(:key "SPC s e"   :description "iedit!")
 '(:key "SPC o s p" :description "highlight symbol"))

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

;;;; org-ref

(cheatsheet-add-group
 'org-ref
 '(:key "SPC o r r" :description "org-ref")
 '(:key "SPC o r n" :description "org-ref notes for this citation"))

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
