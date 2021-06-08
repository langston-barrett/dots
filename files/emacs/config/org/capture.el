;; -*- lexical-binding: t; -*-
(setq org-todo-keywords
      '((sequence "TODO(t)" "ACTIVE(a!)" "WAIT(w!/!)" "|" "DONE(!)")
        (sequence "|" "CANCELLED(c!/!)")))

(defconst meta-org (concat org-directory "/meta.org"))
(defconst vocab-org (concat org-directory "/vocab.org"))
(defconst cards-org (concat org-directory "/cards.org"))
(setq org-capture-templates
      (doct '(("todo"
               :keys "t"
               :file org-default-notes-file
               :hook my/hide
               :template ("* TODO [#%^{Priority|C|A|B|C}] %?"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":CREATED_IN_FILE: %a"
                          ":END:")
               :children (("unsorted"
                           :headline "Unsorted"
                           :keys "t")
                          ("work"
                           :keys "w"
                           :headline "Work"
                           :children (("work"
                                       :keys "w"
                                       :headline "General")
                                      ("jedi"
                                       :keys "j"
                                       :headline "Jedi")
                                      ("underconstrained"
                                       :keys "u"
                                       :headline "Underconstrained Symbolic Execution IR&D")
                                      ("chess"
                                       :keys "c"
                                       :headline "CHESS")
                                      ("vspells"
                                       :keys "v"
                                       :headline "V-SPELLS")
                                      ("vulnerability triage"
                                       :keys "V"
                                       :headline "Vulnerability Triage")))
                          ("personal"
                           :keys "p"
                           :headline "Generic")
                          ("dots"
                           :keys "d"
                           :headline "Dots")
                          ("meta"
                           :keys "m"
                           :headline "Meta")))
              ("pending"
               :keys "p"
               :file org-default-notes-file
               :headline "Pending"
               :hook my/hide
               ;; TODO: Only grab X clipboard if it's a link
               :template ("* TODO [[%x][Check status]]"
                          "DEADLINE: %t"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":CREATED_IN_FILE: %a"
                          ":END:"))
              ("email"
               :keys "e"
               :file org-default-notes-file
               :headline "Email"
               :hook my/hide
               :template ("* TODO [#%^{Priority|C|A|B|C}] [[][%{verb} email about %?]]"
                          ":PROPERTIES:"
                          ":CREATED: %U"
                          ":CREATED_IN_FILE: %a"
                          ":END:")
               :children (("reply"
                           :keys "r"
                           :verb "Reply to")
                          ("compose"
                           :keys "c"
                           :verb "Compose")
                          ("read"
                           :keys "e"
                           :verb "Read")))
              ("anki"
               :keys "a"
               :hook my/hide
               :children (("basic"
                           :keys "b"
                           :file cards-org
                           :headline "Cards"
                           :template ("* %U"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":CREATED_IN_FILE: %a"
                                      ":ANKI_DECK: Default"
                                      ":ANKI_NOTE_TYPE: Basic"
                                      ":END:"
                                      "** Front"
                                      "%?"
                                      "** Back"
                                      ""))
                          ("cloze"
                           :keys "c"
                           :file cards-org
                           :headline "Cards"
                           :template ("* %U"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":CREATED_IN_FILE: %a"
                                      ":ANKI_DECK: Default"
                                      ":ANKI_NOTE_TYPE: Cloze"
                                      ":END:"
                                      "** Text"
                                      "%?"
                                      ""))
                          ("vocab"
                           :keys "v"
                           :file vocab-org
                           :headline "Vocab"
                           :hook my/hide
                           :template ("* %?"
                                      ":PROPERTIES:"
                                      ":CREATED: %U"
                                      ":CREATED_IN_FILE: %a"
                                      ":ANKI_DECK: Default"
                                      ":ANKI_NOTE_TYPE: Basic"
                                      ":ANKI_TAGS: vocab"
                                      ":END:"
                                      "** Front"
                                      "Define:"
                                      "** Back"
                                      "Wiktionary:")))))))
