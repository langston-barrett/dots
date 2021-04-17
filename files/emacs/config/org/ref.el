;; -*- lexical-binding: t; -*-
(setq reftex-default-bibliography (list (concat org-directory "/bib/references.bib")))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes (concat org-directory "/bibliography.org")
      org-ref-default-bibliography reftex-default-bibliography
      org-ref-pdf-directory (concat org-directory "/bib/"))

(setq bibtex-completion-bibliography reftex-default-bibliography
      bibtex-completion-library-path org-ref-pdf-directory
      bibtex-completion-notes-path org-ref-bibliography-notes)

(with-eval-after-load 'org-ref
  (spacemacs/set-leader-keys
    "orr" 'org-ref
    "orn" 'org-ref-open-notes-at-point))
