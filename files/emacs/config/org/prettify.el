;; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(add-hook 'org-mode-hook 'variable-pitch-mode)
;; I think this has some performance issues:
;; (add-hook 'org-mode-hook 'org-pretty-table-mode)
(setq org-hide-emphasis-markers t)

(set-face-attribute 'variable-pitch nil :family "Open Sans" :inherit '(default))
;; (set-face-attribute 'variable-pitch nil :family "DejaVu Sans" :inherit '(default))
;; (set-face-attribute 'variable-pitch nil :family "Lato" :inherit '(default))

;; https://stackoverflow.com/questions/3758139/variable-pitch-for-org-mode-fixed-pitch-for-tables#3761399
(require 'cl-lib)
(defun my/adjoin-to-list-or-symbol (element list-or-symbol)
  (let ((list (if (not (listp list-or-symbol))
                  (list list-or-symbol)
                list-or-symbol)))
    (cl-adjoin element list)))

(mapc
 (lambda (face)
   (set-face-attribute
    face nil
    :inherit
    (my/adjoin-to-list-or-symbol
     'fixed-pitch
     (face-attribute face :inherit))))
 (list 'org-code
       'org-link
       'org-block
       'org-table
       'org-block-begin-line
       'org-block-end-line
       'org-meta-line
       'org-document-info-keyword))

;; (let ((font (cond
;;              ((x-list-fonts "Overpass") "Overpass")
;;              ((x-list-fonts "Lucida") "Lucida")
;;              ((x-list-fonts "Libre Caslon Text") "Libre Caslon Text")
;;              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro.")))))
;;   (mapc
;;    (lambda (face) (set-face-attribute face nil :font font))
;;    (list 'org-level-1
;;          'org-level-2
;;          'org-level-3
;;          'org-level-4
;;          'org-level-5
;;          'org-level-6
;;          'org-level-7
;;          'org-level-8)))

(add-hook 'org-mode-hook 'my/hide)
