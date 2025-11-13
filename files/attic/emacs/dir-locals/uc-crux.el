((haskell-mode
  .
  ((eval
   . (progn
       (load "~/code/dots/files/emacs/dir-locals/uc-crux-funcs.el")
       (add-hook 'after-save-hook #'my/uc-crux-lint nil t))))))
