(defun cabal (tgt)
  (concat "cabal " tgt))

(defun makej (tgt)
  (concat "cabal " tgt))

(defun andand (&rest strs)
  (mapconcat 'identity strs " && "))

(defun my/uc-crux-lint ()
  (interactive)
  (let ((default-directory (concat (projectile-project-root) "/uc-crux-llvm")))
    (compile "make -j lint")))

(defun my/uc-crux-typecheck ()
  (interactive)
  (let ((default-directory (concat (projectile-project-root) "/uc-crux-llvm")))
    (compile
     (mapconcat
      'identity
      (list
       "cabal"
       "repl"
       "--repl-options=-fno-code"
       "--repl-options=-fno-break-on-exception"
       "--repl-options=-fno-break-on-error"
       "--repl-options=-v1"
       "--repl-options=-ferror-spans"
       "--repl-options=-j"
       "lib:uc-crux-llvm")
      " "))))

(defun my/uc-crux-test ()
  (interactive)
  (let ((default-directory (concat (projectile-project-root) "/uc-crux-llvm")))
    (compile "cabal test")))
