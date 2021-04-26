;; -*- lexical-binding: t; -*-
;;; MATE

(defun my/mate-docker-run (cmd)
  (let ((default-directory (my/tramp-add-sudo (projectile-project-root))))
    (compile
     (mapconcat
      'identity
      `("docker run --rm --net=host"
        "--mount type=bind,src=$PWD,dst=/x"
        "--workdir /x"
        "mate-dev"
        ,cmd)
      " "))))

(defun my/select-file-with-suffix-from-project-root (suffix)
  (let* ((root (projectile-project-root))
         (files (directory-files root))
         (suffix-files (cl-remove-if-not
                        (lambda (path) (string-suffix-p suffix path))
                        files)))
    (completing-read "Select file:" suffix-files)))

(defun my/mate-docker-run-and-then (cmd rest)
  (my/mate-docker-run cmd)
  (let ((proc (get-buffer-process (get-buffer "*compilation*"))))
    (when proc
      (set-process-sentinel
       proc
       (lambda (p _event)
         (when (= 0 (process-exit-status p))
           (funcall rest)))))))

(defconst my/shake-str "./shake.sh -j8 ")

(defun mate-shake (tgt)
  (my/mate-docker-run (concat my/shake-str tgt)))

(defun my/mate-shake-and-then (tgt rest)
  (my/mate-docker-run-and-then (concat my/shake-str tgt) rest))

(defun my/mate-compile-to-bitcode (file)
  (interactive
   (list (my/select-file-with-suffix-from-project-root ".c")))
  ;; TODO:
  ;; - Default to current buffer file if it's a C file
  (my/mate-docker-run-and-then
   (concat "clang -O1 -emit-llvm -c " file)
   (lambda ()
     (my/mate-docker-run-and-then
      (concat "llvm-dis-10 " (concat (file-name-sans-extension file) ".bc"))
      (lambda ()
        (find-file (concat (file-name-sans-extension file) ".ll")))))))

(defun my/mate-run-pointer-analysis (file)
  (interactive
   (list (my/select-file-with-suffix-from-project-root ".bc")))
  (lexical-let ((root (projectile-project-root))
                (file file))
    (my/mate-shake-and-then
     (concat "run-souffle -- -- 1-callsite " file)
     (lambda ()
       (funcall-interactively
        #'dired
        (concat root "/.out/cache/pointer-analysis/" file ".results/"))))))

(defun my/mate-lint ()
  (interactive)
  (mate-shake "lint"))

(defun my/mate-format ()
  (interactive)
  (mate-shake "format"))

(defun my/mate-build ()
  (interactive)
  (mate-shake "build"))

(defun my/mate-pytests-fast ()
  (interactive)
  (mate-shake "pytests -- -- -x"))

(defun my/mate-pytests-one (pattern)
  (interactive "MPattern: ")
  (mate-shake (concat "pytests -- -- -x -k " pattern)))

(defun my/mate-lint-jump ()
  (interactive)
  (mate-shake "lint")
  (switch-to-buffer (compilation-find-buffer))
  (goto-char 0)
  (y-or-n-p-with-timeout "Ready to go to error?" 5 nil)
  (compilation-next-error 1)
  (compile-goto-error))

(defun my/next-compilation-error ()
  (interactive)
  (switch-to-buffer (compilation-find-buffer))
  (goto-char 0)
  (compilation-next-error 1)
  (compile-goto-error))

(defun my/mate-lint-jump-no-prompt ()
  (interactive)
  (mate-shake "lint")
  (switch-to-buffer (compilation-find-buffer))
  (while (not (equal nil compilation-in-progress))
    (sleep-for 0.25))
  (if (equal nil (list 0))
      (progn
        (message "Compilation was successful!")
        (kill-buffer))
    (my/next-compilation-error)))
