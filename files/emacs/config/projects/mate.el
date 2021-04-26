;; -*- lexical-binding: t; -*-
;;; MATE

(defun mate-shake (tgt)
  ;; TODO: Handle multi-hop TRAMP
  (let ((default-directory (my/tramp-add-sudo default-directory)))
    (compile
     (mapconcat
      'identity
      `("docker run --rm --net=host"
        "--mount type=bind,src=$PWD,dst=/x"
        "--workdir /x"
        "mate-dev"
        "./shake.sh -j4"
        ,tgt)
      " "))))

(defun my/mate-lint ()
  (interactive)
  (mate-shake "lint"))

(defun my/mate-format ()
  (interactive)
  (mate-shake "format"))

(defun my/mate-build ()
  (interactive)
  (mate-shake "build"))

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
