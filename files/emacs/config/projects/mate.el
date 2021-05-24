;; -*- lexical-binding: t; -*-
;;; MATE

(eval-when-compile
  (require 'cl-lib))

(defun my/mate-docker-run (cmd)
  (let ((default-directory (projectile-project-root)))
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
  (completing-read
   "Select file:"
   (cl-remove-if-not
    (lambda (path) (string-suffix-p suffix path))
    (directory-files (projectile-project-root)))))

(defun my/select-cflags-dynamic (str)
  (let* ((split (split-string str))
         (rest (butlast split))
         (last-word (car (last split))))
    (cond
     ((string-prefix-p "-W" last-word)
      (mapcar
       (lambda (flag) (string-join (append rest (list flag)) " "))
       '("-Werror" "-Wall")))
     (t nil))))

(defun my/select-cflags ()
  (completing-read-multiple
   "CFLAGS: "
   (completion-table-dynamic #'my/select-cflags-dynamic)
   nil                                  ; predicate
   nil                                  ; require-match
   "-fno-discard-value-names,-Wall,-O1" ; initial value
   nil))

(defun my/mate-docker-run-and-then (cmd rest)
  (my/mate-docker-run cmd)
  ;; There is also compilation-in-progress...
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

(defun my/mate-compile-to-bitcode (file cflags)
  (interactive
   (list
    (my/select-file-with-suffix-from-project-root ".c")
    (my/select-cflags)))
  ;; TODO:
  ;; - Default to current buffer file if it's a C file
  (my/mate-docker-run-and-then
   (concat "clang -fno-discard-value-names -emit-llvm " (string-join cflags " ") " -c "  file)
   (lambda ()
     (my/mate-docker-run-and-then
      (concat "llvm-dis-10 " (concat (file-name-sans-extension file) ".bc"))
      (lambda ()
        (find-file-existing (concat (file-name-sans-extension file) ".ll")))))))

(defsubst my/directory-files-with-prefix (prefix dir)
  (cl-loop
   for file in (directory-files dir)
   if (string-prefix-p "assert_" file)
   collect (concat (file-name-as-directory dir) file)))

;; Why does this not work...?
;; (defsubst my/file-size (file) (file-attribute-size (file-attributes file)))

(defsubst my/file-size (file)
  (with-temp-buffer
    (insert-file-contents file)
    (- (point-max) 1)))

(defsubst my/filter-nonempty (files)
  "Find non-empty files in the list FILES."
  (cl-loop for file in files if (< 0 (my/file-size file)) collect file))

;; TODO: Add a timestamp
(defun my/mate-run-pointer-analysis (file)
  (interactive
   (list (my/select-file-with-suffix-from-project-root ".bc")))
  (let* ((root (projectile-project-root))
         (results
          (expand-file-name
           (concat root "/.out/cache/pointer-analysis/" file ".results/"))))
    (my/mate-shake-and-then
     (concat "run-souffle -- -- 1-callsite " file)
     (lambda ()
       (let ((failing (my/filter-nonempty
                       (my/directory-files-with-prefix "assert_" results)))
             (buf-name (format "*pointer analysis %s*" file)))
         (dolist (file failing)
           (message "%s %s" "Assertion failed!" file))
         (funcall-interactively #'dired results)
         (kill-buffer (get-buffer-create buf-name))
         (switch-to-buffer (get-buffer-create buf-name))
         (org-mode)
         (insert
          (format "* Pointer Analysis for %s" file)
          "\n\n"
          (format "Results: [[%s]]" results)
          "\n\n"
          (format "** Failing Assertions")
          "\n\n"
          (mapconcat
           (lambda (file-name)
             (format
              "- [[%s][%s]]"
              file-name
              (file-name-nondirectory file-name)))
           failing
           "\n")))))))

;; (defun my/mate-compile-to-bitcode-then-run-pointer-analysis (file)
;;   (interactive
;;    (list (my/select-file-with-suffix-from-project-root ".c")))
;;   (my/mate-compile-to-bitcode file)
;;   (my/mate-run-pointer-analysis (concat (file-name-sans-extension file) ".bc")))

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
