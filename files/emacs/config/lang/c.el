;; -*- lexical-binding: t; -*-
;;; C and C++

(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-include-path
                           (list (expand-file-name "~/code/your-c-project-goes-here")))))

(require 'llvm-mode nil 'noerror)

;;; Formatting

;; TODO: not sure the below works

;; https://bit.ly/2QqJLvw

;; (defun my/clang-format-buffer-smart ()
;;   "Reformat buffer if .clang-format exists in the projectile root."
;;   (interactive)
;;   (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
;;     (clang-format-buffer)))

;; (defun my/clang-format-buffer-smart-on-save ()
;;   "Add auto-save hook for clang-format-buffer-smart."
;;   (add-hook 'before-save-hook 'my/clang-format-buffer-smart nil t))

;; (spacemacs/add-to-hooks '(lambda () (add-hook 'before-save-hook 'clang-format-buffer nil t))
;;                         '(c-mode-hook c++-mode-hook))

;;; CFlow-Mode

(when (require 'cflow-mode nil 'noerror)
  (spacemacs/set-leader-keys-for-major-mode 'cflow-mode
    "qg" 'cflow-find-function
    "qs" 'cflow-find-function
    "qr" 'cflow-find-recursion-root
    "qR" 'cflow-find-recursion-next
    "qx" 'cflow-goto-expand
    "qE" 'cflow-edit-out-full)
  (autoload 'cflow-mode "cflow-mode")
  (setq auto-mode-alist (append auto-mode-alist
                                '(("\\.cflow$" . cflow-mode)))))

;;; Importing

(defconst libc-headers
  '("<assert.h>"
    "<complex.h>"
    "<ctype.h>"
    "<errno.h>"
    "<fenv.h>"
    "<float.h>"
    "<inttypes.h>"
    "<iso646.h>"
    "<limits.h>"
    "<locale.h>"
    "<math.h>"
    "<setjmp.h>"
    "<signal.h>"
    "<stdalign.h>"
    "<stdarg.h>"
    "<stdatomic.h>"
    "<stdbool.h>"
    "<stddef.h>"
    "<stdint.h>"
    "<stdio.h>"
    "<stdlib.h>"
    "<stdnoreturn.h>"
    "<string.h>"
    "<tgmath.h>"
    "<threads.h>"
    "<time.h>"
    "<uchar.h>"
    "<wchar.h>"
    "<wctype.h>"))

(defconst stl-headers
  '("<concepts>"
    "<cstdlib>"
    "<csignal>"
    "<csetjmp>"
    "<cstdarg>"
    "<typeinfo>"
    "<typeindex>"
    "<type_traits>"
    "<bitset>"
    "<functional>"
    "<utility>"
    "<ctime>"
    "<chrono>"
    "<cstddef>"
    "<initializer_list>"
    "<tuple>"
    "<any>"
    "<optional>"
    "<variant>"
    "<compare>"
    "<version>"
    "<new>"
    "<memory>"
    "<scoped_allocator>"
    "<memory_resource>"
    "<climits>"
    "<cfloat>"
    "<cstdint>"
    "<cinttypes>"
    "<limits>"
    "<exception>"
    "<stdexcept>"
    "<cassert>"
    "<system_error>"
    "<cerrno>"
    "<contract>"
    "<cctype>"
    "<cwctype>"
    "<cstring>"
    "<cwchar>"
    "<cuchar>"
    "<string>"
    "<string_view>"
    "<charconv>"
    "<array>"
    "<vector>"
    "<deque>"
    "<list>"
    "<forward_list>"
    "<set>"
    "<map>"
    "<unordered_set>"
    "<unordered_map>"
    "<stack>"
    "<queue>"
    "<span>"
    "<iterator>"
    "<ranges>"
    "<algorithm>"
    "<execution>"
    "<cmath>"
    "<complex>"
    "<valarray>"
    "<random>"
    "<numeric>"
    "<ratio>"
    "<cfenv>"
    "<bit>"
    "<iosfwd>"
    "<ios>"
    "<istream>"
    "<ostream>"
    "<iostream>"
    "<fstream>"
    "<sstream>"
    "<syncstream>"
    "<strstream>"
    "<iomanip>"
    "<streambuf>"
    "<cstdio>"
    "<locale>"
    "<clocale>"
    "<codecvt>"
    "<regex>"
    "<atomic>"
    "<thread>"
    "<mutex>"
    "<shared_mutex>"
    "<future>"
    "<condition_variable>"
    "<filesystem>"))

(defun my/libc-include (header-name)
  "An interactive prompt for the extension (EXT-NAME) to add."
  (interactive (list (completing-read "libc header: " libc-headers)))
  (evil-open-below 1)
  (evil-beginning-of-line)
  (insert (concat "#include " header-name)))

(defun my/c++-include (header-name)
  "An interactive prompt for the extension (EXT-NAME) to add."
  (interactive (list (completing-read "libc header: "
                                      (append libc-headers stl-headers))))
  (evil-open-below 1)
  (evil-beginning-of-line)
  (insert (concat "#include " header-name)))

(spacemacs/set-leader-keys-for-major-mode 'c-mode
  "ih" 'my/libc-include)

(spacemacs/set-leader-keys-for-major-mode 'c++-mode
  "ih" 'my/c++-include)
