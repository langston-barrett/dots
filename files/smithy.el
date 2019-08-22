;;; smithy.el --- A major mode for editing Smithy files  -*- lexical-binding: t; -*-

;; Copyright (C) 2019, Langston Barrett

;; Author: Langston Barrett <langston.barrett@gmail.com>
;; Keywords: languages
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Package-Version: 0.5
;; Homepage: https://github.com/langston-barrett/smithy-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a major mode for editing Smithy files.

;;; Code:

(require 'compile)
(require 'cl-lib)

;;; Configuration

(defgroup smithy '()
  "Smithy"
  :group 'languages
  :tag "Smithy")

(defface smithy-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "How to highlight Smithy keywords."
  :group 'smithy)

(defface smithy-operator-face
  '((t (:inherit font-lock-keyword-face)))
  "How to highlight Smithy build-in operators."
  :group 'smithy)

;;; Highlighting

(defconst smithy-keywords
  '(;; named types
    "structure"
    "union"

    ;; meta
    "$version"

    ;; modules
    "namespace"
    "use"

    "service"
    "resource"
    "operation"
    ))

(defconst smithy-types
  '("structure"
    "namespace"
    "use"
    "$version"))

(defvar smithy--keyword-regexp
  (regexp-opt smithy-keywords 'words)
  "Regular expression for Smithy keyword highlighting.")

(defconst smithy-operators
  '(":" "#" "->")
  "Operators to highlight in Smithy.")

(defvar smithy--operator-regexp
  (regexp-opt smithy-operators)
  "Regular expression for Smithy keyword highlighting.")

(defvar smithy-font-lock-defaults
  `(((,smithy--keyword-regexp . 'smithy-keyword-face)
     (,smithy--operator-regexp . 'smithy-operator-face)
     ;; ("@\w+" . 'font-lock-function-name-face)
     )
    nil nil nil
    (font-lock-extend-after-change-region-function . smithy--extend-after-change-region-function))
  "Highlighting instructions for Smithy.")

;;; Default keybindings

(defvar smithy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-c") 'smithy-run-current-buffer)
    ;; (define-key map (kbd "C-c C-l") 'smithy-run-current-buffer)
    ;; (define-key map (kbd "<mouse-3>") 'prop-menu-show-menu)
    ;; (define-key map (kbd "C-c C-SPC") 'prop-menu-by-completing-read)
    map)
  "Keymap for Smithy mode.")

;;; The mode itself

;;;###autoload
(define-derived-mode smithy-mode prog-mode "Smithy"
  "A major mode for editing Smithy files."
  (setq font-lock-defaults smithy-font-lock-defaults)
  (setq font-lock-multiline t)

  ;; Comment syntax
  (setq-local comment-start "// ")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.smithy\\'" . smithy-mode))

(provide 'smithy)
;;; smithy.el ends here
