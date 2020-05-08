;;; sadl.el --- A major mode for editing SADL files  -*- lexical-binding: t; -*-

;; Copyright (C) 2019, Langston Barrett

;; Author: Langston Barrett <langston.barrett@gmail.com>
;; Keywords: languages
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Package-Version: 0.5
;; Homepage: https://github.com/langston-barrett/sadl-mode

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

;; This is a major mode for editing SADL files.

;;; Code:

(require 'compile)
(require 'cl-lib)

;;; Configuration

(defgroup sadl '()
  "SADL"
  :group 'languages
  :tag "SADL")

(defface sadl-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "How to highlight SADL keywords."
  :group 'sadl)

(defface sadl-operator-face
  '((t (:inherit font-lock-keyword-face)))
  "How to highlight SADL build-in operators."
  :group 'sadl)

(defface sadl-type-face
  '((t (:inherit font-lock-type-face)))
  "How to highlight SADL build-in operators."
  :group 'sadl)

;;; Highlighting

(defconst sadl-keywords
  '("is"
    "a"
    "are"
    "described"
    "by"
    "of"
    "an"
    "type"
    "class"
    "value"
    "values"
    "are"
    "not"
    "the"
    "with"
    "single"
    "same"))

(defvar sadl--keyword-regexp
  (regexp-opt sadl-keywords 'words)
  "Regular expression for SADL keyword highlighting.")

(defconst sadl-operators
  '("." ",")
  "Operators to highlight in SADL.")

(defvar sadl--operator-regexp
  (regexp-opt sadl-operators)
  "Regular expression for SADL keyword highlighting.")

(defconst sadl-types
  '("string"
    "date"
    "float"))

(defvar sadl--type-regexp
  (regexp-opt sadl-types 'words)
  "Regular expression for SADL type highlighting.")

(defvar sadl-font-lock-defaults
  `(((,sadl--keyword-regexp . 'sadl-keyword-face)
     (,sadl--operator-regexp . 'sadl-operator-face)
     (,sadl--type-regexp . 'sadl-type-face)
  ;; ("@\w+" . 'font-lock-function-name-face)
     )
    nil nil nil
    (font-lock-extend-after-change-region-function . sadl--extend-after-change-region-function))
  "Highlighting instructions for SADL.")

;;; Default keybindings

(defvar sadl-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-c") 'sadl-run-current-buffer)
    map)
  "Keymap for SADL mode.")

;;; The mode itself

;;;###autoload
(define-derived-mode sadl-mode prog-mode "SADL"
  "A major mode for editing SADL files."
  (setq font-lock-defaults sadl-font-lock-defaults)
  (setq font-lock-multiline t)

  ;; Comment syntax
  (setq-local comment-start "// ")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sadl\\'" . sadl-mode))

(provide 'sadl)
;;; sadl.el ends here
