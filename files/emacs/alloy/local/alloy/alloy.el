;;; alloy.el --- A major mode for editing Alloy files  -*- lexical-binding: t; -*-

;; Copyright (C) 2019, Langston Barrett

;; Author: Langston Barrett <langston.barrett@gmail.com>
;; Keywords: languages
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Package-Version: 0.5
;; Homepage: https://github.com/langston-barrett/alloy-mode

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

;; This is a major mode for editing Alloy files.

;;; Code:

(require 'compile)
(require 'cl-lib)

;;; Configuration

(defgroup alloy '()
  "Alloy"
  :group 'languages
  :tag "Alloy")

(defface alloy-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "How to highlight Alloy keywords."
  :group 'alloy)

(defface alloy-constant-face
  '((t (:inherit font-lock-constant-face)))
  "How to highlight Alloy built-in constants."
  :group 'alloy)

(defface alloy-operator-face
  '((t (:inherit font-lock-builtin-face)))
  "How to highlight Alloy built-in operators."
  :group 'alloy)

(defface alloy-type-face
  '((t (:inherit font-lock-constant-face)))
  "How to highlight types (names of signatures)."
  :group 'alloy)

;;; Highlighting

(defconst alloy-keywords
  '(;; Declarations
    "abstract"
    "assert"
    "extends"
    "fun"
    "pred"
    "sig"

    "run"
    "check"

    ;; Multiplicity
    "all"
    "lone"
    "one"
    "set"
    "some"
    ))

(defvar alloy--keyword-regexp
  (regexp-opt alloy-keywords 'words)
  "Regular expression for Alloy keyword highlighting.")

(defconst alloy-constants
  '("ident"
    "univ"
    "empty" ; TODO is this right?
    ))

(defvar alloy--constant-regexp
  (regexp-opt alloy-constants 'words)
  "Regular expression for Alloy keyword highlighting.")

(defconst alloy-operators
  '("." "->" "~" "*" "^" "+" "-" "&" ":>" "<:") ; TODO others?
  "Operators to highlight in Alloy.")

(defvar alloy--operator-regexp
  (regexp-opt alloy-operators)
  "Regular expression for Alloy operator highlighting.")

(defvar alloy--type-regexp
  (rx (char upper) (+ alphanumeric))
  "Regular expression for Alloy operator highlighting.")

(defvar alloy-font-lock-defaults
  `(((,alloy--keyword-regexp . 'alloy-keyword-face)
     (,alloy--constant-regexp . 'alloy-constant-face)
     ;; (,alloy--operator-regexp . 'alloy-operator-face)
     ;; (,alloy--type-regexp . 'alloy-type-face)
     )
    nil nil nil
    (font-lock-extend-after-change-region-function . alloy--extend-after-change-region-function))
  "Highlighting instructions for Alloy.")

;;; Default keybindings

(defvar alloy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "C-c C-c") 'alloy-run-current-buffer)
    ;; (define-key map (kbd "C-c C-l") 'alloy-run-current-buffer)
    ;; (define-key map (kbd "<mouse-3>") 'prop-menu-show-menu)
    ;; (define-key map (kbd "C-c C-SPC") 'prop-menu-by-completing-read)
    map)
  "Keymap for Alloy mode.")

;;; The mode itself

;;;###autoload
(define-derived-mode alloy-mode prog-mode "Alloy"
  "A major mode for editing Alloy files."
  (setq font-lock-defaults alloy-font-lock-defaults)
  (setq font-lock-multiline t)

  ;; Comment syntax
  (setq-local comment-start "// ")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.als\\'" . alloy-mode))

(provide 'alloy)
;;; alloy.el ends here
