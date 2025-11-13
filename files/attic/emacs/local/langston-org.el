;;; langston-org.el ---
;; TODO: Gives error about "callable" variable
;; ??? -*- lexical-binding: t; -*-

;; Copyright (C) 2020, Langston Barrett

;; Author: Langston Barrett <langston.barrett@gmail.com>
;; Keywords: languages
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Package-Version: 0.1
;; Homepage: https://github.com/langston-barrett/dots

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

;; n/a
(require 'dash)
(require 'org)
(require 'subr-x)

;;; Code:

(defun my/org-element-map-file (file types fun)
  "Parse FILE and run org-element-map over its AST"
  (let ((parsed (save-excursion
                  (find-file file)
                  (org-element-parse-buffer))))
    (org-element-map parsed types fun)))


(defun my/org-element-map-agenda (types fun)
  "A version of org-element-map over the agenda files"
  (-flatten
   (-map
    (lambda (file) (my/org-element-map-file file types fun))
    (org-agenda-files))))

;; (defun my/find-agenda-item-2 (item callable)
;;   (let* ((splitted (-drop 1 (split-string (string-trim item))))
;;          ;; The following drop the TODO state and priority
;;          (joined1 (mapconcat 'identity splitted " "))
;;          (joined2 (mapconcat 'identity (-drop 1 splitted) " ")))
;;     (my/org-element-map-agenda
;;      'headline
;;      (lambda (elt)
;;        (when (let ((title (org-element-property :raw-value elt)))
;;                (or (equal title joined1)
;;                    (equal title joined2)))
;;          (list
;;           (org-element-property :begin elt)
;;           title
;;           (funcall callable elt)))))))

(defun my/find-agenda-item (item &optional scope)
  (let* ((splitted (-drop 1 (split-string (string-trim item))))
         ;; The following drop the TODO state and priority
         (joined1 (mapconcat 'identity splitted " "))
         (joined2 (mapconcat 'identity (-drop 1 splitted) " ")))
    (-filter
     (lambda (entry)
       (let ((title (org-element-property :raw-value (nth 1 entry))))
         (or (equal title joined1)
             (equal title joined2))))
     (org-map-entries
      '(lambda () (list (current-buffer) (org-element-at-point)))
      nil
      (or scope 'agenda)
      'archive ;; skip archived
      'comment)))) ;; skip comments

(setq my/org-agenda-transformers
      (list 'my/deadline-transformer
            'my/blocked-transformer))

;;(defun my/test-transformer (text element) (concat text " TEST"))

;; (defun my/trim-transformer (text element)
;;   (concat "   " (string-trim-left text)))

(defun my/org-apply-agenda-transformers-rec (text element transformers)
  (if (equal nil transformers)
      text
    (my/org-apply-agenda-transformers-rec
     (funcall (car transformers)
              text
              element)
     element
     (cdr transformers))))

(defun my/org-apply-agenda-transformers-explicit (item transformers)
  (my/org-apply-agenda-transformers-rec
   item
   (nth 1 (car (my/find-agenda-item item)))
   transformers))

(defun my/org-apply-agenda-transformers (item)
  (my/org-apply-agenda-transformers-explicit item my/org-agenda-transformers))

(defun my/deadline-transformer (text element)
  (concat
   text
   " "
   (my/org-format-deadline (org-element-property :deadline element))))

(defun my/gray-transformer (text element)
  (propertize text 'face '(:foreground "gray")))

(defun my/blocked-transformer (text element)
  (if (not (equal nil (org-element-property :BLOCKED element)))
      (my/gray-transformer text element)
      text))

(defun my/org-format-deadline (timestamp)
  (when timestamp
    (propertize
     (mapconcat
      'number-to-string
      (list
       (org-element-property :year-start timestamp)
       (org-element-property :month-start timestamp)
       (org-element-property :day-start timestamp))
      "-")
     'face
     '(:foreground "orange"))))

;; TODO: Remove these

(defun my/get-item-deadline (item)
  (org-element-property :deadline (nth 1 (car (my/find-agenda-item item)))))

(defun my/org-agenda-append-deadline (item)
  (concat
    item
    " "
    (my/org-format-deadline (my/get-item-deadline item))))

(provide 'langston-org)
;;; langston-org.el ends here
