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

(defun my/find-agenda-item (item callable &optional scope)
  (let* ((splitted (-drop 1 (split-string (string-trim item))))
         ;; The following drop the TODO state and priority
         (joined1 (mapconcat 'identity splitted " "))
         (joined2 (mapconcat 'identity (-drop 1 splitted) " ")))
    (-filter
     (lambda (entry)
       (let ((title (caddr entry)))
         (or (equal title joined1)
             (equal title joined2))))
     (org-map-entries
      '(lambda ()
         (let ((elt (org-element-at-point)))
           (list
            (current-buffer)
            (org-element-property :begin elt)
            (org-element-property :raw-value elt)
            (funcall callable elt))))
      nil
      (or scope 'agenda)
      'archive ;; skip archived
      'comment)))) ;; skip comments

(defun my/get-item-deadline (item)
  (nth 3 (car
          (my/find-agenda-item
            item
            (lambda (element) (org-element-property :deadline element))))))

(defun my/org-format-deadline (timestamp)
  (when timestamp
    (mapconcat
     'number-to-string
     (list
      (org-element-property :year-start timestamp)
      (org-element-property :month-start timestamp)
      (org-element-property :day-start timestamp))
     "-")))

(defun my/org-agenda-append-deadline (item)
  (concat
    item
    " "
    (my/org-format-deadline (my/get-item-deadline item))))

(provide 'langston-org)
;;; langston-org.el ends here
