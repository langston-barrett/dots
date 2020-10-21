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

(defun my/find-agenda-item (item callable &optional scope)
  (-filter
    (lambda (entry)
      (or
      ;; TODO repetition
      ;; The following drop the TODO state and priority
      (equal
        (caddr entry)
        (mapconcat 'identity (-drop 1 (split-string (string-trim item))) " "))
      (equal
        (caddr entry)
        (mapconcat 'identity (-drop 2 (split-string (string-trim item))) " "))))
    (org-map-entries
    '(lambda ()
        (let ((elt (org-element-at-point)))
          (list
          (current-buffer)
          (org-element-property :begin elt)
          (org-element-property :raw-value elt)
          (funcall callable (org-element-at-point)))))
    nil
    (or scope 'agenda)
    'archive ;; skip archived
    'comment))) ;; skip comments

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
