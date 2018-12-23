;;; emojify-org-link.el --- Add the appropriate emoji to org links -*- lexical-binding: t -*-

;; Copyright (C) 2018 Langston Barrett
;; Author: Langston Barrett <langston.barrett@gmail.com>
;; URL: https://github.com/siddharthist
;; Keywords: emoji, org
;; Package-Requires: ()
;; Version: 0.1

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
;;

;;; History:
;;
;; 2018-11-27: Initial work

;;; Code:
;;

(require 'org)

;; TODO: will loop infinitely...
(defun emojify-org-link/char-before-point (c)
  "Find the first position of C before the point"
  (progn
    (setq-local pos (point))
    (while (not (equal (string (char-before pos)) c))
      (setq-local pos (- pos 1))))
  (- pos 1))

;; TODO: will loop infinitely...
;; TODO: not the best if the point is on a space
(defun emojify-org-link/char-after-point (c)
  "Find the first position of C after the point"
  (progn
    (setq-local pos (point))
    (while (not (equal (string (char-after pos)) c))
      (setq-local pos (+ pos 1))))
  pos)

(defun emojify-org-link/org-link-under-point ()
  "Is the point over an org-link?"
  (let ((beg (emojify-org-link/char-before-point " "))
        (end (emojify-org-link/char-after-point  " ")))
    (string-match org-any-link-re (buffer-substring beg end))))

(defun emojify-org-link/get-link-content (beg end)
  "Get the content (URL) of an org link between BEG and END"
  (save-excursion
    (goto-char beg)
    (let ((link-content-beg (+ (emojify-org-link/char-after-point "[") 1))
          (link-content-end (- (emojify-org-link/char-after-point "]") 1)))
      (buffer-substring link-content-beg link-content-end))))

(defun emojify-org-link/get-link-text (beg end)
  "Get the text of an org link between BEG and END"
  (save-excursion
    (goto-char end)
    (let ((link-text-beg (+ (emojify-org-link/char-before-point "[") 1))
          (link-text-end (- (emojify-org-link/char-before-point "]") 1)))
      (buffer-substring link-text-beg link-text-end))))

(defun emojify-org-link/get-link-pair (beg end)
  "Get a pair where the head is the org-link content (URL) and the tail is the text"
  ((emojify-org-link/get-link-content beg end) .
   (emojify-org-link/get-link-text beg end)))

;; A sequence of triples.
;; The 1st element is the URL regex.
;; The 2nd element is the text regex.
;; The 3nd element is the emoji to insert for matching links.
(setq-default emojify-org-link-alist
              (list (list "https*://" ".*" "🌎 ")
                    (list "file:"     ".*" "📁 ")))

(defun emojify-org-link ()
  "Insert the appropriate emoji for an org-link"
  (interactive)
  (when (emojify-org-link/org-link-under-point)
    (save-excursion
      (goto-char (emojify-org-link/char-after-point  " "))
      (goto-char (+ 1 (emojify-org-link/char-before-point "[")))
      (let* ((beg (emojify-org-link/char-before-point " "))
             (end (emojify-org-link/char-after-point " "))
             (url (emojify-org-link/get-link-content beg end))
             (txt (emojify-org-link/get-link-text beg end)))
        (dolist (triple emojify-org-link-alist)
          (when (and (string-match (car triple) url)
                     (string-match (cadr triple) txt))
            (insert (caddr triple))))))))

(provide 'emojify-org-link)
