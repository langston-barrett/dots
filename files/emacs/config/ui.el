;; -*- lexical-binding: t; -*-

;;; Imports

(require 'contract)
(my/load "ui/olivetti.el")

;;; Generic

;; bigger fringes
(setq-default left-fringe-width 16)
(setq-default right-fringe-width 16)

;; fake header line for space between top of frame and buffer text
(setq header-line-format " ")

;;; Dimmer

(defun my/activate-dimmer ()
  (require 'dimmer)
  (setq dimmer-fraction 0.03)
  (setq dimmer-adjustment-mode :background)
  (dimmer-mode))

(my/when-idle-low 'my/activate-dimmer)

;;; Evil Fringe Marks

(defun my/activate-fringe-marks ()
  (require 'evil-fringe-mark)
  (global-evil-fringe-mark-mode))

(my/when-idle-low 'my/activate-fringe-marks)

;;; Golden-Ratio Mode (Unused)

;; (with-eval-after-load 'golden-ratio-mode
;;   (when (boundp 'golden-ratio-exclude-modes)
;;     (add-to-list 'golden-ratio-exclude-modes "pdf-view-mode"))
;;   (golden-ratio-mode)
;;   (setq golden-ratio-adjust-factor .90
;;         golden-ratio-wide-adjust-factor .90))

;;; Darkroom (Unused)

;; (with-eval-after-load 'darkroom
;;   (setq darkroom-text-scale-increase 0.5)
;;   (add-hook 'markdown-mode-hook 'darkroom-tentative-mode))

;;; Spaceline

(with-eval-after-load 'spaceline
  (setq powerline-scale 1.0)
  (setq spaceline-purpose-p nil)
  (setq spaceline-major-mode-p nil)
  (setq spaceline-minor-modes-p nil)
  (setq spaceline-buffer-size-p nil)
  (setq spaceline-buffer-position-p nil)
  (setq spaceline-buffer-encoding-abbrev-p nil))

;;; Automatic Recomputing of Font Size

;; Essential for multi-monitor support!

(contract-defun
 my/compute-font-size (frame)
 :contract (contract-> contract-any-c contract-nat-number-c)
 "Inspired by https://emacs.stackexchange.com/a/44930/17066. FRAME is ignored."
 ;; Using display names is unreliable...switched to checking the resolution
 (let* ((attrs (frame-monitor-attributes)) ;; gets attribs for current frame
        (width-mm (nth 1 (nth 3 attrs)))
        (width-px (nth 3 (nth 2 attrs))))
   (cond ((eq width-px 1920) 8)
         ((eq width-px 2560) 8)
         ((> width-px 6000) 14)
         (t 12))))

(defun my/set-frame-font-size (sz)
  (interactive "nEnter font size: ")
  (set-frame-font (format "Hack %s" sz)))

(defun my/recompute-font-size ()
  (interactive)
  (my/set-frame-font-size
   (my/compute-font-size nil)))

(contract-defun
 my/frame-width-px ()
 :contract (contract-> contract-nat-number-c)
 (let* ((attrs (frame-monitor-attributes))
        (width-px (nth 3 (nth 2 attrs))))
   width-px))

(add-hook 'window-size-change-functions #'my/compute-font-size)
