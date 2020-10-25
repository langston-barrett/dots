;;; packages.el --- alloy layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author:  <langston>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst alloy-packages
  '((alloy :location local)))

(defun alloy/init-alloy ()
  (use-package alloy
    :defer t
    :mode "\\.als\\'")
  )

;;; packages.el ends here
