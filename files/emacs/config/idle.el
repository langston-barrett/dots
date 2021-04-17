;; -*- lexical-binding: t; -*-
;; For functions to run that don't need to be run immediately
(defconst my/idle-time-low-priority 2)
(defconst my/idle-time-med-priority 1)
(defun my/when-idle (time func &rest args)
  (run-with-idle-timer time nil func))
(defun my/when-idle-low (func &rest args)
  (my/when-idle my/idle-time-low-priority func))
(defun my/when-idle-med (func &rest args)
  (my/when-idle my/idle-time-low-priority func))
