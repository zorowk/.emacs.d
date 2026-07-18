;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Run before package activation, frame setup, and init.el.

;;; Code:

(when (version< emacs-version "31.1")
  (error "This configuration requires Emacs 31.1 or newer"))

;; Stabilize frame geometry before init.el loads the selected theme.
(setq frame-inhibit-implied-resize t)

;; A bounded startup threshold avoids collections without allowing unbounded
;; allocation if initialization fails.
(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.6)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun zoro-restore-startup-state ()
  "Restore GC and file handling after loading the init file."
  (setq gc-cons-threshold (* 16 1024 1024)
        gc-cons-percentage 0.1)
  (when (boundp 'file-name-handler-alist-original)
    (setq file-name-handler-alist file-name-handler-alist-original)
    (makunbound 'file-name-handler-alist-original)))

(add-hook 'after-init-hook #'zoro-restore-startup-state)

(setq site-run-file nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(provide 'early-init)
;;; early-init.el ends here
