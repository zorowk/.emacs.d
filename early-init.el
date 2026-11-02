;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Run before package activation, frame setup, and init.el.

;;; Code:

(when (version< emacs-version "31.0.90")
  (error "This configuration requires Emacs 31.0.90 or newer"))

(defvar zoro-system-themes
  '((light . ef-frost)
    (dark . ef-autumn))
  "Themes selected for light and dark system appearances.")

(defun zoro-apply-system-theme (&optional appearance)
  "Load the configured theme for APPEARANCE or the current system.
Defer loading when the theme package is not available yet."
  (let* ((appearance (or appearance
                         (bound-and-true-p ns-system-appearance)
                         'light))
         (theme (alist-get appearance zoro-system-themes)))
    (unless theme
      (error "No theme configured for system appearance `%s'" appearance))
    (if (memq theme (custom-available-themes))
        (progn
          (mapc #'disable-theme custom-enabled-themes)
          (load-theme theme t))
      (message "Theme `%s' is not available yet; deferring" theme)
      nil)))

;; Packages are active by `before-init-hook', while the initial graphical
;; frame has not yet been created.
(add-hook 'before-init-hook #'zoro-apply-system-theme)

;; A bounded startup threshold avoids collections without allowing unbounded
;; allocation if initialization fails.
(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.6)

(defun zoro-restore-startup-state ()
  "Restore regular GC settings after loading the init file."
  (setq gc-cons-threshold (* 16 1024 1024)
        gc-cons-percentage 0.1))

(add-hook 'after-init-hook #'zoro-restore-startup-state)

(setq site-run-file nil)

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(provide 'early-init)
;;; early-init.el ends here
