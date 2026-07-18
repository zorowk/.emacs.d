;;; init-ui.el --- Core appearance and frame behavior -*- lexical-binding: t -*-

;; Derived from M-EMACS configuration by zorowk.
;; Copyright (C) 2019 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure the base theme, fonts, frames, display, and scrolling.

;;; Code:

(declare-function zoro-dashboard-update-banner "init-dashboard" (appearance))

(defun zoro-apply-theme (appearance)
  "Load the theme matching system APPEARANCE."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'ef-frost t))
    ('dark (load-theme 'ef-autumn t)))
  (when (featurep 'dashboard)
    (zoro-dashboard-update-banner appearance)))

(use-package ef-themes
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq modus-themes-mixed-fonts t
        modus-themes-italic-constructs t)
  (add-hook 'ns-system-appearance-change-functions #'zoro-apply-theme)
  (zoro-apply-theme 'light))

(defun zoro-change-font ()
  "Apply the configured fixed, variable, symbol, emoji, and CJK fonts."
  (let ((font-height (if (eq system-type 'darwin) 150 110)))
    (set-face-attribute 'default nil
                        :family "JetBrains Mono"
                        :height font-height))
  (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 1.0)
  (if (eq system-type 'darwin)
      (set-face-attribute 'variable-pitch nil :family "Georgia" :height 1.0)
    (set-face-attribute 'variable-pitch nil :family "Gelasio" :height 1.0))
  (if (eq system-type 'darwin)
      (progn
        (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
        (set-fontset-font t 'symbol (font-spec :family "STIX Two Math"))
        (set-fontset-font t 'greek (font-spec :family "Apple Symbols"))
        (set-fontset-font t 'hangul (font-spec :family "Apple SD Gothic Neo"))
        (set-fontset-font t 'kana (font-spec :family "Hiragino Maru Gothic ProN"))
        (set-fontset-font t 'cjk-misc (font-spec :family "PingFang SC"))
        (set-fontset-font t 'bopomofo (font-spec :family "PingFang SC"))
        (set-fontset-font t 'han (font-spec :family "PingFang SC")))
    (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji"))
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Math"))
    (set-fontset-font t 'greek (font-spec :family "Noto Sans Symbols"))
    (set-fontset-font t 'hangul (font-spec :family "Noto Sans CJK KR"))
    (set-fontset-font t 'kana (font-spec :family "Noto Sans CJK JP"))
    (set-fontset-font t 'han (font-spec :family "Noto Sans CJK SC"))))

(defun zoro-apply-font-to-frame (frame)
  "Apply configured fonts to graphical FRAME."
  (with-selected-frame frame
    (when (display-graphic-p)
      (zoro-change-font))))

(defun zoro-setup-frame-alpha (&optional frame)
  "Apply transparency and blur to FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (set-frame-parameter nil 'ns-alpha-elements
                           '(ns-alpha-default ns-alpha-fringe ns-alpha-glyphs))
      (set-frame-parameter nil 'alpha-background 0.95)
      (set-frame-parameter nil 'ns-background-blur 25))))

(when (display-graphic-p)
  (zoro-change-font))
(add-hook 'after-make-frame-functions #'zoro-apply-font-to-frame)

(global-hl-line-mode 1)
(setq-default frame-title-format
              '("M-EMACS - " user-login-name "@" system-name " - %b"))
(setopt use-short-answers t)
(setq use-dialog-box nil
      inhibit-startup-screen t
      initial-major-mode 'text-mode
      initial-scratch-message "Present Day, Present Time...\n")

(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(column-number-mode 1)
(unless noninteractive
  (display-time-mode 1)
  (pixel-scroll-precision-mode 1))

(when (and (display-graphic-p) (not (daemonp)))
  (zoro-setup-frame-alpha))
(when (daemonp)
  (add-hook 'after-make-frame-functions #'zoro-setup-frame-alpha))

(setq mode-line-collapse-minor-modes '(not)
      scroll-margin 1
      scroll-conservatively 101
      mouse-wheel-progressive-speed nil)

(provide 'init-ui)
;;; init-ui.el ends here
