;;; init-ui.el --- Core appearance and frame behavior -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure the base theme, fonts, frames, display, and scrolling.

;;; Code:

(declare-function set-fontset-font "fontset.c"
                  (fontset characters font-spec &optional frame add))
(declare-function zoro-apply-system-theme "early-init"
                  (&optional appearance))

(use-package ef-themes
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq modus-themes-mixed-fonts t
        modus-themes-italic-constructs t)
  (add-hook 'ns-system-appearance-change-functions
            #'zoro-apply-system-theme)
  (zoro-apply-system-theme))

(defun zoro-apply-font (&optional frame)
  "Apply configured fonts to graphical FRAME or the selected frame."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (let ((font-height (if (eq system-type 'darwin) 150 110)))
        (set-face-attribute 'default nil
                            :family "JetBrains Mono"
                            :height font-height))
      (set-face-attribute 'fixed-pitch nil
                          :family "JetBrains Mono" :height 1.0)
      (set-face-attribute 'variable-pitch nil
                          :family (if (eq system-type 'darwin)
                                      "Georgia"
                                    "Gelasio")
                          :height 1.0)
      (dolist (entry
               (if (eq system-type 'darwin)
                   '((emoji . "Apple Color Emoji")
                     (symbol . "STIX Two Math")
                     (greek . "Apple Symbols")
                     (hangul . "Apple SD Gothic Neo")
                     (kana . "Hiragino Maru Gothic ProN")
                     (cjk-misc . "PingFang SC")
                     (bopomofo . "PingFang SC")
                     (han . "PingFang SC"))
                 '((emoji . "Noto Color Emoji")
                   (symbol . "Noto Sans Math")
                   (greek . "Noto Sans Symbols")
                   (hangul . "Noto Sans CJK KR")
                   (kana . "Noto Sans CJK JP")
                   (han . "Noto Sans CJK SC"))))
        (set-fontset-font t (car entry)
                          (font-spec :family (cdr entry)))))))

(defun zoro-setup-frame-alpha (&optional frame)
  "Apply transparency and blur to FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (set-frame-parameter nil 'ns-alpha-elements
                           '(ns-alpha-default ns-alpha-fringe ns-alpha-glyphs))
      (set-frame-parameter nil 'alpha-background 0.95)
      (set-frame-parameter nil 'ns-background-blur 25))))

(zoro-apply-font)
(add-hook 'after-make-frame-functions #'zoro-apply-font)

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

(zoro-setup-frame-alpha)
(add-hook 'after-make-frame-functions #'zoro-setup-frame-alpha)

(setq mode-line-collapse-minor-modes '(not)
      scroll-margin 1
      scroll-conservatively 101
      mouse-wheel-progressive-speed nil)

(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-modified
   mode-line-remote
   mode-line-window-dedicated
   mode-line-frame-identification
   mode-line-buffer-identification
   "  "
   (project-mode-line project-mode-line-format)
   (vc-mode vc-mode)
   "  "
   mode-line-modes
   "  "
   mode-line-position
   mode-line-format-right-align
   mode-line-mule-info
   mode-line-client
   mode-line-misc-info
   mode-line-end-spaces))

(provide 'init-ui)
;;; init-ui.el ends here
