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
  :config
  (modus-themes-include-derivatives-mode 1)
  (setq modus-themes-mixed-fonts t
        modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-common-palette-overrides
        modus-themes-preset-overrides-faint
        modus-themes-headings
        '((0 . (variable-pitch regular 1.45))
          (1 . (variable-pitch semibold 1.25))
          (2 . (variable-pitch semibold 1.15))
          (3 . (variable-pitch medium 1.08))
          (agenda-structure . (variable-pitch semibold 1.2))
          (agenda-date . (variable-pitch regular 1.05))
          (t . (variable-pitch semibold 1.0))))
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
                          :height 1.08)
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

(zoro-apply-font)
(add-hook 'after-make-frame-functions #'zoro-apply-font)

;; Limit row highlighting to code, tabular, and list buffers rather than prose.
(dolist (hook '(prog-mode-hook
                dired-mode-hook
                elfeed-search-mode-hook
                gnus-group-mode-hook
                gnus-summary-mode-hook
                org-agenda-mode-hook
                tabulated-list-mode-hook
                Buffer-menu-mode-hook))
  (add-hook hook #'hl-line-mode))

(setq-default frame-title-format
              '("M-EMACS - " user-login-name "@" system-name " - %b"))
(setopt use-short-answers t)
(setq use-dialog-box nil
      inhibit-startup-screen t
      initial-major-mode 'text-mode
      initial-scratch-message "Present Day, Present Time...\n")

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(column-number-mode 1)
(unless noninteractive
  (display-time-mode 1)
  (pixel-scroll-precision-mode 1))

(setopt project-mode-line 'non-remote)

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
