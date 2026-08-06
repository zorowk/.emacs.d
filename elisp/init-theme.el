;;; init-theme.el --- Visual interface helpers -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure padding, visual feedback, popup windows, and reading width.

;;; Code:

;; SpaciousPadding
(use-package spacious-padding
  :ensure t
  :config
  (setopt spacious-padding-widths
          '( :internal-border-width 16
             :header-line-width 1
             :mode-line-width 3
             :tab-width 4
             :right-divider-width 8
             :scroll-bar-width 0))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setopt spacious-padding-subtle-frame-lines
          `( :mode-line-active error
             :mode-line-inactive shadow))
  (spacious-padding-mode))

(setq-default line-spacing '(0.05 . 0.10))

;; Pulsar
(use-package pulsar
  :ensure t
  :config
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-region-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-magenta)
  (unless noninteractive
    (pulsar-global-mode 1)))

(use-package popper
  :ensure t
  :demand t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
    '(("^\\*Warnings\\*$" . hide)
      ("^\\*Compile-Log\\*$" . hide)
      ("^\\*Async-native-compile-log\\*$" . hide)
      ("^\\*Messages\\*$" . hide)
      ;; "^\\*Matlab Help.*\\*$"
      "^\\*Backtrace\\*"
      "^\\*evil-registers\\*"
      "^\\*Apropos"
      "^\\*Occur\\*"
      "^\\*xref\\*"
      "^\\*Flymake diagnostics"
      "^\\*Embark Collect \\(Live\\|Completions\\)\\*"
      "^Calc:"
      "^\\*eldoc\\*"
      "^\\*TeX errors\\*"
      "^\\*ielm\\*"
      "^\\*TeX Help\\*"
      "^\\*gptel-ask\\*"
      "\\*Dictionary\\*"
      "\\*Shell Command Output\\*"
      "\\*Async Shell Command\\*"
      ("\\*Detached Shell Command\\*" . hide)
      "\\*Completions\\*"
      "^\\*Org QL View:notmuch-links\\*$"
      "[Oo]utput\\*"
      helpful-mode
      help-mode
      pydoc-mode
      inferior-python-mode
      TeX-special-mode
      messages-buffer-mode
      dictionary-mode
      compilation-mode))
  (setq popper-group-function #'popper-group-by-directory)
  (setq popper-window-height 0.33)
  :config
  (unless noninteractive
    (popper-mode 1)
    (popper-echo-mode 1)))

(use-package olivetti
  :ensure t
  :defer t)
(add-hook 'eww-after-render-hook
          (lambda ()
            (olivetti-mode 1)
            (setq-local olivetti-body-width 120)))
(add-hook 'nov-mode-hook
          (lambda ()
            (olivetti-mode 1)
            (setq-local olivetti-body-width 120)))
(add-hook 'Info-mode-hook
          (lambda ()
            (olivetti-mode 1)
            (setq-local olivetti-body-width 72)))
(add-hook 'elpher-mode-hook
          (lambda ()
            (olivetti-mode 1)
            (setq-local olivetti-body-width 72)))

(provide 'init-theme)
;;; init-theme.el ends here
