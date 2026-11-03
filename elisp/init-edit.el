;;; init-edit.el --- Editing and direct navigation -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure editing commands, direct navigation, window selection, paired
;; delimiters, matching parens, and clipboard integration.

;;; Code:

(use-package expreg
  :ensure t
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))

(use-package crux
  :ensure t
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("C-x 4 t" . crux-transpose-windows)
   ("C-k" . crux-smart-kill-line)
   ("C-c o" . crux-open-with)
   ("C-c d" . crux-delete-file-and-buffer)
   ("C-x C-r" . crux-sudo-edit)
   ("C-c b" . crux-switch-to-previous-buffer)
   ("C-c r" . crux-rename-file-and-buffer)
   ("C-c E" . erase-buffer)
   ("C-^" . crux-top-join-line)
   ("C-c RET" . crux-smart-open-line)
   ("C-c S-RET" . crux-smart-open-line-above)
   ("C-c x" . crux-eval-and-replace)
   ("C-c S" . crux-find-shell-init-file)
   ("C-c I" . crux-find-user-init-file))
  :config
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(use-package avy
  :ensure t
  :defer t
  :bind
  (("C-c j" . avy-goto-char-timer))
  :custom
  (avy-timeout-seconds 0.3)
  (avy-style 'pre)
  :custom-face
  (avy-lead-face ((t (:background "#51afef" :foreground "#870000" :weight bold)))))

(use-package vundo
  :ensure t
  :bind ("C-z u" . vundo))

(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-local-mode)
  :custom
  (electric-pair-skip-self t))

(use-package paren
  :ensure nil
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen 'overlay)
  (show-paren-not-in-comments-or-strings 'on-mismatch)
  :config
  (show-paren-mode 1))

(when (and (eq system-type 'gnu/linux) (display-graphic-p))
  ;; Let the graphical backend handle clipboard and primary selections.
  (setq select-enable-clipboard t
        select-enable-primary t))

(provide 'init-edit)
;;; init-edit.el ends here
