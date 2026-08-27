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
  (("C-c j" . avy-goto-char-timer)
   ("C-c l" . avy-goto-line))
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
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
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
  (setq select-enable-clipboard t
        select-enable-primary t)  ; 开启鼠标中键选区(Primary selection)同步

  (when (and (string= (getenv "XDG_SESSION_TYPE") "wayland")
             (executable-find "wl-copy")
             (executable-find "wl-paste"))
    (setq interprogram-cut-function
          (lambda (text)
            (let ((process (make-process :name "wl-copy"
                                         :buffer nil
                                         :command '("wl-copy")
                                         :connection-type 'pipe)))
              (process-send-string process text)
              (process-send-eof process))))
    (setq interprogram-paste-function
          (lambda ()
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process "wl-paste" nil t nil "-n")))))))

(provide 'init-edit)
;;; init-edit.el ends here
