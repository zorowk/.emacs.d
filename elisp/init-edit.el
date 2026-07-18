;;; init-edit.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-edit.el
;; Description: Initialize Editing Configuration
;; Author: zorowk
;; Copyright (C) 2019 zorowk
;; Created: Thu Mar 28 13:25:24 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d editing
;; Compatibility: Emacs 31
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Configure editing commands, direct navigation, window selection, paired
;; delimiters, matching parens, and clipboard integration.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; expreg
(use-package expreg
  :ensure t
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))
;; -expreg

;; CruxPac
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
   ("C-c I" . crux-find-user-init-file)
   ("C-c e" . crux-eval-and-replace))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))
;; -CruxPac

;; AvyPac
(use-package avy
  :ensure t
  :defer t
  :bind
  (("C-z j" . avy-goto-char-timer)
   ("C-z l" . avy-goto-line))
  :custom
  (avy-timeout-seconds 0.3)
  (avy-style 'pre)
  :custom-face
  (avy-lead-face ((t (:background "#51afef" :foreground "#870000" :weight bold)))))
;; -AvyPac

;; VundoPac
(use-package vundo
  :ensure t
  :bind ("C-z u" . vundo))
;; -VundoPac

;; AceWindowPac
(use-package ace-window
  :ensure t
  :bind ("C-x C-o" . ace-window)
  :custom-face (aw-mode-line-face ((t (:inherit (bold mode-line-emphasis)))))
  :config
  (ace-window-display-mode 1)
  (setq aw-swap-invert t)
  (setq aw-dispatch-always t
        aw-scope 'global
        aw-background nil
        aw-display-mode-overlay nil
        ;; Keep window selection keys disjoint from `aw-dispatch-alist';
        ;; dispatch keys are ignored when they also name candidate windows.
        aw-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?p))
  (setq aw-dispatch-alist
        '((?k aw-delete-window "Delete Window")
          (?x aw-swap-window "Swap Windows")
          (?c aw-copy-window "Copy Window")
          (?j aw-switch-buffer-in-window "Select Buffer")
          (?o aw-flip-window "Flip Window")
          (?b aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?f aw-split-window-fair "Split Fair Window")
          (?v aw-split-window-vert "Split Vert Window")
          (?h aw-split-window-horz "Split Horz Window")
          (?d delete-other-windows "Delete Other Windows")
          (?? aw-show-dispatch-help))))
;; -AceWindowPac

;; ElectricPair
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-local-mode)
  :custom
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
  (electric-pair-skip-self t))
;; -ElectricPair

;; MatchParens
(use-package paren
  :ensure nil
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen 'overlay)
  (show-paren-not-in-comments-or-strings 'on-mismatch)
  :config
  (show-paren-mode 1))
;; -MatchParens

;; patch linux wayland
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
;; -patch linux wayland

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
