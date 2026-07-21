;;; init-core.el --- Core Emacs behavior -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure process-independent behavior shared by all workflows.

;;; Code:

(defun zoro-gc-when-unfocused ()
  "Collect garbage after the last frame loses focus."
  (unless (frame-focus-state)
    (garbage-collect)))

(defun zoro-install-focus-gc ()
  "Install garbage collection after frame focus changes.

`after-focus-change-function' is an abnormal function variable rather than a
normal hook, so attach the callback with `add-function'."
  (add-function :after after-focus-change-function #'zoro-gc-when-unfocused))

(defun zoro-abort-minibuffer-using-mouse ()
  "Abort an active minibuffer when the mouse leaves its buffer."
  (when (and (>= (recursion-depth) 1)
             (active-minibuffer-window))
    (abort-recursive-edit)))

(defun zoro-where-am-i ()
  "Show and copy `buffer-file-name' or `buffer-name'."
  (interactive)
  (message (kill-new (or buffer-file-name (buffer-name)))))

(add-hook 'emacs-startup-hook #'zoro-install-focus-gc)

;; Global bindings.
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "M-m") nil)
(global-set-key (kbd "C-x C-z") nil)
(global-set-key (kbd "M-/") nil)
(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "C-h C-f") #'find-function)
(global-set-key (kbd "C-h C-l") #'find-library)
(global-set-key (kbd "C-h C-k") #'find-function-on-key)
(global-set-key (kbd "C-h C-v") #'find-variable)
(global-set-key (kbd "<f5>") #'revert-buffer-quick)

;; Prefer UTF-8 throughout the editor and clipboard.
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Emacs 31 provides a buffer-local save-time whitespace cleaner.  Enable it
;; for editing modes, then opt out where trailing spaces can carry meaning.
(add-hook 'prog-mode-hook #'delete-trailing-whitespace-mode)
(add-hook 'text-mode-hook #'delete-trailing-whitespace-mode)
(add-hook 'conf-mode-hook #'delete-trailing-whitespace-mode)
(add-hook 'org-mode-hook (lambda () (delete-trailing-whitespace-mode -1)))
(add-hook 'markdown-mode-hook
          (lambda () (delete-trailing-whitespace-mode -1)))
(add-hook 'makefile-mode-hook #'indent-tabs-mode)
(add-hook 'prog-mode-hook #'editorconfig-mode)

(setopt indent-tabs-mode nil
        isearch-lazy-count t
        eldoc-help-at-pt t
        split-window-preferred-direction 'horizontal)
(delete-selection-mode 1)
(repeat-mode 1)
(require 'server)

(defun zoro-start-server-if-needed ()
  "Start the Emacs server unless another instance already provides it."
  (unless (or noninteractive (daemonp) (server-running-p))
    (server-mode 1)))

(zoro-start-server-if-needed)
(unless noninteractive
  (which-key-mode 1)
  (global-so-long-mode 1))

(setq x-alt-keysym 'meta
      confirm-kill-emacs 'y-or-n-p
      confirm-kill-processes t
      ring-bell-function 'ignore
      shr-use-fonts nil
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t
      mouse-yank-at-point t
      apropos-do-all t
      search-default-mode t
      echo-keystrokes 0.1
      ad-redefinition-action 'accept
      warning-minimum-level :warning
      epg-pinentry-mode 'loopback
      bidi-inhibit-bpa t
      redisplay-skip-fontification-on-input t
      read-process-output-max (* 4 1024 1024)
      highlight-nonselected-windows nil
      save-interprogram-paste-before-kill t
      kill-do-not-save-duplicates t
      reb-re-syntax 'string
      window-combination-resize t
      set-mark-command-repeat-pop t
      help-window-select t
      browse-url-browser-function #'eww-browse-url
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(setq-default history-length 500
              bidi-paragraph-direction 'left-to-right
              cursor-in-non-selected-windows nil)

(when (boundp 'xterm-update-cursor)
  (setopt xterm-update-cursor nil))
(when (boundp 'native-comp-async-on-battery-power)
  (setopt native-comp-async-on-battery-power nil))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(add-hook 'mouse-leave-buffer-hook #'zoro-abort-minibuffer-using-mouse)

(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)

(provide 'init-core)
;;; init-core.el ends here
