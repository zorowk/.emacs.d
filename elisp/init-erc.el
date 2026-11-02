;;; init-erc.el --- IRC client configuration -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure ERC sessions, SASL, notifications, and logging.

;;; Code:

(require 'seq)

(declare-function erc-services-mode "erc-services" (&optional arg))
(declare-function erc-send-current-line "erc" ())
(declare-function erc-server-delayed-check-reconnect "erc-backend" (buffer))

(use-package erc
  :ensure nil
  :defer t
  :commands (erc erc-tls)
  :init
  (setq erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs"))
        erc-user-full-name user-full-name
        ;; Keep nick changes, channel modes and topic metadata visible.
        ;; The full member list remains available through /NAMES.
        erc-hide-list '("353")
        ;; Only suppress presence chatter from people who haven't spoken
        ;; in the last twelve hours, rather than hiding everyone equally.
        erc-lurker-hide-list '("JOIN" "PART" "QUIT")
        ;; Visible status messages should not count as unread conversation.
        erc-track-exclude-types
        '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "331" "332" "333" "353" "366")
        erc-server-coding-system '(utf-8 . utf-8)
        erc-interpret-mirc-color t
        erc-kill-buffer-on-part t
        erc-kill-queries-on-quit t
        erc-kill-server-buffer-on-quit t
        erc-autojoin-timing 'ident
        erc-fill-function 'erc-fill-wrap
        erc-fill-static-center 18
        erc-fill-wrap-merge t
        erc-fill-wrap-merge-indicator '(?│ . shadow)
        erc-timestamp-format "[%H:%M]"
        erc-insert-timestamp-function 'erc-insert-timestamp-left-and-right
        erc-timestamp-format-left "\n[%Y-%m-%d %a]\n"
        erc-timestamp-format-right "[%H:%M]"
        erc-lurker-threshold-time 43200
        erc-prompt-for-password nil
        erc-prompt-for-nickserv-password nil
        erc-save-buffer-on-part t
        erc-nick-uniquifier "_"
        erc-log-channels-directory
        (expand-file-name ".erc-logs" user-emacs-directory)
        ;; Save as messages arrive and restore logs only in new buffers,
        ;; avoiding duplicate history after an automatic reconnect.
        erc-log-write-after-insert t
        erc-log-write-after-send t
        erc-log-insert-log-on-open #'erc-log-new-target-buffer-p
  ;; Protect me from accidentally sending excess lines.
        erc-inhibit-multiline-input t
        erc-send-whitespace-lines nil
        erc-ask-about-multiline-input t
  ;; Scroll all windows to prompt when submitting input.
        erc-scrolltobottom-all t
  ;; Reconnect automatically using a fancy strategy.
        erc-server-reconnect-function #'erc-server-delayed-check-reconnect
        erc-server-reconnect-timeout 30
  ;; Show new buffers in the current window instead of a split.
        erc-interactive-display 'buffer
        erc-sasl-user :nick
        erc-sasl-auth-source-function #'erc-auth-source-search
        erc-desktop-notifications-ignored-when-focused '(query mention)
        ;; Show ordinary conversation in the mode line too; desktop
        ;; notifications remain limited to private messages and mentions.
        erc-track-priority-faces-only nil
        erc-track-exclude-server-buffer t
        erc-track-showcount t)
  ;; Insert a newline when I hit <RET> at the prompt, and prefer
  ;; something more deliberate for actually sending messages.
  :bind (:map erc-mode-map
              ("RET" . nil)
              ("C-c C-c" . #'erc-send-current-line))
  :config
  (require 'erc-sasl)
  (custom-set-faces
   '(erc-notice-face ((t (:slant italic :weight unspecified)))))
  ;; Prefer SASL to NickServ and keep tracking focused on real mentions.
  ;; Emacs 31's notifications module handles private messages and nickname
  ;; mentions, including focus filtering and click-to-open actions.
  (setopt erc-modules
          (seq-union '(notifications sasl nicks scrolltobottom log)
                     erc-modules))
  (setopt erc-track-faces-priority-list
          (remq 'erc-notice-face erc-track-faces-priority-list))
  (make-directory (expand-file-name ".erc-logs" user-emacs-directory) t)
  (erc-track-mode t)
  (erc-services-mode 1)
  (erc-keep-place-indicator-mode 1))

(provide 'init-erc)
;;; init-erc.el ends here
