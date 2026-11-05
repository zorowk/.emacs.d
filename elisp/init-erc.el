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
(declare-function erc-auth-source-search "erc" (&rest args))
(declare-function erc-log-new-target-buffer-p "erc-log" ())

(use-package erc
  :ensure nil
  :commands (erc erc-tls)
  :init
  ;; Match network identities even when the server announces a pool hostname.
  ;; Autojoin applies after connecting to each network; it does not connect it.
  (setq erc-server "irc.oftc.net"
        erc-nick "zorowk"
        erc-autojoin-channels-alist '((Libera.Chat "#emacs")
                                     (OFTC "#wayland" "#dri-devel"))
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
        ;; SASL completes before registration; no NickServ delay is needed.
        erc-autojoin-timing 'connect
        erc-fill-function 'erc-fill-wrap
        erc-fill-static-center 18
        erc-fill-wrap-merge t
        erc-fill-wrap-merge-indicator '(?│ . shadow)
        erc-timestamp-format-left "\n[%Y-%m-%d %a]\n"
        erc-lurker-threshold-time 43200
        erc-prompt-for-password nil
        erc-prompt-for-nickserv-password nil
        erc-use-auth-source-for-nickserv-password t
        erc-nick-uniquifier "_"
        erc-log-channels-directory
        (expand-file-name ".erc-logs" user-emacs-directory)
        ;; Save as messages arrive and restore logs only in new buffers,
        ;; avoiding duplicate history after an automatic reconnect.
        erc-log-write-after-insert t
        erc-log-write-after-send t
        erc-log-insert-log-on-open #'erc-log-new-target-buffer-p
        ;; Confirm before sending multiple lines.
        erc-inhibit-multiline-input t
        erc-ask-about-multiline-input t
        ;; Scroll all windows to the prompt after sending.
        erc-scrolltobottom-all t
        ;; Reconnect automatically with a delay.
        erc-server-reconnect-function #'erc-server-delayed-check-reconnect
        erc-server-reconnect-timeout 30
        ;; Show new buffers in the current window.
        erc-interactive-display 'buffer
        erc-sasl-user :nick
        erc-sasl-auth-source-function #'erc-auth-source-search
        erc-desktop-notifications-ignored-when-focused '(query mention)
        ;; Show ordinary conversation in the mode line too; desktop
        ;; notifications remain limited to private messages and mentions.
        erc-track-exclude-server-buffer t
        erc-track-showcount t)
  ;; Insert a newline when I hit <RET> at the prompt, and prefer
  ;; something more deliberate for actually sending messages.
  :bind (:map erc-mode-map
         ("RET" . nil)
         ("C-c C-c" . erc-send-current-line))
  :config
  (require 'erc-sasl)
  (require 'erc-settings)
  ;; Select a TLS URL so there is no separate port prompt.
  (setq erc--prompt-for-server-function
        (lambda ()
          (completing-read "IRC server: "
                           '("ircs://irc.oftc.net:6697"
                             "ircs://irc.libera.chat:6697")
                           nil t nil nil "ircs://irc.oftc.net:6697")))
  ;; OFTC uses NickServ; other networks retain SASL.
  (setopt erc-settings
          '(("oftc\\.net"
             (erc-modules (remq 'sasl erc-modules) :eval)
             (erc-auth-source-server-function nil)))
          erc-modules
          (seq-union '(autojoin notifications sasl settings nicks scrolltobottom log)
                     erc-modules)
          erc-track-faces-priority-list
          (remq 'erc-notice-face erc-track-faces-priority-list))
  (custom-set-faces
   '(erc-notice-face ((t (:slant italic :weight unspecified)))))
  (make-directory erc-log-channels-directory t)
  (erc-track-mode t)
  (erc-services-mode 1)
  (erc-keep-place-indicator-mode 1))

(provide 'init-erc)
;;; init-erc.el ends here
