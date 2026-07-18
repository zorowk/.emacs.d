;;; init-erc.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-erc.el
;; Description: Initialize ERC
;; Author: zorowk
;; Copyright (C) 2019 zorowk
;; Created: Tue Jul 30 22:15:50 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d erc irc
;; Compatibility: Emacs 31
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes erc
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

(require 'seq)

;; ERCPac
(use-package erc
  :ensure nil
  :defer t
  :commands (erc erc-tls)
  :init
  (setq erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs"))
        erc-user-full-name user-full-name
        erc-hide-list '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "331" "333" "353")
        erc-track-exclude-types erc-hide-list
        erc-server-coding-system '(utf-8 . utf-8)
        erc-interpret-mirc-color t
        erc-kill-buffer-on-part t
        erc-kill-queries-on-quit t
        erc-kill-server-buffer-on-quit t
        erc-autojoin-timing 'ident
        erc-fill-function 'erc-fill-wrap
        erc-fill-static-center 18
        erc-lurker-threshold-time 43200
        erc-prompt-for-password nil
        erc-prompt-for-nickserv-password nil
        erc-fill-column 100
        erc-save-buffer-on-part t
        erc-nick-uniquifier "_"
        erc-log-channels-directory
        (expand-file-name ".erc-logs" user-emacs-directory)
  ;; Protect me from accidentally sending excess lines.
        erc-inhibit-multiline-input t
        erc-send-whitespace-lines t
        erc-ask-about-multiline-input t
  ;; Scroll all windows to prompt when submitting input.
        erc-scrolltobottom-all t
  ;; Reconnect automatically using a fancy strategy.
        erc-server-reconnect-function #'erc-server-delayed-check-reconnect
        erc-server-reconnect-timeout 30
  ;; Show new buffers in the current window instead of a split.
        erc-interactive-display 'buffer
        erc-sasl-user :nick
        erc-track-priority-faces-only 'all)
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
          (seq-union '(notifications sasl nicks scrolltobottom)
                     erc-modules))
  (setopt erc-track-faces-priority-list
          (remq 'erc-notice-face erc-track-faces-priority-list))
  (make-directory (expand-file-name ".erc-logs" user-emacs-directory) t)
  (erc-track-mode t)
  (erc-services-mode 1)
  (erc-keep-place-indicator-mode 1))
;; -ERCPac

(provide 'init-erc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-erc.el ends here
