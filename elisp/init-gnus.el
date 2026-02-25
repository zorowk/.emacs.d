;; Description: Initialize ESS
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Tue Sep  3 21:28:26 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d ess
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initialies ESS
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
;; Description: Initialize ESS
;; Author: WenHao Peng
;; Copyright (C) 2026
;; Created: Tue Sep  3 21:28:26 2019 (-0400)
;; Version: 3.0
;; Keywords: M-EMACS .emacs.d gnus
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initialies gnus
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

;; auth2
(use-package auth-source-xoauth2-plugin
  :custom
  (auth-source-xoauth2-plugin-mode t))
;; -auth2

;;; Gnus configuration
;; (info "(gnus) Don't Panic")
(keymap-global-set "C-z g" #'gnus)

;; Tell Emacs we'd like to use Gnus and its Message integration
;; for reading and writing mail.
(setopt
 mail-user-agent 'gnus-user-agent
 read-mail-command #'gnus)

;; Consolidate various Gnus files inside a gnus directory in the
;; `user-emacs-directory'.
(setopt
 gnus-home-directory (+user-emacs-directory "gnus/")
 gnus-directory      (+user-emacs-directory "gnus/news/")
 message-directory   (+user-emacs-directory "gnus/mail/")
 nndraft-directory   (+user-emacs-directory "gnus/drafts/"))

(setopt ; don't bother with .newsrc, use .newsrc.eld instead
 gnus-save-newsrc-file nil
 gnus-read-newsrc-file nil)

;; Don't prompt for confirmation when exiting Gnus.
(setopt gnus-interactive-exit nil)

;; Configure two IMAP mail accounts.
(setopt
 gnus-select-method '(nnnil "")
 gnus-secondary-select-methods
 '((nnimap
    "ec25gnus"
    (nnimap-stream tls)
    (nnimap-address  "mail.kelar.org")
    ;; (nnimap-server-port 993) ; imaps
    (nnimap-authenticator plain)
    (nnimap-user "ec25gnus@kelar.org"))
   (nnimap
    "ec25work"
    (nnimap-stream tls)
    (nnimap-address "mail.kelar.org")
    ;; (nnimap-server-port 993) ; imaps
    (nnimap-authenticator plain)
    (nnimap-user "ec25work@kelar.org")
    ;; Archive messages into yearly Archive folders upon pressing
    ;; 'E' (for Expire) in the summary buffer.
    (nnmail-expiry-wait immediate)
    (nnmail-expiry-target nnmail-fancy-expiry-target)
    (nnmail-fancy-expiry-targets
     (("from" ".*" "nnimap+ec25work:Archive.%Y"))))))

;; `init-file-debug' corresponds to launching emacs with --debug-init
(setq nnimap-record-commands init-file-debug)

;; The "Sent" folder
(setopt gnus-message-archive-group "nnimap+ec25gnus:INBOX")

;;;; Group buffer

;; Always show INBOX groups even if they have no unread or ticked
;; messages.
(setopt gnus-permanently-visible-groups ":INBOX$")
;; Enable topic mode in the group buffer, for classifying groups.
(add-hook 'gnus-group-mode-hook #'gnus-topic-mode)

;;;; Article buffer

;; Display the following message headers in Article buffers,
;; in the given order.
(setopt
 gnus-sorted-header-list
 '("^From:"
   "^X-RT-Originator"
   "^Newsgroups:"
   "^Subject:"
   "^Date:"
   "^Envelope-To:"
   "^Followup-To:"
   "^Reply-To:"
   "^Organization:"
   "^Summary:"
   "^Abstract:"
   "^Keywords:"
   "^To:"
   "^[BGF]?Cc:"
   "^Posted-To:"
   "^Mail-Copies-To:"
   "^Mail-Followup-To:"
   "^Apparently-To:"
   "^Resent-From:"
   "^User-Agent:"
   "^X-detected-operating-system:"
   "^X-Spam_action:"
   "^X-Spam_bar:"
   "^Message-ID:"
   ;; "^References:"
   "^List-Id:"
   "^Gnus-Warning:"))

;;;; Summary buffer

;; Fine-tune sorting of threads in the summary buffer.
;; See: (info "(gnus) Sorting the Summary Buffer")
(setopt
 gnus-thread-sort-functions
 '(gnus-thread-sort-by-number
   gnus-thread-sort-by-subject
   gnus-thread-sort-by-date))

;;;; Message and sending mail

(setopt
 ;; Automatically mark Gcc (sent) messages as read.
 gnus-gcc-mark-as-read t
 ;; Configure posting styles for per-account Gcc groups, and SMTP
 ;; server for sending mail.  See: (info "(gnus) Posting Styles")
 ;; Also see sample .authinfo file provided below.
 gnus-posting-styles
 '(("nnimap\\+ec25gnus:.*"
    (address "ec25gnus@kelar.org")
    ("X-Message-SMTP-Method" "smtp mail.kelar.org 587")
    (gcc "nnimap+ec25gnus:INBOX"))
   ("nnimap\\+ec25work:.*"
    (address "ec25work@kelar.org")
    ("X-Message-SMTP-Method" "smtp dasht.kelar.org 587")
    (gcc "nnimap+ec25work:INBOX"))))

(setopt
 ;; Ask for confirmation when sending a message.
 message-confirm-send t
 ;; Wrap messages at 70 characters when pressing M-q or when
 ;; auto-fill-mode is enabled.
 message-fill-column 70
 ;; Forward messages (C-c C-f) as a proper MIME part.
 message-forward-as-mime t
 ;; Send mail using Emacs's built-in smtpmail library.
 message-send-mail-function #'smtpmail-send-it
 ;; Omit our own email address(es) when composing replies.
 message-dont-reply-to-names "ec25\\(gnus\\|work\\)@kelar\\.org"
 gnus-ignored-from-addresses message-dont-reply-to-names)

;; Unbind C-c C-s for sending mail; too easy to accidentally hit
;; instead of C-c C-d (save draft for later)
(keymap-set message-mode-map "C-c C-s" nil)
;; Display a `fill-column' indicator in Message mode.
(add-hook 'message-mode-hook #'display-fill-column-indicator-mode)
;; Enable Flyspell for on-the-fly spell checking.
(add-hook 'message-mode-hook #'flyspell-mode)

(provide 'init-gnus)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-gnus.el ends here
