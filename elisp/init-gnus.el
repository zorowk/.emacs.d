;;; init-gnus.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize GNUS
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

;; gnus
(use-package gnus
  :straight (:type built-in)
  :defer t
  :init
  (use-package auth-source-xoauth2-plugin :defer t)
  :defer t
  ;; Gnus configuration
  ;; (info "(gnus) Don't Panic")
  :bind (("C-z g" . gnus))
  :hook
  (;; Enable topic mode in the group buffer, for classifying groups.
   (gnus-group-mode-hook . #'gnus-topic-mode)
   ;; Display a `fill-column' indicator in Message mode.
   (message-mode-hook . #'display-fill-column-indicator-mode)
   ;; Enable Flyspell for on-the-fly spell checking.
   (message-mode-hook . #'flyspell-mode))
  :custom
  ;; Tell Emacs we'd like to use Gnus and its Message integration
  ;; for reading and writing mail.
  (mail-user-agent 'gnus-user-agent)
  (read-mail-command #'gnus)
  ;; Consolidate various Gnus files inside a gnus directory in the
  ;; `user-emacs-directory'.
  (gnus-home-directory (expand-file-name "gnus/" user-emacs-directory))
  (gnus-directory (expand-file-name "gnus/news/" user-emacs-directory))
  ;; don't bother with .newsrc, use .newsrc.eld instead
  (gnus-save-newsrc-file nil)
  (gnus-read-newsrc-file nil)
  ;; Don't prompt for confirmation when exiting Gnus.
  (gnus-interactive-exit nil)
  (gnus-select-method '(nnnil ""))

  ;; Gnus summary theme: visible status marks, aligned dates, and
  ;; low-noise Unicode thread glyphs.
  ;; Status columns: %U read mark, %R reply/secondary mark,
  ;; %O download mark, %z score mark.
  (gnus-unread-mark ?U)
  (gnus-dormant-mark ?D)
  (gnus-sum-thread-tree-root "● ")
  (gnus-sum-thread-tree-false-root "○ ")
  (gnus-sum-thread-tree-single-indent "  ")
  (gnus-sum-thread-tree-vertical "│ ")
  (gnus-sum-thread-tree-indent "  ")
  (gnus-sum-thread-tree-leaf-with-other "├─ ")
  (gnus-sum-thread-tree-single-leaf "└─ ")
  (gnus-summary-line-format "%U%R%O%z │ %-12&user-date; │ %-24,24f │ %B%S\n")
  (gnus-user-date-format-alist
   '(((gnus-seconds-today) . "Today %H:%M")
     ((+ 86400 (gnus-seconds-today)) . "Yday  %H:%M")
     ((gnus-seconds-year) . "%m-%d %H:%M")
     (t . "%Y-%m-%d")))
  (gnus-summary-selected-face 'gnus-summary-selected)

  ;; Configure two IMAP mail accounts.
  (gnus-secondary-select-methods
   '((nntp "news.gmane.io"
         (nntp-open-connection-function nntp-open-network-stream)
         (nntp-port-number 119))
     (nntp "news.eternal-september.org"
           (nntp-open-connection-function nntp-open-tls-stream)
           (nntp-port-number 563))
     (nnrss "")
     (nnimap
      "Gmail"
      (nnimap-stream ssl)
      (nnimap-address "imap.gmail.com")
      (nnimap-server-port 993) ; imaps
      (nnimap-authenticator xoauth2)
      (nnimap-user "near.kingzero@gmail.com")
      (nnimap-mailbox-list ("INBOX" "[Gmail]/Sent Mail" "[Gmail]/All Mail" "[Gmail]/Trash" "[Gmail]/Spam"))
      (nnimap-expunge-on-delete t)
      ;; Archive messages into yearly Archive folders upon pressing
      ;; 'E' (for Expire) in the summary buffer.
      (nnmail-expiry-wait immediate)
      (nnmail-expiry-target nnmail-fancy-expiry-target)
      (nnmail-fancy-expiry-targets
       (("from" ".*" "nnimap+Gmail:Archive.%Y"))))))
  ;; `init-file-debug' corresponds to launching emacs with --debug-init
  (nnimap-record-commands init-file-debug)
  ;; The "Sent" folder
  (gnus-message-archive-group "nnimap+Gmail:INBOX")
  ;; Display the following message headers in Article buffers,
  ;; in the given order.
  (gnus-sorted-header-list
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
  ;; Fine-tune sorting of threads in the summary buffer.
  ;; See: (info "(gnus) Sorting the Summary Buffer")
  (gnus-thread-sort-functions
   '(gnus-thread-sort-by-number
     gnus-thread-sort-by-subject
     gnus-thread-sort-by-date))
  ;;;; Message and sending mail

  ;; Automatically mark Gcc (sent) messages as read.
  (gnus-gcc-mark-as-read t)
  ;; Configure posting styles for per-account Gcc groups, and SMTP
  ;; server for sending mail.  See: (info "(gnus) Posting Styles")
  ;; Also see sample .authinfo file provided below.
  (gnus-posting-styles
   '(("nnimap\\+Gmail:.*"
      (address "near.kingzero@gmail.com")
      ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587")
      (gcc "nnimap+Gmail:INBOX"))))

  ;; Ask for confirmation when sending a message.
  (message-confirm-send t)
  ;; Wrap messages at 70 characters when pressing M-q or when
  ;; auto-fill-mode is enabled.
  (message-fill-column 70)
  ;; Forward messages (C-c C-f) as a proper MIME part.
  (message-forward-as-mime t)
  ;; Send mail using Emacs's built-in smtpmail library.
  (message-send-mail-function #'smtpmail-send-it)
  :custom-face
  (gnus-summary-selected ((t (:inherit highlight :extend t))))
  (gnus-summary-normal-unread ((t (:inherit default :weight bold))))
  (gnus-summary-normal-read ((t (:inherit shadow))))
  (gnus-summary-normal-ancient ((t (:inherit shadow))))
  (gnus-summary-low-unread ((t (:inherit font-lock-comment-face :weight bold))))
  (gnus-summary-low-read ((t (:inherit shadow))))
  (gnus-summary-high-unread ((t (:inherit font-lock-keyword-face :weight bold))))
  (gnus-summary-high-read ((t (:inherit font-lock-keyword-face))))
  (gnus-summary-normal-ticked ((t (:inherit warning :weight bold))))
  (gnus-summary-high-ticked ((t (:inherit warning :weight bold))))
  (gnus-summary-low-ticked ((t (:inherit warning))))
  (gnus-summary-cancelled ((t (:inherit error :strike-through t))))
  (gnus-header-name ((t (:inherit font-lock-keyword-face :weight bold))))
  (gnus-header-content ((t (:inherit default))))
  (gnus-header-from ((t (:inherit font-lock-variable-name-face :weight bold))))
  (gnus-header-subject ((t (:inherit font-lock-function-name-face :weight bold))))
  (gnus-header-newsgroups ((t (:inherit font-lock-string-face))))
  (gnus-signature ((t (:inherit shadow))))
  :config
  (setq gnus-visible-headers
        (mapconcat #'identity
                   '("^From:" "^Subject:" "^Date:" "^Newsgroups:" "^To:" "^Cc:")
                   "\\|"))
  (add-hook 'gnus-article-prepare-hook #'gnus-article-buttonize)
  (setq gnus-article-margin 2)
  (auth-source-xoauth2-plugin-mode t))
;; -gnus

(provide 'init-gnus)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-gnus.el ends here
