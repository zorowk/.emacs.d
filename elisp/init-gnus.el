;;; init-gnus.el --- -*- lexical-binding: t -*-
;;
;; Description: Initialize GNUS
;; Author: WenHao Peng
;; Copyright (C) 2026
;; Created: Tue Sep  3 21:28:26 2019 (-0400)
;; Version: 3.0
;; Keywords: M-EMACS .emacs.d gnus
;; Compatibility: Emacs 31
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
  :ensure nil
  :defer t
  :commands gnus
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
  :init
  (use-package auth-source-xoauth2-plugin
    :ensure t
    :defer t
    :commands auth-source-xoauth2-plugin-mode)
  (with-eval-after-load 'auth-source
    (add-to-list 'auth-sources "~/.authinfo.json.gpg"))
  ;; Tell Emacs we'd like to use Gnus and its Message integration
  ;; for reading and writing mail.
  (setq mail-user-agent 'gnus-user-agent)
  (setq read-mail-command #'gnus)
  ;; Consolidate various Gnus files inside a gnus directory in the
  ;; `user-emacs-directory'.
  (setq gnus-home-directory (expand-file-name "gnus/" user-emacs-directory))
  (setq gnus-directory (expand-file-name "gnus/news/" user-emacs-directory))
  ;; don't bother with .newsrc, use .newsrc.eld instead
  (setq gnus-save-newsrc-file nil)
  (setq gnus-read-newsrc-file nil)
  ;; Don't prompt for confirmation when exiting Gnus.
  (setq gnus-interactive-exit nil)
  (setq gnus-select-method '(nnnil ""))

  ;; Gnus summary theme: visible status marks, aligned dates, and
  ;; low-noise Unicode thread glyphs.
  ;; Status columns: %U read mark, %R reply/secondary mark,
  ;; %O download mark, %z score mark.
  (setq gnus-unread-mark ?U)
  (setq gnus-dormant-mark ?D)
  (setq gnus-sum-thread-tree-root "● ")
  (setq gnus-sum-thread-tree-false-root "○ ")
  (setq gnus-sum-thread-tree-single-indent "  ")
  (setq gnus-sum-thread-tree-vertical "│ ")
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-leaf-with-other "├─ ")
  (setq gnus-sum-thread-tree-single-leaf "└─ ")
  (setq gnus-summary-line-format "%U%R%O%z │ %-12&user-date; │ %-24,24f │ %B%S\n")
  (setq gnus-user-date-format-alist
   '(((gnus-seconds-today) . "Today %H:%M")
     ((+ 86400 (gnus-seconds-today)) . "Yday  %H:%M")
     ((gnus-seconds-year) . "%m-%d %H:%M")
     (t . "%Y-%m-%d")))
  (setq gnus-summary-selected-face 'gnus-summary-selected)

  ;; Configure two IMAP mail accounts.
  (setq gnus-secondary-select-methods
   '((nntp "news.gmane.io"
         (nntp-open-connection-function nntp-open-network-stream)
         (nntp-stream ssl)
         (nntp-port-number 119))
     (nntp "news.eternal-september.org"
           (nntp-open-connection-function nntp-open-tls-stream)
           (nntp-stream ssl)
           (nntp-port-number 563)
           ;; required because news.eternal-september.org also accepts
           ;; connections without authentication but will then just show
           ;; 'internal' groups
           (nntp-authinfo-force t)
           ;; for debugging puposes
           (nntp-record-commands nil))
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
  (setq nnimap-record-commands init-file-debug)
  ;; The "Sent" folder
  (setq gnus-message-archive-group "nnimap+Gmail:INBOX")
  ;; Display the following message headers in Article buffers,
  ;; in the given order.
  (setq gnus-sorted-header-list
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
  ;; Fine-tune sorting of summaries: newest threads/articles first.
  ;; See: (info "(gnus) Sorting the Summary Buffer")
  (setq gnus-thread-sort-functions
   '(gnus-thread-sort-by-number
     gnus-thread-sort-by-subject
     gnus-thread-sort-by-most-recent-date))
  (setq gnus-article-sort-functions
   '(gnus-article-sort-by-number
     (not gnus-article-sort-by-date)))
  ;;;; Message and sending mail

  ;; Automatically mark Gcc (sent) messages as read.
  (setq gnus-gcc-mark-as-read t)
  ;; Configure posting styles for per-account Gcc groups, and SMTP
  ;; server for sending mail.  See: (info "(gnus) Posting Styles")
  ;; Also see sample .authinfo file provided below.
  (setq gnus-posting-styles
   '(("nnimap\\+Gmail:.*"
      (address "near.kingzero@gmail.com")
      ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587")
      (gcc "nnimap+Gmail:INBOX"))))

  ;; Ask for confirmation when sending a message.
  (setq message-confirm-send t)
  ;; Wrap messages at 70 characters when pressing M-q or when
  ;; auto-fill-mode is enabled.
  (setq message-fill-column 70)
  ;; Forward messages (C-c C-f) as a proper MIME part.
  (setq message-forward-as-mime t)
  ;; Send mail using Emacs's built-in smtpmail library.
  (setq message-send-mail-function #'smtpmail-send-it)
  :config
  (custom-set-faces
   '(gnus-summary-selected ((t (:inherit highlight :extend t))))
   '(gnus-summary-normal-unread ((t (:inherit default :weight bold))))
   '(gnus-summary-normal-read ((t (:inherit shadow))))
   '(gnus-summary-normal-ancient ((t (:inherit shadow))))
   '(gnus-summary-low-unread
     ((t (:inherit font-lock-comment-face :weight bold))))
   '(gnus-summary-low-read ((t (:inherit shadow))))
   '(gnus-summary-high-unread
     ((t (:inherit font-lock-keyword-face :weight bold))))
   '(gnus-summary-high-read ((t (:inherit font-lock-keyword-face))))
   '(gnus-summary-normal-ticked ((t (:inherit warning :weight bold))))
   '(gnus-summary-high-ticked ((t (:inherit warning :weight bold))))
   '(gnus-summary-low-ticked ((t (:inherit warning))))
   '(gnus-summary-cancelled ((t (:inherit error :strike-through t))))
   '(gnus-header-name
     ((t (:inherit font-lock-keyword-face :weight bold))))
   '(gnus-header-content ((t (:inherit default))))
   '(gnus-header-from
     ((t (:inherit font-lock-variable-name-face :weight bold))))
   '(gnus-header-subject
     ((t (:inherit font-lock-function-name-face :weight bold))))
   '(gnus-header-newsgroups ((t (:inherit font-lock-string-face))))
   '(gnus-signature ((t (:inherit shadow)))))
  (setq gnus-visible-headers
        (mapconcat #'identity
                   '("^From:" "^Subject:" "^Date:" "^Newsgroups:" "^To:" "^Cc:")
                   "\\|"))
  (add-hook 'gnus-article-prepare-hook #'gnus-article-add-buttons)
  (setq gnus-article-margin 2)
  (auth-source-xoauth2-plugin-mode t))
;; -gnus

(provide 'init-gnus)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-gnus.el ends here
