;;; init-mu4e.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-mu4e.el
;; Description: Initialize mu4e
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Mon Dec  2 15:17:14 2019 (-0500)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d mu mu4e
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes mu4e for Email clients in Emacs
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

;; Mu4ePac
(use-package mu4e
  :ensure nil
  :commands (mu4e make-mu4e-context)
  :init
  (use-package mu4e-alert
    :defer t
    :config
    (when (executable-find "notify-send")
      (mu4e-alert-set-default-style 'libnotify))
    :hook
    ((after-init . mu4e-alert-enable-notifications)
     (after-init . mu4e-alert-enable-mode-line-display)))
  (use-package mu4e-overview :defer t)
  (setenv "XAPIAN_CJK_NGRAM" "yes")
  :bind
  (("M-z m" . mu4e)
   ("M-m m" . mu4e)
   (:map mu4e-view-mode-map
         ("e" . mu4e-view-save-attachment)))
  :custom
  ;; 配置环境变量 XAPIAN_CJK_NGRAM 为 1，
  ;; 这样使用 mu find 可以搜索任意单个中文字符
  (mu4e-maildir (expand-file-name "~/Mail"))
  (mu4e-get-mail-command "proxychains offlineimap")
  (mu4e-view-prefer-html t)
  (mu4e-update-interval 300)
  (mu4e-headers-auto-update t)
  (mu4e-compose-format-flowed t)
  (mu4e-view-show-images t)
  (mu4e-image-max-width 800)
  (mu4e-change-filenames-when-moving t) ; work better for mbsync
  (mu4e-attachment-dir "~/Downloads")
  (message-kill-buffer-on-exit t)
  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-signature-auto-include t)
  (mu4e-view-show-addresses t)
  (mu4e-confirm-quit nil)
  (mu4e-use-fancy-chars nil)
  (mu4e-view-use-gnus t)
  (mu4e-headers-include-related t)
  (mail-user-agent 'mu4e-user-agent)
  (mu4e-user-mail-address-list (list  "pengwenhao@uniontech.com"
                                      "near.kingzero@gmail.com"))
  (mu4e-maildir-shortcuts '( ("/Gmail/INBOX" . ?g)
                             ("/Uniontech/INBOX" . ?u)))
  ;; 回复邮件插入邮件引用信息
  (message-citation-line-function 'message-insert-formatted-citation-line)
  (message-citation-line-format "On %a, %b %d %Y, %f wrote:\n")
  (gnus-icalendar-org-capture-file "~/Dropbox/brain/gtd.org") ; Prerequisite: set it to meetings org fie
  (gnus-icalendar-org-capture-headline '("Meetings")) ; Make sure to create Calendar heading first
  :hook
  ((mu4e-view-mode . visual-line-mode)
   (mu4e-compose-mode . (lambda ()
                          (visual-line-mode)
                          (use-hard-newlines -1)
                          (flyspell-mode)))
   (mu4e-view-mode . (lambda() ;; try to emulate some of the eww key-bindings
                       (local-set-key (kbd "<tab>") 'shr-next-link)
                       (local-set-key (kbd "<backtab>") 'shr-previous-link)))
   (mu4e-headers-mode . (lambda ()
                          (interactive)
                          (setq mu4e-headers-fields
                                `((:human-date . 25) ;; alternatively, use :date
                                  (:flags . 6)
                                  (:from . 22)
                                  (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
                                  (:size . 7))))))
  :config
  (require 'mu4e-icalendar)
  (mu4e-icalendar-setup)
  (gnus-icalendar-org-setup)
  (defalias 'mu4e-add-attachment 'mail-add-attachment
    "I prefer the add-attachment function to begin wih mu4e so I can find it easily.")
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  ;; 如果 imagemagick 可用，则使用它处理图像。
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; 配置使用 w3m 处理 html mail，使用下来有些慢，更换为 shr
  ;;(setq mu4e-html2text-command "w3m -dump -T text/html")

  ;; 使用 shr 渲染当前 buffer
  (require 'shr)
  (defun shr-render-current-buffer ()
    "Render the selected region."
    (shr-render-region (point-min) (point-max)))
  (setq mu4e-html2text-command 'shr-render-current-buffer)

  (setq mu4e-contexts
        `(
          ,(make-mu4e-context
          :name "Uniontech"
          :enter-func (lambda () (mu4e-message "Entering context Uniontech"))
          :leave-func (lambda () (mu4e-clear-caches))
          :match-func
          (lambda (msg)
            (when msg
              (string-match "Uniontech" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-sent-folder . "/Uniontech/Sent")
                  (mu4e-drafts-folder . "/Uniontech/Draft")
                  (mu4e-trash-folder . "/Uniontech/Trash")
                  (mu4e-refile-folder . "/Uniontech/Archive")
                  (mu4e-sent-messages-behavior . sent)
                  (mu4e-compose-signature . user-full-name)
                  (user-mail-address . "pengwenhao@uniontech.com") ; Prerequisite: Set this to your email
                  (mu4e-compose-format-flowed . t)
                  (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-smtp-user . "pengwenhao") ; Set to your username
                  (smtpmail-starttls-credentials . (("smtp.263.net" 465 nil nil)))
                  (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
                  (smtpmail-default-smtp-server . "smtp.263.net")
                  (smtpmail-smtp-server . "smtp.263.net")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-debug-info . t)
                  (smtpmail-debug-verbose . t)))
          ,(make-mu4e-context
          :name "Gmail"
          :enter-func (lambda () (mu4e-message "Entering context Gmail"))
          :leave-func (lambda () (mu4e-clear-caches))
          :match-func
          (lambda (msg)
            (when msg
              (string-match "Gmail" (mu4e-message-field msg :maildir))))
          :vars '((mu4e-sent-folder . "/Gmail/[Gmail].Sent Mail")
                  (mu4e-drafts-folder . "/Gmail/[Gmail].Drafts")
                  (mu4e-trash-folder . "/Gmail/[Gmail].Trash")
                  (mu4e-sent-messages-behavior . sent)
                  (mu4e-compose-signature . user-full-name)
                  (user-mail-address . "near.kingzero@gmail.com") ; Prerequisite: Set this to your email
                  (mu4e-compose-format-flowed . t)
                  (message-send-mail-function . smtpmail-send-it)
                  (smtpmail-smtp-user . "zorowk") ; Set to your username
                  (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
                  (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
                  (smtpmail-default-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-server . "smtp.gmail.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-debug-info . t)
                  (smtpmail-debug-verbose . t))))))
;; -Mu4ePac

(provide 'init-mu4e)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mu4e.el ends here
