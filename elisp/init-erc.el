;;; init-erc.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-erc.el
;; Description: Initialize ERC
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Tue Jul 30 22:15:50 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d erc irc
;; Compatibility: emacs-version >= 26.1
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

(eval-when-compile
  (require 'init-global-config)
  (require 'init-func))

;; ERCPac
(use-package erc
  :straight (:type built-in)
  :defer t
  :custom
  ;; 基本身份信息（请手动设置你的昵称）
  (erc-nick "your-nick-here")           ; ← 修改成你的昵称
  (erc-user-full-name user-full-name)
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#emacs")))

  ;; 显示与日志
  (erc-interpret-mirc-color t)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 15)
  (erc-fill-column 100)
  (erc-server-coding-system '(utf-8 . utf-8))
  (erc-log-channels-directory (expand-file-name ".erc-logs" user-emacs-directory))
  (erc-save-buffer-on-part t)

  ;; 行为优化
  (erc-track-exclude-types '("NICK" "PART" "MODE" "324" "329" "332" "333" "353" "477"))
  (erc-kill-buffer-on-part t)
  (erc-kill-queries-on-quit t)
  (erc-kill-server-buffer-on-quit t)
  (erc-lurker-threshold-time 43200)     ; 12 小时视为 lurker
  (erc-server-reconnect-attempts 5)
  (erc-server-reconnect-timeout 3)
  (erc-nick-uniquifier "_")

  ;; 通知与高亮
  (erc-modules '(notifications hl-nicks image services track))  ; 内置 hl-nicks 已可用

  ;; 外观
  :custom-face
  (erc-notice-face ((t (:foreground "#ababab"))))

  :bind
  (("C-c i"   . erc-start-or-switch)     ; 启动或切换（推荐一个快捷键）
   ("C-c C-b" . erc-switch-to-buffer)
   (:map erc-mode-map
         ("M-RET" . newline)))

  :hook
  (ercn-notify . erc-notify)

  :config
  ;; 确保日志目录存在
  (make-directory erc-log-channels-directory t)

  ;; 启用内置模块
  (erc-track-mode 1)
  (erc-services-mode 1)

  ;; 启动或切换函数（简化版）
  (defun erc-start-or-switch ()
    "Start ERC on Libera.Chat or switch to existing buffer."
    (interactive)
    (if (get-buffer "irc.libera.chat:6697")
        (erc-track-switch-buffer 1)
      (erc-tls :server "irc.libera.chat"
               :port 6697
               :nick erc-nick
               :full-name erc-user-full-name)))

  ;; 通知函数（使用 alert 包或内置 notify）
  (defun erc-notify (nickname message)
    "Show notification for ERC message."
    (let* ((channel (buffer-name))
           (nick (or (erc-hl-nicks-trim-irc-nick nickname) nickname))
           (title (if (string-match-p (concat "^" nickname) channel)
                      nick
                    (format "%s (%s)" nick channel)))
           (msg (string-trim (replace-regexp-in-string "[ \t\n]+" " " message))))
      (alert (concat nick ": " msg) :title title))))

(provide 'init-erc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-erc.el ends here
