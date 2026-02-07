;;; init-shrface.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-shrface.el
;; Description: Initialize shrface
;; Author: zorowk
;; Copyright (C) 2021 zorowk
;; Created: Tue Feb 2 13:28:34 2021 (-0400)
;; Version: 3.0
;; URL: https://github.com/zorowk/.emacs.d.git
;; Keywords: M-EMACS .emacs.d shrface
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes shrface
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

;; shrface
(use-package shrface
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))
;; -shrface

;; eww
(use-package eww
  :straight (:type built-in)
  :defer t
  :custom
  (eww-buffer-name-prefix "*eww-")      ; buffer 名变成 *eww-xxx*
  (eww-search-prefix "https://duckduckgo.com/?q=")
  (shr-use-fonts t)                     ; 允许使用字体大小变化
  (shr-use-colors t)                    ; 启用颜色（配合主题
  :bind
  (("C-c w w" . eww)
   ("C-c w o" . eww-open-in-new-buffer)
   ("C-c w s" . eww-search)
   ("C-c w u" . eww-up-url)
   ("C-c w r" . eww-reload))
  :hook
  (eww-after-render . (lambda ()
                        (when (featurep 'shrface)
                          (shrface-mode 1)))))
;; -eww

;; nov
(use-package nov
  :defer t
  :init
  (add-hook 'nov-mode-hook #'shrface-mode)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :config
  (require 'shrface)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions)))
;; -nov

(provide 'init-shrface)
