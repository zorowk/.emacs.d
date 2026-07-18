;;; init-ui-config.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-ui-config.el
;; Description: Initialize UI Configurations
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 16:12:56 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d ui
;; Compatibility: Emacs 31
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Configure global UI behavior.
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

(defun zoro-setup-frame-alpha (&optional frame)
  "Apply transparency and blur to FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (set-frame-parameter nil 'ns-alpha-elements
                           '(ns-alpha-default ns-alpha-fringe ns-alpha-glyphs))
      (set-frame-parameter nil 'alpha-background 0.95)
      (set-frame-parameter nil 'ns-background-blur 25))))

;; Highlight
(global-hl-line-mode 1)
;; -Highlight

;; TitleBar
(setq-default frame-title-format '("M-EMACS - " user-login-name "@" system-name " - %b"))
;; -TitleBar

;; YorN
(setopt use-short-answers t)
(setq use-dialog-box nil)
;; -YorN

;; StartupScreen
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
;; https://www.youtube.com/watch?v=NfjsLmya1PI
(setq initial-scratch-message "Present Day, Present Time...\n")
;; -StartupScreen

;; DisLineNum
;; Hook line numbers only when files are opened.
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
;; Display column numbers in modeline
(column-number-mode 1)
(unless noninteractive
  (display-time-mode 1))
;; -DisLineNum

;; Alpha
(when (and (display-graphic-p)
           (not (daemonp)))
  (zoro-setup-frame-alpha))

(when (daemonp)
  (add-hook 'after-make-frame-functions #'zoro-setup-frame-alpha))

(setq mode-line-collapse-minor-modes '(not))
;; -Alpha

(provide 'init-ui-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui-config.el ends here
