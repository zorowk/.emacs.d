;;; init-translate.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-translate.el
;; Description: Initialize Translate
;; Author: zorowk
;; Copyright (C) 2019 zorowk
;; Created: Thu Nov 27 15:47:34 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d Translate
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes Translate
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

;; go-translate
(use-package go-translate
    :commands (go-translate
               go-translate-popup
               go-translate-popup-current
               go-translate-kill-ring-save
               go-translate-tts-play-current)
    :init
    (defvar go-translate-base-url "https://translate.google.cn")
    (defvar go-translate-local-language "zh-CN")
    (defvar go-translate-target-language "en")
    (defvar go-translate-extra-directions '(("zh-CN" . "ja") ("zh-CN" . "ko")))
    (defvar go-translate-buffer-follow-p t))
;; -go-translate

(provide 'init-translate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-calendar.el ends here
