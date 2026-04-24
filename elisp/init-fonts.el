;;; init-fonts.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-fonts.el
;; Description: Initialize Fonts and Icons
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 17:32:54 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d fonts
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes fonts and icons
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

;; FontFun
(defun change-font ()
  ;; 1. Setup Default Face (Monospaced/Coding)
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :height 150)

  ;; 2. Setup Variable-Pitch Face (Prose/Safari Style)
  ;; We manually set this since we are not using fontaine.
  (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 1.0)
  (set-face-attribute 'variable-pitch nil :family "Georgia" :height 1.0)

  ;; 3. Symbol and Emoji Configuration
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
  (set-fontset-font t 'symbol (font-spec :family "STIX Two Math"))
  (set-fontset-font t 'greek (font-spec :family "Apple Symbols"))

  ;; 4. CJK Character Configuration (Linux optimized)
  (set-fontset-font t 'hangul (font-spec :family "Apple SD Gothic Neo"))
  (set-fontset-font t 'kana (font-spec :family "Hiragino Maru Gothic ProN"))
  (set-fontset-font t 'cjk-misc (font-spec :family "PingFang SC"))
  (set-fontset-font t 'bopomofo (font-spec :family "PingFang SC"))
  (set-fontset-font t 'han (font-spec :family "PingFang SC")))

(when (display-graphic-p)
  (change-font))

;; Re-apply when creating new frames (for emacsclient)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (when (display-graphic-p) (change-font)))))
;; -FontFun

;; ATIPac
(use-package all-the-icons :if (display-graphic-p))
;; -ATIPac

(provide 'init-fonts)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-fonts.el ends here
