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

;; FontsList
;; Input Mono, Monaco Style, Line Height 1.3 download from http://input.fontbureau.com/
(defvar font-list '(("JetBrains Mono" . 11) ("Anonymous Pro" . 11) ("Adwaita Mono" . 11))
  "List of fonts and sizes.  The first one available will be used.")
;; -FontsList

;; --- Mixed-Pitch Configuration ---
;; This is the core plugin that creates the "Safari/Web" look by using
;; proportional fonts for text and monospaced for code blocks.
(use-package mixed-pitch
  :ensure t
  :hook (text-mode . mixed-pitch-mode)
  :config
  ;; Keep font sizes consistent between faces
  (setq mixed-pitch-set-height t))

;; FontFun
(defun get-available-fonts ()
  "Get list of available fonts from font-list."
  (let (available-fonts)
    (dolist (font font-list (nreverse available-fonts))
      (when (member (car font) (font-family-list))
        (push font available-fonts)))))

(defun change-font ()
  "Interactively change a font from a list a available fonts."
  (interactive)
  (let* ((available-fonts (get-available-fonts))
         font-name font-size font-setting)
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      ;; 1. Setup Default Face (Monospaced/Coding)
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string (completing-read "What font to use? " available-fonts nil t) available-fonts)))
            (setq font-name (car chosen) font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts) font-size (cdar available-fonts)))
      (setq font-setting (format "%s-%d" font-name font-size))
      (set-frame-font font-setting nil t)
      (add-to-list 'default-frame-alist (cons 'font font-setting)))

    ;; 2. Setup Variable-Pitch Face (Prose/Safari Style)
      ;; We manually set this since we are not using fontaine.
      (set-face-attribute 'variable-pitch nil
                          :family "Anonymous Pro")

    ;; 3. Symbol and Emoji Configuration
    (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji"))
    (set-fontset-font t 'symbol (font-spec :family "STIX Two Math"))
    (set-fontset-font t 'greek (font-spec :family "Symbol"))

    ;; 4. CJK Character Configuration (Linux optimized)
    (set-fontset-font t 'hangul (font-spec :family "Noto Sans CJK KR"))
    (set-fontset-font t 'kana (font-spec :family "Noto Sans CJK JP"))
    (set-fontset-font t 'cjk-misc (font-spec :family "Noto Sans CJK SC"))
    (set-fontset-font t 'bopomofo (font-spec :family "Noto Sans CJK SC"))
    (set-fontset-font t 'han (font-spec :family "Noto Sans CJK SC"))))

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
