;;; init-theme.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-theme.el
;; Description: Initialize Doom Themes and Modeline
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 17:11:56 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d doom-themes doom-modeline
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes doom-themes and doom-modeline
;; This is NOT Doom, but doom-themes and doom-modeine
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
  (require 'init-const))

(load-theme 'modus-vivendi-tinted :no-confirm)

;; SpaciousPadding
(use-package spacious-padding
  :config
  (setopt spacious-padding-widths
          '( :internal-border-width 16
             :header-line-width 1
             :mode-line-width 1
             :tab-width 4
             :right-divider-width 4
             :scroll-bar-width 0))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setopt spacious-padding-subtle-frame-lines
          `( :mode-line-active error
             :mode-line-inactive shadow))
  (spacious-padding-mode))
;; -SpaciousPadding

;; Pulsar
(use-package pulsar
  :ensure t
  :init
  (pulsar-global-mode 1)
  :config
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-region-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-magenta))
;; -Pulsar

;; popper
(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
     ("M-`"   . popper-cycle)
     ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
    '("\\*Messages\\*"
      "Output\\*$"
      "\\*Async Shell Command\\*"
      help-mode
      eshell-mode
      shell-mode
      dictionary-mode
      compilation-mode))
  (setq popper-group-function #'popper-group-by-directory)
  (setq popper-window-height 0.33)
  (popper-mode +1)
  (popper-echo-mode +1))
;; -popper

(provide 'init-theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-theme.el ends here
