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
             :header-line-width 4
             :mode-line-width 1
             :tab-width 4
             :right-divider-width 8
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
  :config
  (pulsar-global-mode))
;; -Pulsar

(provide 'init-theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-theme.el ends here
