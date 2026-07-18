;;; init-theme.el --- Visual interface helpers -*- lexical-binding: t -*-
;;
;; Filename: init-theme.el
;; Description: Configure focused visual interface helpers
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 17:11:56 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d themes ui
;; Compatibility: Emacs 31
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Configure padding, visual feedback, popup windows, and reading width.
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

;; SpaciousPadding
(use-package spacious-padding
  :ensure t
  :config
  (setopt spacious-padding-widths
          '( :internal-border-width 16
             :header-line-width 1
             :mode-line-width 3
             :tab-width 4
             :right-divider-width 8
             :scroll-bar-width 0))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setopt spacious-padding-subtle-frame-lines
          `( :mode-line-active error
             :mode-line-inactive shadow))
  (spacious-padding-mode))

(setq-default line-spacing '(0.05 . 0.10))
;; -SpaciousPadding

;; Pulsar
(use-package pulsar
  :ensure t
  :commands pulsar-global-mode
  :config
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 5)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-region-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-magenta))
;; -Pulsar

;; popper
(use-package popper
  :ensure t
  :commands (popper-mode popper-echo-mode)
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
    '(("^\\*Warnings\\*$" . hide)
      ("^\\*Compile-Log\\*$" . hide)
      ("^\\*Async-native-compile-log\\*$" . hide)
      ("^\\*Messages\\*$" . hide)
      ;; "^\\*Matlab Help.*\\*$"
      "^\\*Backtrace\\*"
      "^\\*evil-registers\\*"
      "^\\*Apropos"
      "^\\*Occur\\*"
      "^\\*xref\\*"
      "^\\*Flymake diagnostics"
      "^\\*Embark Collect \\(Live\\|Completions\\)\\*"
      "^Calc:"
      "^\\*eldoc\\*"
      "^\\*TeX errors\\*"
      "^\\*ielm\\*"
      "^\\*TeX Help\\*"
      "^\\*gptel-ask\\*"
      "\\*Dictionary\\*"
      "\\*Shell Command Output\\*"
      "\\*Async Shell Command\\*"
      ("\\*Detached Shell Command\\*" . hide)
      "\\*Completions\\*"
      "^\\*Org QL View:notmuch-links\\*$"
      "[Oo]utput\\*"
      helpful-mode
      help-mode
      pydoc-mode
      inferior-python-mode
      inferior-ess-mode
      TeX-special-mode
      messages-buffer-mode
      dictionary-mode
      compilation-mode))
  (setq popper-group-function #'popper-group-by-directory)
  (setq popper-window-height 0.33))
;; -popper

;; olivetti
(use-package olivetti
  :ensure t
  :defer t)
(add-hook 'eww-after-render-hook
          (lambda ()
            (olivetti-mode 1)
            (setq-local olivetti-body-width 120)))
(add-hook 'nov-mode-hook
          (lambda ()
            (olivetti-mode 1)
            (setq-local olivetti-body-width 120)))
(add-hook 'Info-mode-hook
          (lambda ()
            (olivetti-mode 1)
            (setq-local olivetti-body-width 72)))
(add-hook 'elpher-mode-hook
          (lambda ()
            (olivetti-mode 1)
            (setq-local olivetti-body-width 72)))
;; -olivetti

(provide 'init-theme)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-theme.el ends here
