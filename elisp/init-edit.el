;;; init-edit.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-edit.el
;; Description: Initialize Editing Configuration
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 28 13:25:24 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d iedit
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes iedit, electric-pair, delete-block
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

;; expreg
(use-package expreg
  :ensure t
  :bind (("C-=" . expreg-expand)
         ("C--" . expreg-contract)))
;; -expreg

;; ElectricPair
(use-package elec-pair
  :ensure nil
  :hook (prog-mode . electric-pair-local-mode)
  :custom
  (electric-pair-preserve-balance t)
  (electric-pair-delete-adjacent-pairs t)
  (electric-pair-skip-self t))
;; -ElectricPair

;; MatchParens
(use-package paren
  :ensure nil
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen 'overlay)
  (show-paren-not-in-comments-or-strings 'on-mismatch)
  :config
  (show-paren-mode 1))
;; -MatchParens

;; patch linux wayland
(when (and (eq system-type 'gnu/linux) (display-graphic-p))
  (setq select-enable-clipboard t
        select-enable-primary t)  ; 开启鼠标中键选区(Primary selection)同步

  (when (and (string= (getenv "XDG_SESSION_TYPE") "wayland")
             (executable-find "wl-copy")
             (executable-find "wl-paste"))
    (setq interprogram-cut-function
          (lambda (text)
            (let ((process (make-process :name "wl-copy"
                                         :buffer nil
                                         :command '("wl-copy")
                                         :connection-type 'pipe)))
              (process-send-string process text)
              (process-send-eof process))))
    (setq interprogram-paste-function
          (lambda ()
            (with-output-to-string
              (with-current-buffer standard-output
                (call-process "wl-paste" nil t nil "-n")))))))
;; -patch linux wayland

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
