;;; early-init.el --- -*- lexical-binding: t -*-
;;
;; Filename: early-init.el
;; Description: Early initialization
;; Author: zorowk
;; Copyright (C) 2019 zorowk
;; Created: Sun Jun  9 17:58:05 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d init early-init
;; Compatibility: Emacs 31
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file runs before package activation, frame setup, and init.el.
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

(when (version< emacs-version "31.1")
  (error "This configuration requires Emacs 31.1 or newer"))

;; Stabilize frame geometry before init.el loads the selected theme.
(setq frame-inhibit-implied-resize t)

;; DeferGC
;; A bounded startup threshold avoids collections without allowing unbounded
;; allocation if initialization fails.
(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.6)
;; -DeferGC

;; UnsetFNHA
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
;; -UnsetFNHA

(defun zoro-restore-startup-state ()
  "Restore GC and file handling after loading the init file."
  (setq gc-cons-threshold (* 16 1024 1024)
        gc-cons-percentage 0.1)
  (when (boundp 'file-name-handler-alist-original)
    (setq file-name-handler-alist file-name-handler-alist-original)
    (makunbound 'file-name-handler-alist-original)))

(add-hook 'after-init-hook #'zoro-restore-startup-state)

;; UnsetSRF
(setq site-run-file nil)
;; -UnsetSRF

;; DisableUnnecessaryInterface
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; -DisableUnnecessaryInterface

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
