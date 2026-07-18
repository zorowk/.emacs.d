;;; init.el --- -*- lexical-binding: t -*-
;;
;; Filename: init.el
;; Description: Initialize M-EMACS
;; Author: zorowk
;; Copyright (C) 2019 zorowk
;; Created: Thu Mar 14 10:15:28 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d init
;; Compatibility: Emacs 31
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the init.el file for M-EMACS
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

;; LoadPath
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
;; -LoadPath

;; Shared constants and named functions.
(require 'init-const)

;; Packages

;; Package Management
(require 'init-package)

;; Apply the selected theme before loading nonessential startup modules.
(require 'init-ui)

;; Core behavior, persistent files, and side-effect-free helpers.
(require 'init-core)
(require 'init-files)
(require 'init-development)

(require 'init-search)

(require 'init-edit)

(require 'init-shell)

(require 'init-dired)

(require 'init-buffer)

(require 'init-theme)

(require 'init-dashboard)

(require 'init-complete)

(require 'init-templates)

;; Programming
(require 'init-ess)

(require 'init-latex)

;; Office
(require 'init-org)

(require 'init-hyperbole)

(require 'init-reader)

;; Internet
(require 'init-erc)

(require 'init-llm)

(require 'init-gnus)

;; Defer only the measured startup outliers.
(unless noninteractive
  (run-with-idle-timer 0.10 nil #'require 'dashboard)
  (when (memq window-system '(mac ns))
    (run-with-idle-timer 1.15 nil #'exec-path-from-shell-initialize))
  (run-with-idle-timer 2.10 nil #'zoro-dashboard-enable-agenda))

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
