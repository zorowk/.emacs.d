;;; init.el --- Personal Emacs configuration -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Load the personal configuration by feature.

;;; Code:

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

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
(require 'init-gnuplot)

(require 'init-latex)

;; Office
(require 'init-org)

(require 'init-hyperbole)

(require 'init-reader)

;; Internet
(require 'init-erc)

(require 'init-llm)

(require 'init-gnus)

(unless noninteractive
  (when (memq window-system '(mac ns))
    (zoro-import-shell-environment))
  (zoro-dashboard-load))

(provide 'init)
;;; init.el ends here
