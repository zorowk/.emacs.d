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

;;; Shared constants and named functions
(require 'init-const)

;;; Package management
(require 'init-package)

;;; Appearance
;; Apply the selected theme before loading nonessential startup modules.
(require 'init-ui)

;;; Core behavior and editing
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

;;; Programming
(require 'init-gnuplot)

(require 'init-latex)

;;; Org and reading
(require 'init-org)


(require 'init-reader)

;;; Internet
(require 'init-erc)

(require 'init-llm)

(require 'init-gnus)

;;; Interactive startup
(unless noninteractive
  (when (display-graphic-p)
    (zoro-import-shell-environment))
  (zoro-dashboard-load))

(provide 'init)
;;; init.el ends here
