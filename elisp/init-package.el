;;; init-package.el --- Built-in package management -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Use package.el and use-package bundled with Emacs 31.  Package declarations
;; explicitly distinguish built-in packages (`:ensure nil') from packages
;; installed from GNU ELPA, NonGNU ELPA, or MELPA (`:ensure t').

;;; Code:

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("gnu" . 30)
        ("nongnu" . 20)
        ("melpa" . 10))
      package-pinned-packages
      '((crux . "melpa")
        (markdown-mode . "melpa"))
      ;; Never replace a library bundled with Emacs 31 merely because an
      ;; archive carries a newer version.
      package-install-upgrade-built-in nil)

;; Package activation happens before init.el and may have populated archive
;; metadata before the archives and pins above were configured.  Reload it on
;; demand so future installs and upgrades honor `package-pinned-packages'.
(setq package-archive-contents nil)

;; Normal startup activates installed packages after early-init.el and before
;; init.el.  Keep activation on that lightweight built-in path instead of
;; rescanning package descriptors and archive metadata here.

;; Keep package-selected-packages and all other Customize output out of the
;; hand-written init file, including during first-run package installation.
(setq custom-file (expand-file-name "custom-set-variables.el"
                                    user-emacs-directory))
(load custom-file 'noerror)

(require 'use-package)

(setopt use-package-always-ensure nil
        use-package-compute-statistics nil
        use-package-enable-imenu-support t
        use-package-expand-minimally t
        use-package-verbose nil)

(provide 'init-package)
;;; init-package.el ends here
