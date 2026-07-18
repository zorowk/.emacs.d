;;; init-appearance.el --- Theme selection and system appearance -*- lexical-binding: t -*-

;; Derived from M-EMACS configuration by Mingde (Matthew) Zeng.
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Load the selected theme early and follow macOS appearance changes.

;;; Code:

(require 'init-function)

(use-package ef-themes
  :ensure t
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :config
  (setq modus-themes-mixed-fonts t
        modus-themes-italic-constructs t)

  (add-hook 'ns-system-appearance-change-functions #'zoro-apply-theme)
  (zoro-apply-theme 'light))

(provide 'init-appearance)
;;; init-appearance.el ends here
