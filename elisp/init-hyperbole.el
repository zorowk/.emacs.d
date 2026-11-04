;;; init-hyperbole.el --- Configure Hyperbole implicit links -*- lexical-binding: t -*-

;; Author: zorowk
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Enable implicit links and keep button data with this Emacs configuration.

;;; Code:

(use-package hyperbole
  :ensure t
  :defer 2
  :bind (("C-z h" . hyperbole))
  :init
  (setq hsys-org-enable-smart-keys 'buttons
        hywiki-default-mode nil
        hbmap:dir-user (expand-file-name "hyperbole/" user-emacs-directory)
        hbmap:dir-filename (expand-file-name "HBMAP" hbmap:dir-user))
  :config
  (hyperbole-mode 1))

(provide 'init-hyperbole)
;;; init-hyperbole.el ends here
