;;; init-hyperbole.el --- Configure Hyperbole implicit links -*- lexical-binding: t -*-

;; Author: zorowk
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Enable implicit links and keep button data with this Emacs configuration.

;;; Code:

(declare-function hywiki-init "hywiki" ())

(use-package hyperbole
  :ensure t
  :defer 2
  :bind (("C-z h" . hyperbole))
  :init
  ;; Hyperbole requires hywiki.el even when its mode is disabled.  Suppress
  ;; its separate initialization, which opens a temporary Org buffer and
  ;; scans the wiki directory, while retaining Hyperbole's other features.
  (advice-add 'hywiki-init :override #'ignore)
  ;; Link recognition also probes HyWiki while its mode is off.  These
  ;; page helpers create `hywiki-directory' even for a failed lookup.
  ;; Disable them before Hyperbole loads since HyWiki is unused here.
  (dolist (function '(hywiki-get-existing-page-file
                      hywiki-get-page-file
                      hywiki-get-page-files))
    (advice-add function :override #'ignore))
  (setq hsys-org-enable-smart-keys 'buttons
        hywiki-default-mode nil
        hbmap:dir-user (expand-file-name "hyperbole/" user-emacs-directory)
        hbmap:dir-filename (expand-file-name "HBMAP" hbmap:dir-user))
  :config
  (remove-hook 'after-init-hook #'hywiki-init)
  (hywiki-mode 0)
  (hyperbole-mode 1))

(provide 'init-hyperbole)
;;; init-hyperbole.el ends here
