;;; init-hyperbole.el --- Configure Hyperbole and HyWiki -*- lexical-binding: t -*-

;; Author: zorowk
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure GNU Hyperbole and keep all personal Hyperbole data in Dropbox.

;;; Code:

(require 'init-const)

(use-package hyperbole
  :ensure t
  :bind (("C-z h" . hyperbole))
  :init
  ;; Set every personal data path before Hyperbole loads so none of its
  ;; home-directory defaults can take effect.
  (setq hbmap:dir-user zoro-hyperbole-directory
        hbmap:dir-filename
        (expand-file-name "HBMAP" zoro-hyperbole-directory)
        hywiki-directory zoro-hywiki-directory
        hywiki-org-publishing-directory zoro-hywiki-publishing-directory
        hyrolo-default-file zoro-hyrolo-file
        hyrolo-file-list (list zoro-hyrolo-file)
        hynote-directory-list (list zoro-org-directory
                                    zoro-denote-directory
                                    zoro-hywiki-directory))
  :config
  (hyperbole-mode 1)
  ;; Recognize HyWikiWords in text buffers and programming comments.
  (hywiki-mode :all))

(provide 'init-hyperbole)
;;; init-hyperbole.el ends here
