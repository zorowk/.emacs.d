;;; init-hyperbole.el --- Configure Hyperbole and HyWiki -*- lexical-binding: t -*-

;; Author: zorowk
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure GNU Hyperbole and keep HyWiki pages in Dropbox.

;;; Code:

(require 'init-const)

(use-package hyperbole
  :vc (:url "https://git.savannah.gnu.org/git/hyperbole.git"
       :rev :newest)
  :bind (("C-z h" . hyperbole))
  :init
  ;; Set this before Hyperbole loads so HyWiki initializes against the
  ;; persistent wiki rather than `user-emacs-directory'.
  (setq hywiki-directory zoro-hywiki-directory)
  :config
  (hyperbole-mode 1)
  ;; Recognize HyWikiWords in text buffers and programming comments.
  (hywiki-mode :all))

(provide 'init-hyperbole)
;;; init-hyperbole.el ends here
