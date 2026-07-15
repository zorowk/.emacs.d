;;; init-hyperbole.el --- Configure Hyperbole and HyWiki -*- lexical-binding: t -*-

;;; Commentary:
;; Configure GNU Hyperbole and keep HyWiki pages in Dropbox.

;;; Code:

(use-package hyperbole
  :demand t
  :init
  ;; Set this before Hyperbole loads so HyWiki initializes against the
  ;; persistent wiki rather than `user-emacs-directory'.
  (setq hywiki-directory
        (file-name-as-directory (expand-file-name "~/Dropbox/hywiki/")))
  :config
  (hyperbole-mode 1)
  ;; Recognize HyWikiWords in text buffers and programming comments.
  (hywiki-mode :all))

(provide 'init-hyperbole)
;;; init-hyperbole.el ends here
