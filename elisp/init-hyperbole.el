;;; init-hyperbole.el --- Configure Hyperbole and HyWiki -*- lexical-binding: t -*-

;;; Commentary:
;; Configure GNU Hyperbole and keep HyWiki pages in Dropbox.

;;; Code:

(require 'init-const)

(use-package hyperbole
  ;; Keep Hyperbole off the startup critical path; enable it once Emacs has
  ;; been idle for a few seconds.
  :defer 8
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
