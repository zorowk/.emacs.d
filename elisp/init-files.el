;;; init-files.el --- File persistence and save behavior -*- lexical-binding: t -*-

;; Derived from M-EMACS configuration by Mingde (Matthew) Zeng.
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure file history, backups, locking, file modes, and save hooks.

;;; Code:

(use-package recentf
  :straight (:type built-in)
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup "05:00am")
  (recentf-autosave-interval 300)
  (recentf-show-messages nil)
  (recentf-max-saved-items 200)
  (recentf-exclude `(,(regexp-quote
                       (file-name-as-directory
                        (expand-file-name package-user-dir)))
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "url"
                     "COMMIT_EDITMSG\\'")))

(setopt save-place-autosave-interval 300)
(save-place-mode 1)

;; Keep recovery files out of source directories while retaining local locks.
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      backup-by-copying t)
(setq-default create-lockfiles t)
(setopt remote-file-name-inhibit-locks t
        remote-file-name-inhibit-auto-save-visited t)

;; Keep Customize output separate from hand-written configuration.
(setq custom-file (expand-file-name "custom-set-variables.el" user-emacs-directory))
(load custom-file 'noerror)

(global-so-long-mode)
(setq require-final-newline t
      large-file-warning-threshold 100000000)

(dolist (entry '(("\\.in\\'" . text-mode)
                 ("\\.out\\'" . text-mode)
                 ("\\.args\\'" . text-mode)
                 ("\\.bb\\'" . shell-script-mode)
                 ("\\.bbclass\\'" . shell-script-mode)
                 ("\\.Rmd\\'" . markdown-mode)))
  (add-to-list 'auto-mode-alist entry))

(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

(advice-add 'save-place-find-file-hook :after
            (lambda (&rest _)
              (when buffer-file-name
                (ignore-errors (recenter)))))

(provide 'init-files)
;;; init-files.el ends here
