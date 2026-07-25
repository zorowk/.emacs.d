;;; init-files.el --- File persistence and save behavior -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure file history, backups, locking, file modes, and save hooks.

;;; Code:

(use-package recentf
  :ensure nil
  :commands recentf-open-files
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

(defun zoro-recenter-after-restoring-place ()
  "Recenter after restoring point in a displayed file buffer."
  (when buffer-file-name
    (ignore-errors (recenter))))

(add-hook 'save-place-after-find-file-hook
          #'zoro-recenter-after-restoring-place)

(provide 'init-files)
;;; init-files.el ends here
