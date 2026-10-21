;;; init-reader.el --- Reading tools -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure Dictionary, Nov, Elfeed, and Elpher.

;;; Code:

(setq dictionary-use-single-buffer t)
(setq dictionary-server "dict.tw")
(global-set-key (kbd "C-c s") #'dictionary-lookup-definition)

(use-package nov
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package elfeed
  :ensure t
  :defer t
  :bind ("C-z e" . elfeed)
  :custom
  (elfeed-feeds
   '(;; Engineering
     ("https://planet.emacslife.com/atom.xml"
      engineering)
     ("https://lwn.net/headlines/rss"
      engineering upstream)
     ("https://www.khronos.org/feeds/blog_feed"
      engineering upstream)
     ("https://www.phoronix.com/rss.php"
      engineering upstream)
     ("https://planet.kde.org/global/rss20.xml"
      engineering upstream)
     ("https://planet.gnome.org/rss20.xml"
      engineering upstream)
     ("https://lobste.rs/rss"
      engineering)
     ("https://hnrss.org/frontpage"
      engineering)
     ("https://github.blog/feed/"
      engineering)

     ;; Opportunities
     ("https://hnrss.org/whoishiring/jobs"
      opportunity jobs)
     ("https://hnrss.org/whoishiring/freelance"
      opportunity consulting)

     ;; Business
     ("https://www.thediff.co/rss/"
      business opinion)
     ("https://steveblank.com/feed/"
      business)

     ;; Economy: analysis and opinion, not primary data.
     ("https://marginalrevolution.com/feed"
      economy opinion)
     ("https://noahpinion.substack.com/feed"
      economy opinion)
     ("https://worksinprogress.co/rss.xml"
      economy analysis)

     ;; Society and philosophy
     ("https://www.foreignaffairs.com/rss.xml"
      society opinion)
     ("https://aeon.co/feed.rss"
      philosophy)))

  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-save-multiple-enclosures-without-asking t)
  (elfeed-search-date-format '("%Y-%m-%d" 10 :left))
  (elfeed-search-title-min-width 45)
  :config
  (defun my/elfeed-hide-unread-tag (column)
    "Hide the unread label in the formatted tag COLUMN."
    (when column
      (let ((tags (delete "unread" (split-string (substring column 2 -1) ","))))
        (when tags
          (concat " (" (string-join tags ",") ")")))))
  (advice-add 'elfeed-search--column-tags :filter-return #'my/elfeed-hide-unread-tag))

(use-package elpher
  :ensure t
  :bind ("C-z b" . elpher))

(provide 'init-reader)
;;; init-reader.el ends here
