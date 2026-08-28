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
  :bind (("C-z e" . elfeed))
  :custom
  (elfeed-feeds
   '(("https://planet.emacslife.com/atom.xml"
      emacs planet)

     ("https://hnrss.org/frontpage"
      hacker-news)

     ("https://lobste.rs/rss"
      lobsters)

     ("https://tympanus.net/codrops/feed/"
      design codrops)

     ("https://www.awwwards.com/blog/feed/"
      design awwwards)

     ("https://sidebar.io/feed.xml"
      design frontend architecture)

     ("http://feeds2.feedburner.com/itsnicethat/SlXC"
      design photography typography)

     ("https://www.foreignaffairs.com/rss.xml"
      politics geopolitics usa)

     ("https://marginalrevolution.com/feed"
      economics markets ideas)

     ("https://noahpinion.substack.com/feed"
      economics china usa industry tech)

     ;; Linux kernel / systems / graphics stack
     ("https://lwn.net/headlines/rss"
      linux kernel systems graphics)

     ;; Vulkan / OpenGL / WebGL / glTF / OpenXR
     ("https://www.khronos.org/feeds/blog_feed"
      graphics vulkan opengl webgl khronos)

     ;; Browser engine / rendering / CSS / WebGPU
     ("https://webkit.org/blog/feed"
      webkit browser rendering frontend)

     ;; UX / typography / CSS / design systems
     ("https://www.smashingmagazine.com/feed/"
      design frontend ux typography)

     ;; Technology / infrastructure / institutions / progress
     ("https://worksinprogress.co/rss.xml"
      economics technology society infrastructure ideas)

     ;; Finance / technology / companies / macro
     ("https://www.thediff.co/rss/"
      economics finance tech business ideas)))

  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-save-multiple-enclosures-without-asking t)
  (elfeed-search-clipboard-type 'CLIPBOARD)
  (elfeed-search-date-format '("%Y-%m-%d" 10 :left))
  (elfeed-search-title-min-width 45))

(use-package elpher
  :ensure t
  :bind ("C-z b" . elpher))

(provide 'init-reader)
;;; init-reader.el ends here
