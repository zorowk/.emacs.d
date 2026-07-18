;;; init-reader.el --- Reading tools -*- lexical-binding: t -*-

;; Author: zorowk
;; Copyright (C) 2019 zorowk
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
  (elfeed-feeds '(
                  ("https://planet.emacslife.com/atom.xml" emacs planet)
                  ("https://www.phoronix.com/rss.php" linux phoronix)
                  ("https://hnrss.org/frontpage" hacker-news)
                  ("https://lobste.rs/rss" lobsters)))
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
