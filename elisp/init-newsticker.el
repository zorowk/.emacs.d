;;; init-newsticker.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-newsticker.el
;; Description: Initialize Newsticker
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 17:32:54 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d fonts
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes newsticker.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
;; NewstickerPac

(use-package newsticker
  :ensure nil
  :defer t
  :init
  (setq newsticker-retrieval-interval 0
        newsticker-ticker-interval 0)
  :config
  (defun my/newsticker-treeview-in-new-tab ()
    (interactive)
    (let (succ)
      (unwind-protect
          (progn
            (tab-bar-new-tab)
            (call-interactively #'newsticker-treeview)
            (tab-bar-rename-tab "newsticker")
            (setq succ t))
        (unless succ
          (tab-bar-close-tab)))))

  (defun my/newsticker-treeview-quit-and-close-tab ()
    (interactive)
    (newsticker-treeview-quit)
    (newsticker-stop)
    (tab-close))

  (general-define-key
   :keymaps 'newsticker-treeview-mode-map
   :states 'normal
   "q" 'my/newsticker-treeview-quit-and-close-tab)

  :custom
  (newsticker-url-list '(
                         ;; ("title" "URL" other options)
                         ("Planet Emacs Life" "https://planet.emacslife.com/atom.xml" nil nil nil)
                         ("Karthinks" "https://karthinks.com/index.xml" nil nil nil)
                         ("lazycat" "https://manateelazycat.github.io/feed.xml" nil nil nil)
                         ("hacker news" "https://hnrss.org/frontpage")
                         ("lobsters" "https://lobste.rs/rss")
                         ("drew devault" "https://drewdevault.com/blog/index.xml")
                         ("Simon Ser" "https://emersion.fr/blog/atom.xml")
                         ))
  (newsticker-retrieval-method 'extern)
  (newsticker-wget-name "curl")
  (newsticker-wget-arguments '("--disable" "--silent" "--location" "--proxy" "socks5://127.0.0.1:7891"))
  (newsticker-url-list-defaults nil)    ;remove default list (i.e. emacswiki)
  (newsticker-automatically-mark-items-as-old nil))
;; -NewstickerPac

(provide 'init-newsticker)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-newsticker.el ends here
