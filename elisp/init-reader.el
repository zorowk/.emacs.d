;;; init-reader.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-reader.el
;; Description: emacs reader
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Tue Jun  4 00:26:09 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d reader
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes emacs reader
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

;; dictionary
(setq dictionary-use-single-buffer t)
(global-set-key (kbd "C-c C-l") #'dictionary-lookup-definition)

(when (eq system-type 'darwin)
  (use-package osx-dictionary
    :if (eq system-type 'darwin)
    :defer t
    :bind ("C-c C-l" . osx-dictionary-search-pointer)))
;; -dictionary

;; nov
(use-package nov
  :defer t
  :init
  (add-hook 'nov-mode-hook #'shrface-mode)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))
;; -nov

;; elfeed
(use-package elfeed
  :defer t
  :bind (("C-z e" . elfeed))
  :custom
  (elfeed-feeds '(
                  ("https://planet.emacslife.com/atom.xml" emacs planet)
                  ("https://www.phoronix.com/rss.php" linux phoronix)
                  ("https://hnrss.org/frontpage" hacker-news)
                  ("https://lobste.rs/rss" lobsters)))

  (setq-default elfeed-db-directory
                (expand-file-name "elfeed" user-emacs-directory)
                elfeed-save-multiple-enclosures-without-asking t
                elfeed-search-clipboard-type 'CLIPBOARD
                elfeed-search-date-format '("%Y-%m-%d" 10 :left)
                elfeed-search-title-min-width 45))
;; -elfeed

(provide 'init-reader)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-reader.el ends here
