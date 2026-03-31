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

;; ReaderPac
(when (eq system-type 'gnu/linux)
  (use-package reader
    :defer t
    :straight '(reader :type git :host codeberg :repo "divyaranjan/emacs-reader"
                       :files ("*.el" "render-core.so")
                       :pre-build ("make" "all"))
    :config
    (setq reader-supported-formats
          (delete "epub" reader-supported-formats))))
;; -ReaderPac

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
  (elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch #'elfeed-display-buffer)

  (defun elfeed-display-buffer (buf &optional act)
    (pop-to-buffer buf)
    (set-window-text-height (get-buffer-window) (round (* 0.7 (frame-height)))))

  (defun elfeed-search-show-entry-pre (&optional lines)
    "Returns a function to scroll forward or back in the Elfeed
     search results, displaying entries without switching to them."
    (lambda (times)
      (interactive "p")
      (forward-line (* times (or lines 0)))
      (recenter)
      (call-interactively #'elfeed-search-show-entry)
      (select-window (previous-window))
      (unless elfeed-search-remain-on-entry (forward-line -1))))

  (define-key elfeed-search-mode-map (kbd "n") (elfeed-search-show-entry-pre +1))
  (define-key elfeed-search-mode-map (kbd "p") (elfeed-search-show-entry-pre -1))
  (define-key elfeed-search-mode-map (kbd "M-RET") (elfeed-search-show-entry-pre))

  (defun elfeed-scroll-up-command (&optional arg)
    "Scroll up or go to next feed item in Elfeed"
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-up-command arg)
        (error (elfeed-show-next)))))

  (defun elfeed-scroll-down-command (&optional arg)
    "Scroll up or go to next feed item in Elfeed"
    (interactive "^P")
    (let ((scroll-error-top-bottom nil))
      (condition-case-unless-debug nil
          (scroll-down-command arg)
        (error (elfeed-show-prev)))))

  (define-key elfeed-show-mode-map (kbd "SPC") 'elfeed-scroll-up-command)
  (define-key elfeed-show-mode-map (kbd "S-SPC") 'elfeed-scroll-down-command)

  (defun elfeed-tag-selection-as (mytag)
    "Returns a function that tags an elfeed entry or selection as
     MYTAG"
    (lambda ()
      "Toggle a tag on an Elfeed search selection"
      (interactive)
      (elfeed-search-toggle-all mytag)))

  (define-key elfeed-search-mode-map "l" (elfeed-tag-selection-as 'readlater))
  (define-key elfeed-search-mode-map "d" (elfeed-tag-selection-as 'junk))

  (defun elfeed-show-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-show-visit use-generic-p)))

  (defun elfeed-search-eww-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-search-browse-url use-generic-p)))

  (define-key elfeed-show-mode-map (kbd "B") 'efleed-show-eww-open)
  (define-key elfeed-search-mode-map (kbd "B") 'efleed-search-eww-open)

  (setq browse-url-browser-function
      '(("https:\\/\\/www\\.youtu\\.*be." . browse-url-mpv)
        ("." . browse-url-generic)))

  (defun browse-url-mpv (url &optional single)
    (start-process "mpv" nil "mpv" (shell-quote-argument url))))
;; -elfeed

(provide 'init-reader)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-reader.el ends here
