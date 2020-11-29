;;; init-orgmisc.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-orgmisc.el
;; Description: Initialize journal and brain
;; Author: zorkwk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 08:40:27 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d org brain
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes brain and journal
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

;; org journal
(use-package org-journal
  :ensure t
  :after org
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/Dropbox/journal/"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-file-type 'weekly)

  (defun org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything \n\n")
       (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded \n\n")
       (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded \n\n")
       (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded \n\n"))))
  (setq org-journal-file-header 'org-journal-file-header-func))
;; -org journal

;; org-ref
(use-package org-ref
  :after org
  :init
  (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))
  (setq org-ref-bibliography-notes "~/Dropbox/bibliography/books.org"
        org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
        org-ref-pdf-directory "~/Dropbox/bibliography/book")

  (setq bibtex-completion-bibliography "~/Dropbox/bibliography/references.bib"
             bibtex-completion-library-path "~/Dropbox/bibliography/book"
             bibtex-completion-notes-path "~/Dropbox/bibliography/bibnotes.org")
  :config
  (key-chord-define-global "uu" 'org-ref-cite-hydra/body)
  ;; variables that control bibtex key format for auto-generation
  ;; I want firstauthor-year-title-words
  ;; this usually makes a legitimate filename to store pdfs under.
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5))
;; -org-ref

;; org gkroam
(use-package gkroam
  :ensure t
  :init
  (setq gkroam-root-dir "~/Dropbox/think/")
  (setq gkroam-prettify-p t
        gkroam-show-brackets-p t
        gkroam-use-default-filename t
        gkroam-window-margin 4)
  :bind
  (("C-c r I" . gkroam-index)
   ("C-c r d" . gkroam-daily)
   ("C-c r f" . gkroam-find)
   ("C-c r i" . gkroam-insert)
   ("C-c r c" . gkroam-capture)
   ("C-c r e" . gkroam-link-edit)
   ("C-c r n" . gkroam-smart-new)
   ("C-c r p" . gkroam-toggle-prettify)
   ("C-c r t" . gkroam-toggle-brackets)
   ("C-c r D" . gkroam-toggle-dynamic)
   ("C-c r g" . gkroam-update)))
;; -org gkroam

;; org bullets
(use-package org-bullets
  :init
  (setq org-bullets-bullet-list '("●" "○" "●" "○" "●" "◉" "○" "◆"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;; -org bullets

;; org valign
(use-package valign
  :load-path (lambda () (expand-file-name "site-elisp/valign" user-emacs-directory))
  :config
  (add-hook 'org-mode-hook #'valign-mode))
;; -org valign

;;
(provide 'init-orgmisc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-orgmisc.el ends here
