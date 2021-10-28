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

;; org-ref
(use-package ivy-bibtex
  :init
  (setq bibtex-completion-bibliography '("~/Dropbox/bibliography/references.bib")
        bibtex-completion-library-path '("~/Dropbox/bibliography/book/")
	    bibtex-completion-notes-path "~/Dropbox/emacs/bibliography/notes/"
	    bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
        bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "open" nil 0 nil fpath))))

(use-package org-ref
  :ensure nil
 ;; :load-path (lambda () (expand-file-name "org-ref" scimax-dir))
  :init
;;  (add-to-list 'load-path
;;               (expand-file-name "org-ref" scimax-dir))
  (require 'bibtex)
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5)
  (define-key bibtex-mode-map (kbd "H-b") 'org-ref-bibtex-hydra/body)
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  (define-key org-mode-map (kbd "s-[") 'org-ref-insert-link-hydra/body)
  (require 'org-ref-ivy)
  (require 'org-ref-arxiv)
  (require 'org-ref-scopus)
  (require 'org-ref-wos))

(use-package org-ref-ivy
  :ensure nil
;;  :load-path (lambda () (expand-file-name "org-ref" scimax-dir))
  :init (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
              org-ref-insert-cite-function 'org-ref-cite-insert-ivy
              org-ref-insert-label-function 'org-ref-insert-label-link
              org-ref-insert-ref-function 'org-ref-insert-ref-link
              org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))
;; -org-ref

;; org roam
(use-package org-roam
      :ensure t
      :custom
      (org-roam-directory "~/Dropbox/notes/")
      :bind (("C-c n l" . org-roam-buffer-toggle)
             ("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n i" . org-roam-node-insert)
             ("C-c n c" . org-roam-capture)
             ;; Dailies
             ("C-c n j" . org-roam-dailies-capture-today))
      :config
      (org-roam-setup)
      (require 'org-roam-protocol)
      (setq org-roam-dailies-capture-templates
            '(("d" "default" entry
               #'org-roam-capture--get-point
               "* %?"
               :file-name "daily/%<%Y-%m-%d>"
               :head "#+title: %<%Y-%m-%d>\n\n"))))

(use-package org-roam-bibtex
  :after org-roam
  :config
  (require 'org-ref))
;; -org roam

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
