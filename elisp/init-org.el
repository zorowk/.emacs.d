;;; init-org.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-org.el
;; Description: Initialize Org, Toc-org, HTMLize, OX-GFM
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:09:30 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d org toc-org htmlize ox-gfm
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes org toc-org htmlize ox-gfm
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

;; OrgPac
(use-package org
  :ensure nil
  :defer t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :custom
  (org-log-done 'time)
  (calendar-latitude 114.3052) ;; Prerequisite: set it to your location, currently default: Toronto, Canada
  (calendar-longitude 30.5928) ;; Usable for M-x `sunrise-sunset' or in `org-agenda'
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-latex-listings-options '(("breaklines" "true")))
  (org-latex-listings t)
  (org-deadline-warning-days 7)
  (org-agenda-window-setup 'other-window)
  (org-latex-pdf-process
    (let
      ((cmd (concat "pdflatex -shell-escape -interaction nonstopmode"
		    " -output-directory %o %f")))
      (list cmd
	    "cd %o; if test -r %b.idx; then makeindex %b.idx; fi"
	    "cd %o; bibtex %b"
	    cmd
	    cmd
	    "rm -rf %b.out %b.log %b.tex %b.bbl %b.ind auto")))
  :custom-face
  (org-agenda-current-time ((t (:foreground "spring green"))))
  :config
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (unless (version< org-version "9.2")
    (require 'org-tempo))

  ;; config stuck project
  (setq org-stuck-projects
        '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)"))))

  ;; Change task state to STARTED when clocking in
  (setq org-clock-in-switch-to-state "STARTED")
  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  (setq org-log-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line
  (setq org-tags-match-list-sublevels nil)

  (setq org-plantuml-jar-path
        (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))
  (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (shell . t)
     (latex .t)
     (python . t)
     (emacs-lisp . t)
     (plantuml . t)
     (C . t)
     (ditaa . t)
     (gnuplot . t)))

  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.x?html?\\'" . "google-chrome-stable %s")
          ("\\.mp4\\'" . "mpv \"%s\"")
          ("\\.mkv" . "mpv \"%s\"")))

  ;; define the refile targets
  (setq org-agenda-dir "~/Dropbox/brain/")
  (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
  (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
  (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
  (setq org-agenda-files (list org-agenda-dir))

  ;; the %i would copy the selected text into the template
  ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
  ;;add multi-file journal
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Personal")
           "* TODO [#B] %?\n  %i\n"
           :empty-lines 1)
          ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
           "* %?\n  %i\n %U"
           :empty-lines 1)
          ("l" "Learn" entry (file+headline org-agenda-file-note "Learning")
           "* TODO [#B] %?\n  %i\n %U"
           :empty-lines 1)
          ("s" "Code Snippet" entry (file org-agenda-file-code-snippet)
           "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
          ("w" "work" entry (file+headline org-agenda-file-gtd "Deepin")
           "* TODO [#A] %?\n  %i\n %U"
           :empty-lines 1)
          ("p" "Protocol" entry (file+headline org-agenda-file-note "Chrome Content")
           "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
           :empty-lines 1)
          ("L" "Protocol Link" entry (file+headline org-agenda-file-note "Chrome Links")
           "* %? [[%:link][%:description]] \nCaptured On: %U"
           :empty-lines 1)))

  (setq bibtex-completion-bibliography "~/Dropbox/bibliography/references.bib"
        bibtex-completion-library-path "~/Dropbox/bibliography/book"
        bibtex-completion-notes-path "~/Dropbox/bibliography/bibnotes.org")

  (defun org-export-toggle-syntax-highlight ()
    "Setup variables to turn on syntax highlighting when calling `org-latex-export-to-pdf'."
    (interactive)
    (setq-local org-latex-listings 'minted)
    (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

  (defun org-table-insert-vertical-hline ()
    "Insert a #+attr_latex to the current buffer, default the align to |c|c|c|, adjust if necessary."
    (interactive)
    (insert "#+attr_latex: :align |c|c|c|")))
;; -OrgPac

;; TocOrgPac
(use-package toc-org
  :hook (org-mode . toc-org-mode))
;; -TocOrgPac

;; HTMLIZEPac
(use-package htmlize :defer t)
;; -HTMLIZEPac

;; OXGFMPac
(use-package ox-gfm :defer t)
;; -OXGFMPac

;; PlantUMLPac
(use-package plantuml-mode
  :defer t
  :custom
  (org-plantuml-jar-path (expand-file-name "/usr/share/java/plantuml/plantuml.jar")))
;; -PlantUMLPac

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
