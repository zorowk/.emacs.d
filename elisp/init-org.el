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

;; OrgPac
(use-package org
  :straight (:type built-in)
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
  (org-habit-graph-column 80)
  (org-duration-format 'h:mm) ;; show hours at max, not days
  (org-agenda-compact-blocks t)
  (org-agenda-span 'day)
  (org-agenda-start-day "-0d")
  (org-agenda-start-on-weekday nil)
  (org-latex-pdf-process
    (let
      ((cmd (concat "xelatex -shell-escape -interaction nonstopmode"
		    " -output-directory %o %f")))
      (list cmd
	    "cd %o; if test -r %b.idx; then makeindex %b.idx; fi"
	    "cd %o; bibtex %b"
	    cmd
	    cmd
	    "rm -rf %b.out %b.log %b.tex %b.bbl %b.ind auto")))
  (org-latex-compiler "xelatex")
  :custom-face
  (org-agenda-current-time ((t (:foreground "spring green"))))
  :config
  (add-to-list 'org-latex-packages-alist '("" "listings"))

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

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (shell . t)
     (latex .t)
     (python . t)
     (R . t)
     (julia . t)
     (emacs-lisp . t)
     (maxima . t)
     (gnuplot . t)))

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

;; Denote
(use-package denote
  :defer t
  :hook
  (;; If you use plain text files (.txt), then you want to make the
   ;; Denote links clickable (Org mode and Markdown mode render links
   ;; as buttons right away and provide commands to open them)
   (text-mode . denote-fontify-links-mode)
   ;; Apply colours to Denote names in Dired.  This applies to all
   ;; directories.  Check `denote-dired-directories' for the specific
   ;; directories you may prefer instead.  Then, instead of
   ;; `denote-dired-mode', use `denote-dired-mode-in-directories'.
   (dired-mode . denote-dired-mode))
  :bind
  ;; Denote DOES NOT define any key bindings.  This is for the user to
  ;; decide.  For example:
  ( :map global-map
    ("C-c n n" . denote)
    ("C-c n d" . denote-dired)
    ("C-c n g" . denote-grep)
    ;; If you intend to use Denote with a variety of file types, it is
    ;; easier to bind the link-related commands to the `global-map', as
    ;; shown here.  Otherwise follow the same pattern for `org-mode-map',
    ;; `markdown-mode-map', and/or `text-mode-map'.
    ("C-c n l" . denote-link)
    ("C-c n L" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ("C-c n q c" . denote-query-contents-link) ; create link that triggers a grep
    ("C-c n q f" . denote-query-filenames-link) ; create link that triggers a dired
    ;; Note that `denote-rename-file' can work from any context, not just
    ;; Dired bufffers.  That is why we bind it here to the `global-map'.
    ("C-c n r" . denote-rename-file)
    ("C-c n R" . denote-rename-file-using-front-matter)

    ;; Key bindings specifically for Dired.
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))

  :config
  ;; Remember to check the doc string of each of those variables.
  (setq denote-directory (expand-file-name "~/Dropbox/notes/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("deepin" "book" "math" "blog"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-keywords-to-not-infer-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))

  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)

  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :defer t
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(use-package denote-org
  :defer t
  :commands
  ;; I list the commands here so that you can discover them more
  ;; easily.  You might want to bind the most frequently used ones to
  ;; the `org-mode-map'.
  (denote-org-link-to-heading
    denote-org-backlinks-for-heading
    denote-org-extract-org-subtree
    denote-org-convert-links-to-file-type
    denote-org-convert-links-to-denote-type
    denote-org-dblock-insert-files
    denote-org-dblock-insert-links
    denote-org-dblock-insert-backlinks
    denote-org-dblock-insert-missing-links
    denote-org-dblock-insert-files-as-headings))
;; -Denote

;; org bullets
(use-package org-bullets
  :after org
  :init
  (setq org-bullets-bullet-list '("●" "○" "●" "○" "●" "◉" "○" "◆"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;; -org bullets

;; ox-hugo
(use-package ox-hugo
  :defer t   ;Auto-install the package from Melpa
  :after ox)
;; -ox-hugo

;; Ebib
;; Bibtex stuff
(use-package ebib
  :defer t
  :bind (("C-z ." . ebib))
  (:map ebib-index-mode-map
              ("B" . ebib-biblio-import-doi))
  :init
  (defun zw/ebib-create-identifier (key _) key)
  (setq ebib-preload-bib-files '("~/Dropbox/bibliography/references.bib")
        ebib-notes-default-file "~/Dropbox/bibliography/notes.org"
        ebib-notes-template "* %T\n:PROPERTIES:\n%K\n:NOTER_DOCUMENT: papers/%k.pdf\n:END:\n%%?\n"
        ebib-keywords (expand-file-name "~/Dropbox/bibliography/keywords.txt")
        ebib-reading-list-file "~/Dropbox/brain/readlist.org"
        ebib-bib-search-dirs  '("~/Dropbox/bibliography/")
        ebib-bibtex-dialect 'biblatex
        ebib-autogenerate-keys nil
        ebib-notes-storage 'multiple-notes-per-file)
  :config
  (add-to-list 'ebib-notes-template-specifiers '(?k . zw/ebib-create-identifier))
  (add-to-list 'ebib-file-search-dirs "~/Documents/papers")
  (if (eq system-type 'darwin)
      (add-to-list 'ebib-file-associations '("pdf" . "open"))
    (add-to-list 'ebib-file-associations '("pdf" . nil)))
  (add-to-list 'ebib-citation-commands '(org-mode (("ref" "[cite:@%(%K%,)]"))))
  (add-to-list 'ebib-citation-commands '(context-mode (("cite" "\\cite[%(%K%,)]")
                                                       ("authoryear" "\\cite[authoryear][%(%K%,)]")
                                                       ("authoryears" "\\cite[authoryears][%(%K%,)]")
                                                       ("entry" "\\cite[entry][%(%K%,)]")
                                                       ("author" "\\cite[author][%(%K%,)]"))))

  (advice-add 'bibtex-generate-autokey :around
              (lambda (orig-func &rest args)
                (replace-regexp-in-string ":" "" (apply orig-func args))))
  (remove-hook 'ebib-notes-new-note-hook #'org-narrow-to-subtree))

(defun sci-hub-pdf-url (doi)
  "Get url to the pdf from SCI-HUB using DOI."
  (setq *doi-utils-pdf-url* (concat "https://sci-hub.hkvisa.net/" doi) ;captcha
        *doi-utils-waiting* t
        )
  ;; try to find PDF url (if it exists)
  (url-retrieve (concat "https://sci-hub.hkvisa.net/" doi)
                (lambda (_)
                  (goto-char (point-min))
                  (while (search-forward-regexp
                          "\\(https:\\|sci-hub.hkvisa.net/downloads\\).+download=true'" nil t)
                    (let ((foundurl (match-string 0)))
                      (message foundurl)
                      (if (string-match "https:" foundurl)
                          (setq *doi-utils-pdf-url* foundurl)
                        (setq *doi-utils-pdf-url* (concat "https:" foundurl))))
                    (setq *doi-utils-waiting* nil))))
  (while *doi-utils-waiting* (sleep-for 0.1))
  (replace-regexp-in-string "\\\\" "" *doi-utils-pdf-url*))

(defun acm-pdf-url (doi)
  "Retrieve a DOI pdf from the ACM."
  (concat "https://dl.acm.org/doi/pdf/" doi))

(defun ieee-pdf-url (doi)
  "Retrieve a DOI pdf from the IEEE."
  (when (string-match "\\.\\([0-9]*\\)$" doi)
    (let ((doi-bit (match-string 1 doi)))
      (concat "https://ieeexplore.ieee.org/stampPDF/getPDF.jsp?tp=&arnumber=" doi-bit "&ref="))))

(defun springer-pdf-url (doi)
  "Retrieve a DOI pdf from the Springer."
  (concat "https://link.springer.com/content/pdf/" doi ".pdf"))

(defun arxiv-pdf-url (epr)
  (concat "https://arxiv.org/pdf/" epr ".pdf"))

(defun download-pdf-from-doi (key &optional doi publisher eprint journal organization url)
  "Download pdf from doi with KEY name."
  (let ((pub  (or publisher ""))
        (epr  (or eprint ""))
        (jour (or journal ""))
        (org  (or organization ""))
        (link (or url "")))
    (url-copy-file (cond
                    ((not doi) link)
                    ((or (string-match "ACM" (s-upcase pub))
                         (string-match "association for computing machinery" (s-downcase pub)))
                     (acm-pdf-url doi))
                    ((string-match "arxiv" (s-downcase pub))
                     (arxiv-pdf-url epr))
                    ((or (string-match "IEEE" (s-upcase pub))
                         (string-match "IEEE" (s-upcase jour))
                         (string-match "IEEE" (s-upcase org)))
                     (ieee-pdf-url doi))
                    ((string-match "springer" (s-downcase pub))
                     (springer-pdf-url doi))
                    (t (sci-hub-pdf-url doi)))
                   (concat (car ebib-file-search-dirs) "/" key ".pdf"))))

(defun download-pdf-from-link (link key)
  (url-copy-file link
                 (concat (car ebib-file-search-dirs) "/" key ".pdf")))

(defun download-pdf-from-downloads (key)
  (copy-file (concat "~/Downloads/" key ".pdf")
             (concat (car ebib-file-search-dirs) "/" key ".pdf") t))

(defun get-bib-from-doi (doi)
  "Get the bibtex from DOI."
  (shell-command (concat "curl -L -H \"Accept: application/x-bibtex; charset=utf-8\" "
                         "https://doi.org/" doi)))

(defun ebib-download-pdf-from-doi ()
  "Download a PDF for the current entry."
  (interactive)
  (let* ((key (ebib--get-key-at-point))
         (doi (ebib-get-field-value "doi" key ebib--cur-db 'noerror 'unbraced 'xref))
         (publisher (ebib-get-field-value "publisher" key ebib--cur-db 'noerror 'unbraced 'xref))
         (eprinttype (ebib-get-field-value "eprinttype" key ebib--cur-db 'noerror 'unbraced 'xref))
         (eprint (ebib-get-field-value "eprint" key ebib--cur-db 'noerror 'unbraced 'xref))
         (journal (ebib-get-field-value "journal" key ebib--cur-db 'noerror 'unbraced 'xref))
         (journaltitle (ebib-get-field-value "journaltitle" key ebib--cur-db 'noerror 'unbraced 'xref))
         (organization (ebib-get-field-value "organization" key ebib--cur-db 'noerror 'unbraced 'xref))
         (url (ebib-get-field-value "url" key ebib--cur-db 'noerror 'unbraced 'xref)))
    (unless key
      (error "[Ebib] No key assigned to entry"))
    (download-pdf-from-doi key doi (or publisher eprinttype) eprint (or journal journaltitle) organization url)))

(defun ebib-check-file ()
  "Download a PDF for the current entry."
  (interactive)
  (let ((key (ebib--get-key-at-point)))
    (unless (file-exists-p (concat (car ebib-file-search-dirs) "/" key ".pdf"))
      (error "[Ebib] No PDF found."))
    t))
;; -Ebib

;; MarkdownModePac
(use-package markdown-mode :defer t)
;; -MarkdownModePac

;; Olivetti
(use-package olivetti :defer t)
;; -Olivetti

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
