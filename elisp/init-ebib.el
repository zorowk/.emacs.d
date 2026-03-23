;;; init-ebib.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-ebib.el
;; Description: Initialize Ebib
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Tue Mar 19 09:20:19 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d ebib ebib-here
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
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

;; Ebib
;; Bibtex stuff
(use-package ebib
  :defer t
  :bind (("C-z b" . ebib))
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

(provide 'init-ebib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ebib.el ends here
