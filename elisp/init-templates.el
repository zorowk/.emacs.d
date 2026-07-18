;;; init-templates.el --- Built-in Tempo templates -*- lexical-binding: t -*-

;; Author: zorowk
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Define mode-aware templates using Emacs's built-in Tempo library.

;;; Code:

(require 'tempo)

(setopt tempo-interactive t)

(defun zoro-tempo--define (scope tag elements &optional documentation)
  "Define a Tempo template for SCOPE named TAG from ELEMENTS.

DOCUMENTATION describes the generated insertion command.  A nil SCOPE makes
the template available globally."
  (tempo-define-template
   (format "zoro-%s-%s" (or scope "global") tag)
   elements tag documentation
   (and scope (intern (format "zoro-tempo-%s-tags" scope)))))

(defun zoro-tempo--latex-matrix ()
  "Read matrix dimensions and return a LaTeX matrix string."
  (let* ((rows (read-number "Rows: " 2))
         (columns (read-number "Columns: " 2))
         (type (read-string "Matrix type: " nil nil "pmatrix"))
         (row (string-join (make-list columns "") " & ")))
    (concat "\\begin{" type "}\n"
            (string-join (make-list rows row) " \\\\\n")
            "\n\\end{" type "}")))

(defun zoro-tempo--text-table ()
  "Read table dimensions and return an Org-style table string."
  (let* ((rows (read-number "Rows: " 2))
         (columns (read-number "Columns: " 2))
         (row (concat "| " (string-join (make-list columns "  ") " | ") " |"))
         (separator (concat "|" (string-join (make-list columns "----") "+") "|")))
    (string-join (append (list row separator) (make-list rows row)) "\n")))

(defun zoro-tempo--match-tag ()
  "Return the Tempo tag immediately before point and its start position."
  (let ((end (point)))
    (save-excursion
      (skip-syntax-backward "w_")
      (when (< (point) end)
        (when (eq (char-before) ?<)
          (backward-char))
        (cons (buffer-substring-no-properties (point) end) (point))))))

(defun zoro-tempo-setup ()
  "Install the Tempo tag lists appropriate for the current major mode."
  (when (derived-mode-p 'prog-mode 'conf-mode)
    (tempo-use-tag-list 'zoro-tempo-prog-tags))
  (when (derived-mode-p 'text-mode)
    (tempo-use-tag-list 'zoro-tempo-text-tags))
  (when (derived-mode-p 'latex-mode 'LaTeX-mode)
    (tempo-use-tag-list 'zoro-tempo-latex-tags))
  (when (derived-mode-p 'texinfo-mode)
    (tempo-use-tag-list 'zoro-tempo-texinfo-tags))
  (when (derived-mode-p 'lisp-mode 'emacs-lisp-mode 'lisp-interaction-mode)
    (tempo-use-tag-list 'zoro-tempo-lisp-tags))
  (when (derived-mode-p 'eshell-mode)
    (tempo-use-tag-list 'zoro-tempo-eshell-tags))
  (when (derived-mode-p 'rst-mode)
    (tempo-use-tag-list 'zoro-tempo-rst-tags))
  (when (derived-mode-p 'java-mode)
    (tempo-use-tag-list 'zoro-tempo-java-tags))
  (when (derived-mode-p 'c-mode)
    (tempo-use-tag-list 'zoro-tempo-c-tags))
  (when (derived-mode-p 'org-mode)
    (tempo-use-tag-list 'zoro-tempo-org-tags))
  (setq-local tempo-match-finder #'zoro-tempo--match-tag))

(defun zoro-tempo-complete-tag ()
  "Expand the Tempo tag immediately before point."
  (interactive)
  (zoro-tempo-setup)
  (call-interactively #'tempo-complete-tag))

(defun zoro-tempo-insert (tag)
  "Select and insert a Tempo template by TAG."
  (interactive
   (progn
     (zoro-tempo-setup)
     (list (completing-read "Template: " (tempo-build-collection) nil t))))
  (zoro-tempo-setup)
  (tempo-insert-template (cdr (assoc tag (tempo-build-collection)))
                         current-prefix-arg))

(defvar zoro-tempo-prog-tags nil)
(defvar zoro-tempo-latex-tags nil)
(defvar zoro-tempo-texinfo-tags nil)
(defvar zoro-tempo-lisp-tags nil)
(defvar zoro-tempo-eshell-tags nil)
(defvar zoro-tempo-text-tags nil)
(defvar zoro-tempo-rst-tags nil)
(defvar zoro-tempo-java-tags nil)
(defvar zoro-tempo-c-tags nil)
(defvar zoro-tempo-org-tags nil)

(global-set-key (kbd "M-+") #'zoro-tempo-complete-tag)
(global-set-key (kbd "M-*") #'zoro-tempo-insert)
(global-set-key (kbd "C-c t n") #'tempo-forward-mark)
(add-hook 'after-change-major-mode-hook #'zoro-tempo-setup)

;; Available in every major mode.
(zoro-tempo--define
 nil "today" '((format-time-string "%Y-%m-%d")) "Insert today's date.")

;; Programming modes.
(dolist (entry
         '(("fixme" ((or comment-start ";; ") "FIXME "))
           ("todo" ((or comment-start ";; ") "TODO "))
           ("bug" ((or comment-start ";; ") "BUG "))
           ("hack" ((or comment-start ";; ") "HACK "))))
  (zoro-tempo--define "prog" (car entry) (cadr entry)))

;; LaTeX modes.
(dolist (entry
         '(("abstract" ("\\begin{abstract}" n> r> n> "\\end{abstract}"))
           ("align" ("\\begin{align}" n> r> n> "\\end{align}"))
           ("alignn" ("\\begin{align*}" n> r> n> "\\end{align*}"))
           ("gather" ("\\begin{gather}" n> r> n> "\\end{gather}"))
           ("gatherr" ("\\begin{gather*}" n> r> n> "\\end{gather*}"))
           ("appendix" ("\\begin{appendix}" n> r> n> "\\end{appendix}"))
           ("center" ("\\begin{center}" n> r> n> "\\end{center}"))
           ("displaymath" ("\\begin{displaymath}" n> r> n> "\\end{displaymath}"))
           ("document" ("\\begin{document}" n> r> n> "\\end{document}"))
           ("enumerate" ("\\begin{enumerate}" n> "\\item " r> n> "\\end{enumerate}"))
           ("equation" ("\\begin{equation}" n> r> n> "\\end{equation}"))
           ("flushleft" ("\\begin{flushleft}" n> r> n> "\\end{flushleft}"))
           ("flushright" ("\\begin{flushright}" n> r> n> "\\end{flushright}"))
           ("fussypar" ("\\begin{fussypar}" n> r> n> "\\end{fussypar}"))
           ("itemize" ("\\begin{itemize}" n> "\\item " r> n> "\\end{itemize}"))
           ("letter" ("\\begin{letter}" n> r> n> "\\end{letter}"))
           ("math" ("\\begin{math}" n> r> n> "\\end{math}"))
           ("minipage" ("\\begin{minipage}[t]{0.5\\linewidth}" n> r> n> "\\end{minipage}"))
           ("quotation" ("\\begin{quotation}" n> r> n> "\\end{quotation}"))
           ("quote" ("\\begin{quote}" n> r> n> "\\end{quote}"))
           ("sloppypar" ("\\begin{sloppypar}" n> r> n> "\\end{sloppypar}"))
           ("theindex" ("\\begin{theindex}" n> r> n> "\\end{theindex}"))
           ("trivlist" ("\\begin{trivlist}" n> r> n> "\\end{trivlist}"))
           ("verbatim" ("\\begin{verbatim}" n r n "\\end{verbatim}"))
           ("verbatimm" ("\\begin{verbatim*}" n r n "\\end{verbatim*}"))))
  (zoro-tempo--define "latex" (car entry) (cadr entry)))
(zoro-tempo--define
 "latex" "begin"
 '("\\begin{" (P "Environment: " environment) "}" n> r> n>
   "\\end{" (s environment) "}"))
(zoro-tempo--define "latex" "frac" '("\\frac{" p "}{" p "}"))
(zoro-tempo--define "latex" "matrix" '((zoro-tempo--latex-matrix)))

;; Texinfo modes.
(dolist (entry
         '(("defmac" ("@defmac " p n> r> n> "@end defmac"))
           ("defun" ("@defun " p n> r> n> "@end defun"))
           ("defvar" ("@defvar " p n> r> n> "@end defvar"))
           ("example" ("@example" n> r> n> "@end example"))
           ("lisp" ("@lisp" n> r> n> "@end lisp"))
           ("bullet" ("@itemize @bullet{}" n> r> n> "@end itemize"))
           ("code" ("@code{" p "}"))
           ("var" ("@var{" p "}"))))
  (zoro-tempo--define "texinfo" (car entry) (cadr entry)))

;; Lisp modes.
(zoro-tempo--define "lisp" "lambda" '("(lambda (" p ")" n> r> ")"))

;; Eshell mode.
(dolist (entry
         '(("for" ("for " (P "Variable: ") " in " p " { " r " }"))
           ("while" ("while { " p " } { " r " }"))
           ("until" ("until { " p " } { " r " }"))
           ("if" ("if { " p " } { " r " }"))
           ("ife" ("if { " p " } { " p " } { " r " }"))
           ("unl" ("unless { " p " } { " r " }"))
           ("unle" ("unless { " p " } { " p " } { " r " }"))))
  (zoro-tempo--define "eshell" (car entry) (cadr entry)))

;; Text modes.
(zoro-tempo--define
 "text" "box"
 '((P "Text: " text noinsert)
   "┌─" (make-string (length (tempo-lookup-named 'text)) ?─) "─┐" n
   "│ " (s text) " │" n
   "└─" (make-string (length (tempo-lookup-named 'text)) ?─) "─┘"))
(zoro-tempo--define
 "text" "abox"
 '((P "Text: " text noinsert)
   "+-" (make-string (length (tempo-lookup-named 'text)) ?-) "-+" n
   "| " (s text) " |" n
   "+-" (make-string (length (tempo-lookup-named 'text)) ?-) "-+"))
(zoro-tempo--define
 "text" "cut"
 '("--8<---------------cut here---------------start------------->8---" n r n
   "--8<---------------cut here---------------end--------------->8---"))
(zoro-tempo--define
 "text" "rot13"
 '((P "Plain text: " text noinsert) (rot13 (tempo-lookup-named 'text))))
(zoro-tempo--define
 "text" "calc"
 '((P "Formula: " formula noinsert) "----" n
   (format "%s" (calc-eval (tempo-lookup-named 'formula)))))
(zoro-tempo--define "text" "table" '((zoro-tempo--text-table)))

;; reStructuredText mode.
(zoro-tempo--define
 "rst" "title"
 '((P "Title: " title noinsert)
   (make-string (length (tempo-lookup-named 'title)) ?=) n
   (s title) n
   (make-string (length (tempo-lookup-named 'title)) ?=)))

;; Java and C modes.
(zoro-tempo--define
 "java" "class"
 '("public class " (file-name-base (or buffer-file-name (buffer-name))) " {" n>
   r> n "}"))
(zoro-tempo--define
 "c" "inc"
 '("#include <" (concat (file-name-base (or buffer-file-name (buffer-name))) ".h") ">"))
(zoro-tempo--define
 "c" "incc"
 '("#include \"" (concat (file-name-base (or buffer-file-name (buffer-name))) ".h") "\""))

;; Org mode.
(dolist (entry
         '(("caption" ("#+caption: " p))
           ("drawer" (":" p ":" n r n ":end:"))
           ("quote" ("#+begin_quote" n> r> n "#+end_quote"))
           ("sidenote" ("#+begin_sidenote" n> r> n "#+end_sidenote"))
           ("marginnote" ("#+begin_marginnote" n> r> n "#+end_marginnote"))
           ("example" ("#+begin_example" n> r> n "#+end_example"))
           ("center" ("#+begin_center" n> r> n "#+end_center"))
           ("ascii" ("#+begin_export ascii" n> r> n "#+end_export"))
           ("html" ("#+begin_export html" n> r> n "#+end_export"))
           ("latex" ("#+begin_export latex" n> r> n "#+end_export"))
           ("comment" ("#+begin_comment" n> r> n "#+end_comment"))
           ("verse" ("#+begin_verse" n> r> n "#+end_verse"))
           ("src" ("#+begin_src " p n r n "#+end_src"))
           ("elisp" ("#+begin_src emacs-lisp" n r n "#+end_src"))
           ("inlsrc" ("src_" p "{" r "}"))
           ("title" ("#+title: " p n "#+author: " user-full-name n "#+language: en"))))
  (zoro-tempo--define "org" (car entry) (cadr entry)))
(zoro-tempo--define
 "org" "begin"
 '("#+begin_" (P "Block name: " block-name) n> r> n
   "#+end_" (s block-name)))
(zoro-tempo--define
 "org" "gnuplot"
 '("#+begin_src gnuplot :var data=" (P "Table: ") " :file " (P "File: " )
   n r n "#+end_src"))

(provide 'init-templates)
;;; init-templates.el ends here
