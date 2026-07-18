;;; init-templates.el --- Built-in Tempo templates -*- lexical-binding: t -*-

;;; Commentary:
;; Define mode-aware templates using Emacs's built-in Tempo library.

;;; Code:

(require 'init-function)
(require 'tempo)

(setopt tempo-interactive t)

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
