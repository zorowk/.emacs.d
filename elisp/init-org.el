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
  :ensure nil
  :defer t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         (:map org-mode-map (("C-c C-p" . eaf-org-export-to-pdf-and-open)
                             ("C-c ;" . nil))))
  :custom
  (org-log-done 'time)
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-latex-listings-options '(("breaklines" "true")))
  (org-latex-listings t)
  (org-deadline-warning-days 7)
  (org-agenda-window-setup 'other-window)
  :config
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (unless (version< org-version "9.2")
    (require 'org-tempo))

  (with-eval-after-load 'org
    (progn
      ;; Allow multiple line Org emphasis markup.
      ;; http://emacs.stackexchange.com/a/13828/115
      (setcar (nthcdr 4 org-emphasis-regexp-components) 20) ;Up to 20 lines, default is just
      ;; Below is needed to apply the modified `org-emphasis-regexp-components'
      ;; settings from above.
      (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

      (require 'org-compat)
      (add-to-list 'org-modules 'org-habit)
      (require 'org-habit)

      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-targets
            '((nil :maxlevel . 4)
              (org-agenda-files :maxlevel . 4)))

      ;; config stuck project
      (setq org-stuck-projects
            '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

      (setq org-agenda-inhibit-startup t) ;; ~50x speedup
      (setq org-agenda-span 'day)
      (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
      (setq org-agenda-window-setup 'current-window)
      (setq org-log-done t)
      (setq org-startup-indented t)
      (setq org-html-validation-link nil)

      (require 'cal-china)
      ;; diary for chinese birthday
      ;; https://emacs-china.org/t/topic/2119/14
      (defun my--diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
        (if year
            (let* ((d-date (diary-make-date lunar-month lunar-day year))
                   (a-date (calendar-absolute-from-gregorian d-date))
                   (c-date (calendar-chinese-from-absolute a-date))
                   (date a-date)
                   (cycle (car c-date))
                   (yy (cadr c-date))
                   (y (+ (* 100 cycle) yy)))
              (diary-chinese-anniversary lunar-month lunar-day y mark))
          (diary-chinese-anniversary lunar-month lunar-day year mark)))

      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                    (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))

      ;; Change task state to STARTED when clocking in
      (setq org-clock-in-switch-to-state "STARTED")
      ;; Save clock data and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      (setq org-log-into-drawer t)
      ;; Removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line
      (setq org-tags-match-list-sublevels nil)

      (require 'ox-publish)
      (setq org-latex-listings t)
      (add-to-list 'org-latex-packages-alist
                   '("AUTO" "inputenc" t))

      (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[10pt]{ctexart}
                                        \\usepackage[slantfont, boldfont]{xeCJK}
                                        [NO-DEFAULT-PACKAGES]
                                        [PACKAGES]
                                        \\setCJKmainfont{Noto Sans CJK SC}
                                        \\parindent 2em
                                        \\setmainfont{Input}
                                        \\setsansfont{Input}
                                        \\setmonofont{Source Code Pro}
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\usepackage{color}
                                        \\usepackage{lstautogobble}
                                        \\usepackage{zi4}
                                        \\definecolor{bluekeywords}{rgb}{0.13, 0.13, 1}
                                        \\definecolor{greencomments}{rgb}{0, 0.5, 0}
                                        \\definecolor{redstrings}{rgb}{0.9, 0, 0}
                                        \\definecolor{graynumbers}{rgb}{0.5, 0.5, 0.5}
                                        \\defaultfontfeatures{Mapping=tex-text}
                                        \\XeTeXlinebreaklocale \"zh\"
                                        \\XeTeXlinebreakskip = 0pt plus 1pt minus 0.1pt
                                        \\lstset{autogobble,
                                                 columns=fullflexible,
                                                 showspaces=false,
                                                 showtabs=false,
                                                 breaklines=true,
                                                 showstringspaces=false,
                                                 breakatwhitespace=true,
                                                 escapeinside={(*@}{@*)},
                                                 commentstyle=\\color{greencomments},
                                                 keywordstyle=\\color{bluekeywords},
                                                 stringstyle=\\color{redstrings},
                                                 numberstyle=\\color{graynumbers},
                                                 basicstyle=\\ttfamily\\footnotesize,
                                                 frame=l,
                                                 framesep=12pt,
                                                 xleftmargin=12pt,
                                                 tabsize=4,
                                                 captionpos=b}
                                        [EXTRA]"
                                        ("\\section{%s}" . "\\section*{%s}")
                                        ("\\subsection{%s}" . "\\subsection*{%s}")
                                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (setq org-latex-default-class "ctexart")
      (setq org-latex-pdf-process
            '("xelatex -interaction nonstopmode -shell-escape -output-directory %o %f"
	          "bibtex %b"
              "makeindex %b"
	          "xelatex -interaction nonstopmode --shell-escape -output-directory %o %f"
	          "xelatex -interaction nonstopmode -shell-escape -output-directory %o %f"
              "rm -rf %b.out %b.log %b.tex %b.bbl auto"))
      (setq org-latex-listings t)

      ;;reset subtask
      (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

      (setq org-startup-with-inline-images t)
      ;;(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

      (setq org-plantuml-jar-path
            (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))
      (setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((perl . t)
         (ruby . t)
         (dot . t)
         (js . t)
         (latex .t)
         (python . t)
         (emacs-lisp . t)
         (plantuml . t)
         (C . t)
         (ditaa . t)
         (gnuplot . t)))

      (setq org-file-apps
            '((auto-mode . emacs)
              ("\\.x?html?\\'" . "firefxo %s")
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

      ;;An entry without a cookie is treated just like priority ' B '.
      ;;So when create new task, they are default 重要且紧急
      (setq org-agenda-custom-commands
            '(
              ("w" . "Task Schedule")
              ("wa" "Important and urgent tasks" tags-todo "+PRIORITY=\"A\"")
              ("wb" "Important and not urgent tasks" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
              ("wc" "Not important and urgent tasks" tags-todo "+PRIORITY=\"C\"")
              ("b" "Blog" tags-todo "BLOG")
              ("p" . "Project")
              ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"Deepin\"")
              ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zorowk\"")
              ("W" "Weekly Review"
               ((stuck "") ;; review stuck projects as designated by org-stuck-projects
                (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
                ))))

      (setq org-confirm-babel-evaluate nil
            org-src-fontify-natively t
            org-src-tab-acts-natively t)

      (setq spaceline-org-clock-p t)

      (setq bibtex-completion-bibliography "~/Dropbox/bibliography/references.bib"
            bibtex-completion-library-path "~/Dropbox/bibliography/book"
            bibtex-completion-notes-path "~/Dropbox/bibliography/bibnotes.org")))

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

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
