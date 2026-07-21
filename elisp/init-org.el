;;; init-org.el --- Org and Denote workflows -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure Org workflows, export, capture, Babel, and Denote notes.

;;; Code:

(require 'init-const)

(defvar org-latex-packages-alist)

(defun zoro-org-decide-line-range (file begin end)
  "Return an Org :lines range in FILE delimited by BEGIN and END.

BEGIN and END are optional regexps.  The matching delimiter lines are
excluded: Org treats the first line as inclusive and the second as exclusive.
An omitted delimiter produces an open range such as `-20' or `10-'."
  (let ((first-line "")
        (last-line ""))
    (save-match-data
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when begin
          (re-search-forward begin)
          (setq first-line
                (1+ (line-number-at-pos (match-beginning 0)))))
        (when end
          (re-search-forward end)
          (setq last-line
                (line-number-at-pos (match-beginning 0))))
        (format "%s-%s" first-line last-line)))))

(defun zoro-org-update-include-ranges ()
  "Update :lines on #+INCLUDE directives carrying :range-* markers.

The nonstandard :range-begin and :range-end parameters contain regexps matched
against the included file.  They are converted to the numeric range understood
by Org before the current buffer is saved."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^\\s-*#\\+INCLUDE: *\"\\([^\"]+\\)\".*:range-\\(begin\\|end\\)"
            nil t)
      (let* ((file (expand-file-name (match-string-no-properties 1)))
             lines begin end)
        (forward-line 0)
        (when (looking-at "^.*:range-begin *\"\\([^\"]+\\)\"")
          (setq begin (match-string-no-properties 1)))
        (when (looking-at "^.*:range-end *\"\\([^\"]+\\)\"")
          (setq end (match-string-no-properties 1)))
        (setq lines (zoro-org-decide-line-range file begin end))
        (when lines
          (if (looking-at ".*:lines *\"\\([-0-9]+\\)\"")
              (replace-match lines :fixedcase :literal nil 1)
            (goto-char (line-end-position))
            (insert " :lines \"" lines "\"")))))))

(defun zoro-org-enable-include-range-updates ()
  "Update marked Org INCLUDE ranges whenever this buffer is saved."
  (add-hook 'before-save-hook #'zoro-org-update-include-ranges nil t))

(defun zoro-org-export-toggle-syntax-highlight ()
  "Use minted syntax highlighting for the current Org export buffer."
  (interactive)
  (setq-local org-latex-src-block-backend 'minted)
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

(defun zoro-org-table-insert-vertical-hline ()
  "Insert a LaTeX table alignment attribute with vertical rules."
  (interactive)
  (insert "#+attr_latex: :align |c|c|c|"))

(use-package org
  :ensure nil
  :defer t
  :bind (("C-c C-l" . org-store-link)
         ("C-c C-i" . org-insert-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook (org-mode . zoro-org-enable-include-range-updates)
  :custom
  (org-log-done 'time)
  (calendar-latitude 30.5928) ;; Used by `sunrise-sunset' and `org-agenda'.
  (calendar-longitude 114.3052)
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate t)
  (org-latex-listings-options '(("breaklines" "true")))
  (org-latex-src-block-backend 'listings)
  (org-hide-leading-stars t)
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)
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
  (org-preview-latex-default-process 'xelatex)
  :custom-face
  (org-agenda-current-time ((t (:foreground "spring green"))))
  :config
  (add-to-list 'org-latex-packages-alist '("" "listings"))
  (require 'org-tempo)

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
     (latex . t)
     (python . t)
     (R . t)
     (emacs-lisp . t)
     (maxima . t)
     (gnuplot . t)))

  ;; define the refile targets
  (setq org-agenda-dir zoro-org-directory)
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
           :empty-lines 1))))

(use-package denote
  :ensure t
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
  (setq denote-directory zoro-denote-directory)
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
  :ensure t
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :config
  (consult-denote-mode 1))

(use-package org-bullets
  :ensure t
  :after org
  :init
  (setq org-bullets-bullet-list '("⚙" "※" "✥" "⚛" "§" "☩" "♅" "⚔"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Keep Org tables monospaced, especially when prose uses a variable-pitch face.
(with-eval-after-load 'org
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch))

(provide 'init-org)
;;; init-org.el ends here
