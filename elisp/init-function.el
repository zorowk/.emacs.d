;;; init-function.el --- Shared configuration functions -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Keep named configuration functions in one place.  Feature modules retain
;; declarations, hooks, key bindings, and package-specific settings.

;;; Code:

(require 'init-const)

;; Startup lifecycle and deferred work.

(defvar zoro-startup-idle-timers (make-hash-table :test #'eq)
  "Pending startup idle timers keyed by task name.")

(defvar zoro-startup-idle-task-history nil
  "Completed startup idle tasks, newest first.")

(defun zoro-restore-startup-state ()
  "Restore GC and file handling after loading the init file."
  (setq gc-cons-threshold (* 16 1024 1024)
        gc-cons-percentage 0.1)
  (when (boundp 'file-name-handler-alist-original)
    (setq file-name-handler-alist file-name-handler-alist-original)
    (makunbound 'file-name-handler-alist-original)))

(defun zoro-startup--run-idle-task (name function arguments)
  "Run startup idle task NAME by applying FUNCTION to ARGUMENTS once."
  (remhash name zoro-startup-idle-timers)
  (let ((started-at (current-time)))
    (condition-case error-data
        (progn
          (apply function arguments)
          (push (list :name name
                      :status 'ok
                      :elapsed-ms
                      (* 1000.0
                         (float-time (time-subtract (current-time) started-at))))
                zoro-startup-idle-task-history))
      (error
       (push (list :name name
                   :status 'error
                   :error error-data
                   :elapsed-ms
                   (* 1000.0
                      (float-time (time-subtract (current-time) started-at))))
             zoro-startup-idle-task-history)
       (message "Startup idle task %s failed: %s"
                name (error-message-string error-data))))))

(defun zoro-startup-cancel-idle-tasks ()
  "Cancel every pending startup idle task."
  (maphash (lambda (_name timer)
             (when (timerp timer)
               (cancel-timer timer)))
           zoro-startup-idle-timers)
  (clrhash zoro-startup-idle-timers))

(defun zoro-startup-schedule-idle-tasks (&optional force)
  "Schedule `zoro-startup-idle-tasks' after initialization.

Cancel pending tasks first so evaluating the configuration again does not
register duplicates.  In batch sessions, only schedule tasks when FORCE is
non-nil."
  (zoro-startup-cancel-idle-tasks)
  (setq zoro-startup-idle-task-history nil)
  (when (or force (not noninteractive))
    (dolist (task zoro-startup-idle-tasks)
      (let ((name (plist-get task :name))
            (delay (plist-get task :delay))
            (function (plist-get task :function))
            (arguments (plist-get task :arguments))
            (predicate (plist-get task :predicate)))
        (when (or (null predicate) (funcall predicate))
          (puthash name
                   (run-with-idle-timer
                    delay nil #'zoro-startup--run-idle-task
                    name function arguments)
                   zoro-startup-idle-timers))))))

(defun zoro-startup-idle-task-report ()
  "Display pending and completed startup idle tasks."
  (interactive)
  (with-help-window "*Startup Idle Tasks*"
    (princ "Pending\n-------\n")
    (if (zerop (hash-table-count zoro-startup-idle-timers))
        (princ "None\n")
      (dolist (task zoro-startup-idle-tasks)
        (let ((name (plist-get task :name)))
          (when (gethash name zoro-startup-idle-timers)
            (princ (format "%-16s %5.2fs  %s\n"
                           name
                           (plist-get task :delay)
                           (if-let* ((arguments
                                      (plist-get task :arguments)))
                               (cons (plist-get task :function) arguments)
                             (plist-get task :function))))))))
    (princ "\nCompleted\n---------\n")
    (if zoro-startup-idle-task-history
        (dolist (result (reverse zoro-startup-idle-task-history))
          (princ (format "%-16s %-5s %8.2fms%s\n"
                         (plist-get result :name)
                         (plist-get result :status)
                         (plist-get result :elapsed-ms)
                         (if-let* ((error-data (plist-get result :error)))
                             (format "  %s" (error-message-string error-data))
                           ""))))
      (princ "None\n"))))

(defun zoro-gc-when-unfocused ()
  "Collect garbage after the last frame loses focus."
  (unless (frame-focus-state)
    (garbage-collect)))

(defun zoro-install-focus-gc ()
  "Install garbage collection after frame focus changes."
  (add-function :after after-focus-change-function #'zoro-gc-when-unfocused))

;; Core editing helpers.

(defun abort-minibuffer-using-mouse ()
  "Abort an active minibuffer when the mouse leaves its buffer."
  (when (and (>= (recursion-depth) 1)
             (active-minibuffer-window))
    (abort-recursive-edit)))

(defun where-am-i ()
  "Show and copy `buffer-file-name' or `buffer-name'."
  (interactive)
  (message (kill-new (or buffer-file-name (buffer-name)))))

;; Fonts and frames.

(defun zoro-apply-theme (appearance)
  "Load the theme matching system APPEARANCE."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'ef-frost t))
    ('dark (load-theme 'ef-autumn t)))
  (when (featurep 'dashboard)
    (zoro-dashboard-update-banner appearance)))

(defun change-font ()
  "Apply the configured fixed, variable, symbol, emoji, and CJK fonts."
  (let ((font-height (if (eq system-type 'darwin) 150 110)))
    (set-face-attribute 'default nil
                        :family "JetBrains Mono"
                        :height font-height))
  (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 1.0)
  (if (eq system-type 'darwin)
      (set-face-attribute 'variable-pitch nil :family "Georgia" :height 1.0)
    (set-face-attribute 'variable-pitch nil :family "Gelasio" :height 1.0))
  (if (eq system-type 'darwin)
      (progn
        (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji"))
        (set-fontset-font t 'symbol (font-spec :family "STIX Two Math"))
        (set-fontset-font t 'greek (font-spec :family "Apple Symbols"))
        (set-fontset-font t 'hangul (font-spec :family "Apple SD Gothic Neo"))
        (set-fontset-font t 'kana (font-spec :family "Hiragino Maru Gothic ProN"))
        (set-fontset-font t 'cjk-misc (font-spec :family "PingFang SC"))
        (set-fontset-font t 'bopomofo (font-spec :family "PingFang SC"))
        (set-fontset-font t 'han (font-spec :family "PingFang SC")))
    (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji"))
    (set-fontset-font t 'symbol (font-spec :family "Noto Sans Math"))
    (set-fontset-font t 'greek (font-spec :family "Noto Sans Symbols"))
    (set-fontset-font t 'hangul (font-spec :family "Noto Sans CJK KR"))
    (set-fontset-font t 'kana (font-spec :family "Noto Sans CJK JP"))
    (set-fontset-font t 'han (font-spec :family "Noto Sans CJK SC"))))

(defun zoro-apply-font-to-frame (frame)
  "Apply configured fonts to graphical FRAME."
  (with-selected-frame frame
    (when (display-graphic-p)
      (change-font))))

(defun setup-frame-alpha (&optional frame)
  "Apply transparency and blur to FRAME."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (set-frame-parameter nil 'ns-alpha-elements
                           '(ns-alpha-default ns-alpha-fringe ns-alpha-glyphs))
      (set-frame-parameter nil 'alpha-background 0.95)
      (set-frame-parameter nil 'ns-background-blur 25))))

;; Deferred UI features.

(defun zoro-enable-popper ()
  "Enable Popper and its echo mode."
  (popper-mode 1)
  (popper-echo-mode 1))

(defun zoro-initial-dashboard-buffer ()
  "Return the lightweight initial Dashboard buffer."
  (get-buffer-create "*dashboard*"))

(defun zoro-dashboard-update-banner (appearance)
  "Set and refresh the Dashboard banner for APPEARANCE."
  (setq dashboard-startup-banner
        (expand-file-name
         (if (eq appearance 'dark)
             "images/KEC_Dark_BK_Small.png"
           "images/KEC_Light_BK_Small.png")
         user-emacs-directory))
  (when-let* ((buffer-name (and (boundp 'dashboard-buffer-name)
                                dashboard-buffer-name))
              (buffer (get-buffer buffer-name)))
    (with-current-buffer buffer
      (dashboard-insert-startupify-lists t))))

(defun zoro-dashboard-enable-agenda ()
  "Add the Agenda widget and refresh an existing Dashboard buffer."
  (require 'dashboard)
  (setq dashboard-items '((recents . 7)
                          (bookmarks . 7)
                          (agenda . 5)))
  (when-let* ((buffer (get-buffer dashboard-buffer-name)))
    (with-current-buffer buffer
      (dashboard-insert-startupify-lists t))))

(defun open-dashboard ()
  "Open the Dashboard buffer and jump to the first widget."
  (interactive)
  (require 'dashboard)
  (dashboard-insert-startupify-lists)
  (switch-to-buffer dashboard-buffer-name)
  (goto-char (point-min))
  (delete-other-windows))

(defun zoro-dashboard-browse-homepage (&rest _)
  "Open the configured homepage."
  (browse-url "https://zorowk.github.io/"))

(defun zoro-dashboard-find-config (&rest _)
  "Find a file in the Emacs configuration project."
  (let ((default-directory user-emacs-directory))
    (project-find-file)))

(defun zoro-dashboard-open-info (&rest _)
  "Open the Emacs Info reader."
  (info))

;; Org helpers.

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

(defun org-export-toggle-syntax-highlight ()
  "Use minted syntax highlighting for the current Org export buffer."
  (interactive)
  (setq-local org-latex-src-block-backend 'minted)
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

(defun org-table-insert-vertical-hline ()
  "Insert a LaTeX table alignment attribute with vertical rules."
  (interactive)
  (insert "#+attr_latex: :align |c|c|c|"))

;; Tempo helpers.

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

;; Tree-sitter helpers.

(defun treesit-show-parser-used-at-point ()
  "Show the Tree-sitter parser used at point."
  (interactive)
  (if-let* ((lang (and (treesit-available-p)
                       (treesit-language-at (point)))))
      (message "%s" lang)
    (message "treesit is not available")))

;; Package-specific commands and callbacks.

(defun magit-log-follow-current-file ()
  "Call `magit-log-buffer-file' with history following enabled."
  (interactive)
  (magit-log-buffer-file t))

(defun erc-notify (nickname message)
  "Display an ERC notification from NICKNAME containing MESSAGE."
  (let* ((channel (buffer-name))
         (title (if (string-match-p (concat "^" nickname) channel)
                    nickname
                  (concat nickname " (" channel ")")))
         (text (string-trim
                (replace-regexp-in-string "[[:space:]\n]+" " " message))))
    (if (fboundp 'notifications-notify)
        (notifications-notify :title title
                              :body (concat nickname ": " text))
      (message "%s: %s" title text))))

(provide 'init-function)
;;; init-function.el ends here
