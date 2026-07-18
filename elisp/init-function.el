;;; init-function.el --- Shared configuration functions -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Keep only callbacks shared across feature modules, multi-step package
;; operations, and user commands without a direct Emacs or package equivalent.
;; Feature modules retain declarations, hooks, bindings, and ordinary settings.
;; Prefer calling public APIs directly at those use sites when no policy is
;; added here.

;;; Code:

(require 'init-const)

;; These packages remain deferred.  The declarations provide byte-compiler
;; context without loading them as a side effect of this shared module.
(declare-function tempo-build-collection "tempo")
(declare-function tempo-complete-tag "tempo")
(declare-function tempo-define-template "tempo")
(declare-function tempo-insert-template "tempo")
(declare-function tempo-use-tag-list "tempo")
(declare-function treesit-language-at "treesit" (position))

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
  "Install garbage collection after frame focus changes.

`after-focus-change-function' is an abnormal function variable rather than a
normal hook, so attach the callback with `add-function'."
  (add-function :after after-focus-change-function #'zoro-gc-when-unfocused))

;; Editing commands without direct built-in equivalents.

(defun zoro-abort-minibuffer-using-mouse ()
  "Abort an active minibuffer when the mouse leaves its buffer."
  (when (and (>= (recursion-depth) 1)
             (active-minibuffer-window))
    (abort-recursive-edit)))

(defun zoro-where-am-i ()
  "Show and copy `buffer-file-name' or `buffer-name'."
  (interactive)
  (message (kill-new (or buffer-file-name (buffer-name)))))

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

;; Tree-sitter has a language query API but no matching interactive command.

(defun zoro-treesit-show-parser-used-at-point ()
  "Show the Tree-sitter parser used at point."
  (interactive)
  (if-let* ((lang (and (treesit-available-p)
                       (treesit-language-at (point)))))
      (message "%s" lang)
    (message "treesit is not available")))

(provide 'init-function)
;;; init-function.el ends here
