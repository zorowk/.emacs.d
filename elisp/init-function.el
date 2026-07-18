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
