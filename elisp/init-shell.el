;;; init-shell.el --- Login-shell environment -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Import environment variables asynchronously for graphical sessions on Linux and macOS.

;;; Code:

(defconst zoro-shell-environment-variables
  '("PATH" "MANPATH" "LANG" "LC_ALL")
  "Environment variables imported from the login shell.")

(defconst zoro-shell-environment-marker "\0ZORO-ENV-BEGIN\0"
  "Delimiter separating shell startup messages from environment output.")

(defun zoro--finish-shell-environment-import (process _event)
  "Apply the environment emitted by PROCESS when it exits successfully."
  (when (memq (process-status process) '(exit signal))
    (let ((output (process-buffer process))
          (errors (process-get process 'stderr-buffer)))
      (unwind-protect
          (if (zerop (process-exit-status process))
              (let ((environment
                     (with-current-buffer output
                       (goto-char (point-min))
                       (when (search-forward zoro-shell-environment-marker nil t)
                         (let ((process-environment
                                (split-string
                                 (buffer-substring-no-properties (point) (point-max))
                                 "\0" t)))
                           (mapcar (lambda (name) (cons name (getenv name)))
                                   zoro-shell-environment-variables))))))
                (if-let* ((path (cdr (assoc-string "PATH" environment)))
                          ((not (string-empty-p path))))
                    (progn
                      (dolist (entry environment)
                        (when (cdr entry)
                          (setenv (car entry) (cdr entry))))
                      (setq exec-path
                            (append (parse-colon-path path) (list exec-directory)))
                      (set-default 'eshell-path-env path))
                  (message "Login shell returned no valid PATH; keeping current environment")))
            (message "Login shell environment import failed: %s"
                     (with-current-buffer errors
                       (string-trim (buffer-string)))))
        (kill-buffer output)
        (kill-buffer errors)))))

(defun zoro-import-shell-environment ()
  "Import selected variables from an interactive login shell asynchronously."
  (interactive)
  (let ((output (generate-new-buffer " *login-shell-environment*"))
        (errors (generate-new-buffer " *login-shell-errors*")))
    (condition-case err
        (let ((process
               (make-process
                :name "login-shell-environment"
                :buffer output
                :command (list (or shell-file-name (getenv "SHELL") "/bin/sh")
                               "-l" "-i" "-c"
                               "printf '\\000ZORO-ENV-BEGIN\\000'; exec /usr/bin/env -0")
                :coding 'utf-8-unix
                :connection-type 'pipe
                :noquery t
                :sentinel #'zoro--finish-shell-environment-import
                :stderr errors)))
          (process-put process 'stderr-buffer errors)
          process)
      (error
       (kill-buffer output)
       (kill-buffer errors)
       (message "Could not start login shell: %s" (error-message-string err))))))

(provide 'init-shell)
;;; init-shell.el ends here
