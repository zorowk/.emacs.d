;;; init-shell.el --- Login-shell environment -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Import environment variables asynchronously for graphical macOS sessions.

;;; Code:

(defconst zoro-shell-environment-variables
  '("PATH" "MANPATH" "LANG" "LC_ALL")
  "Environment variables imported from the login shell.")

(defun zoro--finish-shell-environment-import (process _event)
  "Apply the environment emitted by PROCESS when it exits successfully."
  (when (memq (process-status process) '(exit signal))
    (let ((output (process-buffer process))
          (errors (process-get process 'stderr-buffer)))
      (unwind-protect
          (if (zerop (process-exit-status process))
              (let ((environment (make-hash-table :test #'equal)))
                (with-current-buffer output
                  (dolist (entry (split-string (buffer-string) "\0" t))
                    (when-let* ((separator (string-search "=" entry))
                                (name (substring entry 0 separator))
                                ((member name zoro-shell-environment-variables)))
                      (puthash name (substring entry (1+ separator))
                               environment))))
                (dolist (name zoro-shell-environment-variables)
                  (let ((value (gethash name environment)))
                    (setenv name value)
                    (when (string= name "PATH")
                      (setq exec-path
                            (append (parse-colon-path value)
                                    (list exec-directory)))
                      (set-default 'eshell-path-env value)))))
            (message "Login shell environment import failed: %s"
                     (with-current-buffer errors
                       (string-trim (buffer-string)))))
        (kill-buffer output)
        (kill-buffer errors)))))

(defun zoro-import-shell-environment ()
  "Import selected variables from an interactive login shell asynchronously."
  (let ((output (generate-new-buffer " *login-shell-environment*"))
        (errors (generate-new-buffer " *login-shell-errors*")))
    (condition-case err
        (let ((process
               (make-process
                :name "login-shell-environment"
                :buffer output
                :command (list (or shell-file-name (getenv "SHELL") "/bin/sh")
                               "-l" "-i" "-c" "exec /usr/bin/env -0")
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
