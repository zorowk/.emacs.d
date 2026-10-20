;;; init-llm.el --- Agent Shell configuration -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure Agent Shell for Codex sessions.

;;; Code:

(use-package agent-shell
  :ensure t
  :defer t
  :bind (("C-z a" . agent-shell))
  :custom
  (agent-shell-header-style 'text)
  (agent-shell-agent-configs
   (append '(agent-shell-openai-make-codex-config)
           (when (executable-find "omp")
             '(agent-shell-omp-make-agent-config))))
  (agent-shell-session-restore-verbosity 'full)
  (agent-shell-busy-indicator-frames "")
  (agent-shell-inhibit-system-sleep nil))

(defvar emacs-gtd-directory)
(defvar emacs-gtd-extra-files)
(defvar emacs-gtd-agent-storage-directory)
(declare-function emacs-gtd-agent-enable "agent-shell-org-tasks" ())

(defvar zoro-agent-task-integration-loading nil
  "Non-nil while loading the optional Org/Agent Shell integration.")

(defun zoro-enable-agent-task-integration ()
  "Load task integration on first use of Org, Agenda or Agent Shell."
  (unless (or zoro-agent-task-integration-loading
              (featurep 'agent-shell-org-tasks))
    (let* ((zoro-agent-task-integration-loading t)
           (skills-root
            (expand-file-name "skills/" (or (getenv "CODEX_HOME") "~/.codex")))
           (integration
            (expand-file-name
             "emacs-gtd-assistant/scripts/agent-shell-org-tasks.el" skills-root)))
      (when (file-readable-p integration)
        (load integration nil t)
        (setq emacs-gtd-directory zoro-org-directory
              emacs-gtd-extra-files (org-agenda-files)
              emacs-gtd-agent-storage-directory
              (expand-file-name ".agent-shell/" zoro-dropbox-directory))
        (emacs-gtd-agent-enable)))))

;; Org is the task record; load its integration only when a relevant UI is used.
(add-hook 'org-mode-hook #'zoro-enable-agent-task-integration)
(with-eval-after-load 'org-agenda
  (zoro-enable-agent-task-integration))

(with-eval-after-load 'agent-shell
  (zoro-enable-agent-task-integration)
  (let ((skills-root
         (expand-file-name "skills/"
                           (or (getenv "CODEX_HOME") "~/.codex"))))
    (dolist (extension
             '(("emacs-code-navigator/scripts/agent-shell-code-context.el"
                . emacs-code-navigator-agent-shell-enable)))
      (when (load (expand-file-name (car extension) skills-root) t t)
        (funcall (cdr extension))))))

(provide 'init-llm)
;;; init-llm.el ends here
