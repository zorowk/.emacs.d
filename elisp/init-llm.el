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

;; Org is the task/implementation record; agent-shell is the execution view.
(let* ((skills-root (expand-file-name "skills/" (or (getenv "CODEX_HOME") "~/.codex")))
       (integration (expand-file-name
                     "emacs-gtd-assistant/scripts/agent-shell-org-tasks.el" skills-root)))
  (if (file-readable-p integration)
      (progn
        (load integration nil t)
        (setq emacs-gtd-directory zoro-org-directory
              emacs-gtd-extra-files (org-agenda-files)
              emacs-gtd-agent-storage-directory
              (expand-file-name ".agent-shell/" zoro-dropbox-directory))
        (emacs-gtd-agent-enable))
    (display-warning 'init-llm "Org/agent-shell integration file is missing" :warning)))

(with-eval-after-load 'agent-shell
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
