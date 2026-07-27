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
  (agent-shell-markdown-render-function #'agent-shell-markdown-replace-markup)
  (agent-shell-highlight-blocks t)
  (agent-shell-show-session-id nil)
  (agent-shell-header-style 'text)
  (agent-shell-agent-configs
   (append '(agent-shell-openai-make-codex-config)
           (when (executable-find "omp")
             '(agent-shell-omp-make-agent-config))))
  ( agent-shell-session-restore-verbosity 'full)
  ( agent-shell-inhibit-system-sleep nil))

(with-eval-after-load 'agent-shell
  (let ((skills-root
         (expand-file-name "skills/"
                           (or (getenv "CODEX_HOME") "~/.codex"))))
    (dolist (extension
             '(("emacs-code-navigator/scripts/agent-shell-code-context.el"
                . emacs-code-navigator-agent-shell-enable)
               ("git-commit/scripts/agent-shell-git-review.el"
                . agent-shell-git-review-enable)
               ("emacs-gtd-assistant/scripts/agent-shell-gtd-capture.el"
                . agent-shell-gtd-capture-enable)
               ("denote-scribe/scripts/agent-shell-denote-capture.el"
                . agent-shell-denote-capture-enable)
               ("skill-usage-review/scripts/agent-shell-skill-usage-review.el"
                . agent-shell-skill-usage-review-enable)))
      (when (load (expand-file-name (car extension) skills-root) t t)
        (funcall (cdr extension))))))

(provide 'init-llm)
;;; init-llm.el ends here
