;;; init-llm.el --- Agent Shell configuration -*- lexical-binding: t -*-

;; Author: zorowk
;; Copyright (C) 2019 zorowk
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
  ( agent-shell-agent-configs
    '(agent-shell-openai-make-codex-config))
  ( agent-shell-session-restore-verbosity 'full)
  ( agent-shell-inhibit-system-sleep nil))

(provide 'init-llm)
;;; init-llm.el ends here
