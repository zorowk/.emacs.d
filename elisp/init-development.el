;;; init-development.el --- Project development tools -*- lexical-binding: t -*-

;; Derived from M-EMACS configuration by zorowk.
;; Copyright (C) 2019 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure projects, builds, version control, and Tree-sitter.

;;; Code:

(use-package project
  :ensure nil
  :bind (:map project-prefix-map
              ("t" . project-recompile))
  :config
  (add-to-list 'project-switch-commands
               '(project-compile "Build/test") t))

(use-package compile
  :ensure nil
  :custom
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  (compilation-scroll-output 'first-error))

(use-package magit
  :ensure t
  :defer t
  :if (executable-find "git")
  :bind
  (("C-x g" . magit-status)
   (:map magit-status-mode-map
         ("M-RET" . magit-diff-visit-file-other-window)))
  :commands magit-log-buffer-file)

(declare-function treesit-language-at "treesit" (position))

(defun zoro-treesit-show-parser-used-at-point ()
  "Show the Tree-sitter parser used at point."
  (interactive)
  (if-let* ((lang (and (treesit-available-p)
                       (treesit-language-at (point)))))
      (message "%s" lang)
    (message "treesit is not available")))

(when (treesit-available-p)
  (setopt treesit-enabled-modes
          '(c-ts-mode
            c++-ts-mode
            css-ts-mode
            java-ts-mode
            js-ts-mode
            json-ts-mode
            python-ts-mode
            rust-ts-mode
            typescript-ts-mode
            yaml-ts-mode))
  (setopt treesit-auto-install-grammar 'ask)
  (setopt treesit-extra-load-path
          (list (expand-file-name "tree-sitter" user-emacs-directory))))

(provide 'init-development)
;;; init-development.el ends here
