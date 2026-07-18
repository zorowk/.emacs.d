;;; init-project.el --- Project builds and error navigation -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Use the built-in project and compilation frameworks as the common entry
;; point for builds and tests.  `project-compile' runs `compile-command' from
;; the project root; `recompile' repeats it without another prompt.
;;
;; Emacs 31 already binds `project-compile' to C-x p c, `next-error' to
;; M-g n, `previous-error' to M-g p, and `recompile' to g in Compilation
;; buffers.  C-x p t repeats the last build or test from source buffers.

;;; Code:

(use-package project
  :ensure nil
  :bind (:map project-prefix-map
              ("t" . recompile))
  :config
  (add-to-list 'project-switch-commands
               '(project-compile "Build/test") t))

(use-package compile
  :ensure nil
  :custom
  ;; Replace a running build without asking and save source buffers first.
  (compilation-always-kill t)
  (compilation-ask-about-save nil)
  ;; Follow output until the first error, then leave navigation to M-g n/p.
  (compilation-scroll-output 'first-error))

(provide 'init-project)
;;; init-project.el ends here
