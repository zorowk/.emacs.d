;;; check-startup.el --- Assertions for an already loaded init -*- lexical-binding: t; -*-

;;; Commentary:
;; Load this after init.el in a fresh batch Emacs.  It checks compatibility and
;; synchronous startup ownership without forcing deferred packages to load.

;;; Code:

(require 'seq)

(unless (version<= "31.0.90" emacs-version)
  (error "Emacs %s is older than the required 31.0.90 baseline" emacs-version))

(dolist (feature '(init-const init-package init-ui init-core init-files
                   init-development init-search init-edit init-shell init-dired
                   init-buffer init-theme init-dashboard init-complete
                   init-templates init-gnuplot init-latex init-org init-hyperbole
                   init-reader init-erc init-llm init-gnus))
  (unless (featurep feature)
    (error "Startup did not provide %S" feature)))

(let* ((deferred '(org denote hyperbole agent-shell gnus erc))
       (loaded (seq-filter #'featurep deferred)))
  (when loaded
    (error "Deferred features loaded synchronously: %S" loaded)))

(when package-install-upgrade-built-in
  (error "Built-in packages may be replaced by archive versions"))

(princ (format "Startup assertions passed on Emacs %s.\n" emacs-version))

;;; check-startup.el ends here
