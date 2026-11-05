;;; integration-tests.el --- Package and configuration integration tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify the ownership boundaries between completion packages and audit the
;; package declarations against the packages activated by the full init.

;;; Code:

(require 'test-helper)

(ert-deftest zoro-integration-all-init-modules-load ()
  (dolist (feature '(init-const init-package init-ui init-core init-files
                     init-development init-search init-edit init-shell
                     init-dired init-buffer init-theme init-dashboard
                     init-complete init-templates init-gnuplot init-latex init-org init-hyperbole
                     init-reader init-erc init-llm init-gnus))
    (should (featurep feature))))

(ert-deftest zoro-integration-package-declarations-have-explicit-origin ()
  (dolist (form (zoro-test-use-package-forms))
    (should (or (memq :ensure (cddr form))
                (memq :vc (cddr form))))))

(ert-deftest zoro-integration-third-party-packages-are-installed ()
  (dolist (form (zoro-test-use-package-forms))
    (when (or (eq (plist-get (cddr form) :ensure) t)
              (memq :vc (cddr form)))
      (should (package-installed-p (cadr form))))))

(ert-deftest zoro-integration-builtins-are-not-replaced-by-archives ()
  (should-not package-install-upgrade-built-in)
  (dolist (form (zoro-test-use-package-forms))
    (when (and (null (plist-get (cddr form) :ensure))
               (not (memq :vc (cddr form))))
      (let ((package (cadr form)))
        (should (zoro-test-bundled-library-p package))))))

(ert-deftest zoro-integration-crux-is-pinned-to-melpa ()
  (should (equal (alist-get 'crux package-pinned-packages) "melpa")))

(ert-deftest zoro-integration-completion-roles-are-explicit ()
  (should (equal completion-styles '(orderless basic)))
  (should (equal (alist-get 'file completion-category-overrides)
                 '((styles partial-completion))))
  (should (equal (alist-get 'eglot-capf completion-category-overrides)
                 '((styles orderless basic))))
  (should (eq tab-always-indent 'complete))
  (should-not text-mode-ispell-word-completion))

(ert-deftest zoro-integration-corfu-yields-vertico-minibuffers ()
  (require 'corfu)
  (should (memq 'corfu-history savehist-additional-variables))
  (should-not corfu-preview-current)
  (should (equal corfu-popupinfo-delay '(1.0 . 0.5)))
  (should (functionp global-corfu-minibuffer))
  (let ((vertico--input t))
    (should-not (funcall global-corfu-minibuffer)))
  (let ((vertico--input nil)
        (mct--active nil))
    (should (funcall global-corfu-minibuffer))))

(ert-deftest zoro-integration-eglot-can-fall-through-to-cape ()
  (require 'eglot)
  (require 'cape)
  (should (memq #'cape-file
                (default-value 'completion-at-point-functions)))
  (should (advice-member-p #'cape-wrap-nonexclusive
                           #'eglot-completion-at-point)))

(ert-deftest zoro-integration-has-one-popup-completion-frontend ()
  (should-not (featurep 'company))
  (should-not (featurep 'auto-complete))
  (should-not (bound-and-true-p fido-mode))
  (should-not (bound-and-true-p fido-vertical-mode))
  (should-not (bound-and-true-p icomplete-mode)))

(ert-deftest zoro-integration-eglot-preserves-global-dabbrev-binding ()
  (require 'eglot)
  (should (eq (lookup-key global-map (kbd "M-/")) #'dabbrev-completion))
  (should-not (lookup-key eglot-mode-map (kbd "M-/")))
  (should (eq (lookup-key eglot-mode-map (kbd "C-c l i"))
              #'eglot-find-implementation)))

(ert-deftest zoro-integration-denote-search-key-has-single-declaration ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "elisp/init-org.el" zoro-test-root))
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "\"C-c n g\"" nil t)
        (setq count (1+ count)))
      (should (= count 1)))))

(ert-deftest zoro-integration-crux-avoids-legacy-advice-and-duplicate-key ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "elisp/init-edit.el" zoro-test-root))
    (goto-char (point-min))
    (should-not (search-forward "crux-with-region-" nil t))
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "crux-eval-and-replace" nil t)
        (setq count (1+ count)))
      (should (= count 1)))))

(ert-deftest zoro-integration-crux-uses-package-archive ()
  (let ((form (seq-find (lambda (candidate)
                          (eq (cadr candidate) 'crux))
                        (zoro-test-use-package-forms))))
    (should form)
    (should (eq (plist-get (cddr form) :ensure) t))
    (should-not (memq :vc (cddr form)))))

(ert-deftest zoro-integration-hyperbole-preserves-org-editing ()
  ;; HyWiki initialization used to open an Org buffer and run every Org hook,
  ;; including the retired GTD integration's lazy loader.
  (let* ((org-mode-calls 0)
         (org-mode-hook (cons (lambda () (cl-incf org-mode-calls))
                              org-mode-hook)))
    (require 'hyperbole)
    (should (zerop org-mode-calls)))
  (should (eq hsys-org-enable-smart-keys 'buttons))
  (should hyperbole-mode)
  (should-not hywiki-mode)
  (should-not hywiki--org-heading-regexp)
  (should-not (memq #'hywiki-kill-buffer-hook kill-buffer-hook))
  (with-temp-buffer
    (org-mode)
    (insert "* Existing heading")
    (let (org-fallback-called)
      (cl-letf (((symbol-function 'org-meta-return)
                 (lambda (&rest _args)
                   (interactive)
                   (setq org-fallback-called t))))
        (call-interactively (key-binding (kbd "M-RET"))))
      (should org-fallback-called))))

(ert-deftest zoro-integration-hyperbole-data-stays-in-emacs-directory ()
  (should (equal hbmap:dir-user
                 (expand-file-name "hyperbole/" user-emacs-directory)))
  (should (equal hbmap:dir-filename
                 (expand-file-name "HBMAP" hbmap:dir-user))))

(ert-deftest zoro-integration-org-and-agent-shell-without-gtd-runtime ()
  (require 'hyperbole)
  (require 'org-agenda)
  (require 'agent-shell)
  (with-temp-buffer
    (org-mode)
    (should (eq (key-binding (kbd "C-z a")) #'agent-shell)))
  (with-temp-buffer
    (org-agenda-mode)
    (should (eq (key-binding (kbd "C-z a")) #'agent-shell)))
  (should (eq agent-shell-dot-subdir-function #'agent-shell--dot-subdir-in-repo))
  (dolist (feature '(agent-shell-org-tasks emacs-gtd-assistant emacs-gtd-project))
    (should-not (featurep feature))))

(ert-deftest zoro-integration-agent-code-context ()
  (skip-unless (zoro-test-skill-file-readable-p
                "emacs-code-navigator/scripts/agent-shell-code-context.el"))
  (require 'agent-shell)
  (should (memq #'emacs-code-navigator-agent-shell-context agent-shell-context-sources)))

(provide 'integration-tests)
;;; integration-tests.el ends here
