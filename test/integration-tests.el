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
  (require 'hyperbole)
  (should (eq hsys-org-enable-smart-keys 'buttons))
  (should hyperbole-mode)
  (should-not hywiki-mode)
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

(ert-deftest zoro-integration-org-agent-task-entry-points ()
  (skip-unless (zoro-test-skill-file-readable-p
                "emacs-gtd-assistant/scripts/agent-shell-org-tasks.el"))
  (require 'org-agenda)
  (should (eq (lookup-key org-mode-map (kbd "C-z a")) #'emacs-gtd-agent-open))
  (should (eq (lookup-key org-agenda-mode-map (kbd "C-z a")) #'emacs-gtd-agent-open))
  (should (eq (lookup-key org-agenda-mode-map (kbd "C-c C-x w")) #'emacs-gtd-agent-wrap-up))
  (should (equal (emacs-gtd--files) (org-agenda-files))))

(ert-deftest zoro-integration-agent-storage-and-existing-code-context ()
  (skip-unless (zoro-test-skill-file-readable-p
                "emacs-gtd-assistant/scripts/agent-shell-org-tasks.el"))
  (require 'agent-shell)
  (should (eq agent-shell-dot-subdir-function #'emacs-gtd-agent-dot-subdir))
  (should (equal emacs-gtd-agent-storage-directory
                 (expand-file-name ".agent-shell/" zoro-dropbox-directory))))

(ert-deftest zoro-integration-agent-code-context ()
  (skip-unless (zoro-test-skill-file-readable-p
                "emacs-code-navigator/scripts/agent-shell-code-context.el"))
  (require 'agent-shell)
  (should (memq #'emacs-code-navigator-agent-shell-context agent-shell-context-sources)))

(ert-deftest zoro-integration-agent-storage-follows-session-origin ()
  (skip-unless (zoro-test-skill-file-readable-p
                "emacs-gtd-assistant/scripts/agent-shell-org-tasks.el"))
  (require 'agent-shell)
  (let ((emacs-gtd-agent-storage-directory "/tmp/zoro-central-storage/")
        (cwd (file-name-as-directory zoro-test-root)))
    (cl-letf (((symbol-function 'agent-shell-cwd) (lambda () cwd)))
      (dolist (subdir '("transcripts" "screenshots"))
        (let ((emacs-gtd-agent-project-id nil)
              (emacs-gtd-agent--starting-project-id nil))
          (should (equal (funcall agent-shell-dot-subdir-function subdir)
                         (expand-file-name (concat ".agent-shell/" subdir) cwd))))
        ;; Cover both an existing Org session and a project being started.
        (dolist (starting '(nil t))
          (let ((emacs-gtd-agent-project-id (unless starting "org-project"))
                (emacs-gtd-agent--starting-project-id (when starting "org-project")))
            (should
             (equal (funcall agent-shell-dot-subdir-function subdir)
                    (expand-file-name
                     (concat "projects/" (secure-hash 'sha256 "org-project") "/" subdir)
                     emacs-gtd-agent-storage-directory)))))))))

(provide 'integration-tests)
;;; integration-tests.el ends here
