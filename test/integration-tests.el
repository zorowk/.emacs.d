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
                     init-complete init-templates init-ess init-latex init-org
                     init-hyperbole init-reader init-erc init-llm init-gnus))
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

(ert-deftest zoro-integration-contextual-m-slash-ownership ()
  (require 'eglot)
  (should (eq (lookup-key global-map (kbd "M-/")) #'dabbrev-completion))
  (should (eq (lookup-key eglot-mode-map (kbd "M-/"))
              #'eglot-find-implementation)))

(ert-deftest zoro-integration-denote-search-key-has-single-declaration ()
  (with-temp-buffer
    (insert-file-contents (expand-file-name "elisp/init-org.el" zoro-test-root))
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "\"C-c n g\"" nil t)
        (setq count (1+ count)))
      (should (= count 1)))))

(ert-deftest zoro-integration-hyperbole-provides-hywiki ()
  (require 'hywiki)
  (should (fboundp 'hywiki-mode)))

(provide 'integration-tests)
;;; integration-tests.el ends here
