;;; init-functions-tests.el --- Tests for configuration-owned functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise decision logic owned by this configuration.  Third-party package
;; internals remain the responsibility of their upstream test suites.

;;; Code:

(require 'test-helper)

(ert-deftest zoro-version-baseline-accepts-current-emacs ()
  (should (version<= "31.0.90" emacs-version)))

(ert-deftest zoro-system-theme-defers-when-package-is-unavailable ()
  (let ((custom-enabled-themes '(existing-theme))
        disabled-themes
        loaded-theme)
    (cl-letf (((symbol-function 'custom-available-themes) (lambda () nil))
              ((symbol-function 'disable-theme)
               (lambda (theme) (push theme disabled-themes)))
              ((symbol-function 'load-theme)
               (lambda (theme &rest _) (setq loaded-theme theme))))
      (should-not (zoro-apply-system-theme 'light)))
    (should-not disabled-themes)
    (should-not loaded-theme)))

(ert-deftest zoro-system-theme-loads-when-package-is-available ()
  (let ((custom-enabled-themes '(existing-theme))
        disabled-themes
        loaded-theme)
    (cl-letf (((symbol-function 'custom-available-themes)
               (lambda () '(ef-frost)))
              ((symbol-function 'disable-theme)
               (lambda (theme) (push theme disabled-themes)))
              ((symbol-function 'load-theme)
               (lambda (theme &rest _) (setq loaded-theme theme))))
      (zoro-apply-system-theme 'light))
    (should (equal disabled-themes '(existing-theme)))
    (should (eq loaded-theme 'ef-frost))))

(ert-deftest zoro-restore-startup-state-restores-bounded-values ()
  (let ((gc-cons-threshold most-positive-fixnum)
        (gc-cons-percentage 0.9)
        (file-name-handler-alist nil)
        (file-name-handler-alist-original '(("example" . ignore))))
    (zoro-restore-startup-state)
    (should (= gc-cons-threshold (* 16 1024 1024)))
    (should (= gc-cons-percentage 0.1))
    (should (equal file-name-handler-alist '(("example" . ignore))))
    (should-not (boundp 'file-name-handler-alist-original))))

(ert-deftest zoro-org-line-range-excludes-delimiters ()
  (zoro-test-with-temp-file "zero\nBEGIN\none\ntwo\nEND\nlast\n" file
    (should (equal (zoro-org-decide-line-range file "^BEGIN$" "^END$")
                   "3-5"))))

(ert-deftest zoro-org-line-range-supports-open-ranges ()
  (zoro-test-with-temp-file "zero\nBEGIN\none\ntwo\nEND\nlast\n" file
    (should (equal (zoro-org-decide-line-range file "^BEGIN$" nil) "3-"))
    (should (equal (zoro-org-decide-line-range file nil "^END$") "-5"))
    (should (equal (zoro-org-decide-line-range file nil nil) "-"))))

(ert-deftest zoro-org-line-range-reports-missing-delimiter ()
  (zoro-test-with-temp-file "one\ntwo\n" file
    (should-error (zoro-org-decide-line-range file "missing" nil)
                  :type 'search-failed)))

(ert-deftest zoro-org-line-range-preserves-caller-match-data ()
  (string-match "\\(keep\\)" "keep")
  (let ((before (match-data)))
    (zoro-test-with-temp-file "BEGIN\nbody\nEND\n" file
      (zoro-org-decide-line-range file "BEGIN" "END"))
    (should (equal before (match-data)))))

(ert-deftest zoro-org-update-include-ranges-replaces-existing-lines ()
  (zoro-test-with-temp-file "zero\nBEGIN\none\ntwo\nEND\n" included
    (with-temp-buffer
      (insert (format "#+INCLUDE: \"%s\" :range-begin \"^BEGIN$\" :range-end \"^END$\" :lines \"1-2\"\n"
                      included))
      (zoro-org-update-include-ranges)
      (should (string-match-p ":lines \"3-5\"" (buffer-string))))))

(ert-deftest zoro-org-update-include-ranges-appends-lines-idempotently ()
  (zoro-test-with-temp-file "BEGIN\none\nEND\n" included
    (with-temp-buffer
      (insert (format "#+INCLUDE: \"%s\" :range-begin \"^BEGIN$\" :range-end \"^END$\"\n"
                      included))
      (zoro-org-update-include-ranges)
      (let ((once (buffer-string)))
        (zoro-org-update-include-ranges)
        (should (equal once (buffer-string)))
        (should (= 1 (how-many ":lines" (point-min) (point-max))))))))

(ert-deftest zoro-org-update-include-ranges-resolves-relative-files ()
  (let* ((directory (make-temp-file "zoro-include-" t))
         (included (expand-file-name "source.txt" directory)))
    (unwind-protect
        (progn
          (with-temp-file included
            (insert "BEGIN\nbody\nEND\n"))
          (with-temp-buffer
            (setq default-directory (file-name-as-directory directory))
            (insert "#+INCLUDE: \"source.txt\" :range-begin \"BEGIN\" :range-end \"END\"\n")
            (zoro-org-update-include-ranges)
            (should (string-match-p ":lines \"2-3\"" (buffer-string)))))
      (delete-directory directory t))))

(ert-deftest zoro-org-update-include-ranges-ignores-standard-include ()
  (with-temp-buffer
    (insert "#+INCLUDE: \"ordinary.org\"\n")
    (let ((before (buffer-string)))
      (zoro-org-update-include-ranges)
      (should (equal before (buffer-string))))))

(ert-deftest zoro-org-include-save-hook-is-buffer-local ()
  (with-temp-buffer
    (zoro-org-enable-include-range-updates)
    (should (local-variable-p 'before-save-hook))
    (should (memq #'zoro-org-update-include-ranges before-save-hook))))

(ert-deftest zoro-tempo-match-tag-finds-angle-prefixed-tag ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(<lambda")
    (should (equal (zoro-tempo--match-tag) '("<lambda" . 2)))))

(ert-deftest zoro-tempo-setup-composes-general-and-mode-tags ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (zoro-tempo-setup)
    (should (member '(zoro-tempo-prog-tags) tempo-local-tags))
    (should (member '(zoro-tempo-lisp-tags) tempo-local-tags))
    (should (eq tempo-match-finder #'zoro-tempo--match-tag))
    (should (local-variable-p 'tempo-match-finder))))

(ert-deftest zoro-tempo-latex-matrix-honors-requested-shape ()
  (let ((numbers '(2 3)) completion-arguments)
    (cl-letf (((symbol-function 'read-number)
               (lambda (&rest _) (pop numbers)))
              ((symbol-function 'completing-read)
               (lambda (&rest arguments)
                 (setq completion-arguments arguments)
                 "bmatrix")))
      (let ((matrix (zoro-tempo--latex-matrix)))
        (should (equal (nth 1 completion-arguments)
                       '("pmatrix" "bmatrix" "Bmatrix"
                         "vmatrix" "Vmatrix" "matrix")))
        (should (eq (nth 3 completion-arguments) t))
        (should (equal (nth 6 completion-arguments) "pmatrix"))
        (should (string-prefix-p "\\begin{bmatrix}\n" matrix))
        (should (string-suffix-p "\n\\end{bmatrix}" matrix))
        (should (= 2 (seq-count (lambda (line) (string-match-p "&" line))
                                (split-string matrix "\n"))))))))

(ert-deftest zoro-tempo-text-table-honors-requested-shape ()
  (let ((numbers '(2 3)))
    (cl-letf (((symbol-function 'read-number)
               (lambda (&rest _) (pop numbers))))
      (let ((lines (split-string (zoro-tempo--text-table) "\n")))
        (should (= 4 (length lines)))
        (should (equal (nth 1 lines) "|----+----+----|"))))))

(ert-deftest zoro-treesit-command-has-no-parser-fallback ()
  (let (reported)
    (cl-letf (((symbol-function 'treesit-available-p) (lambda () nil))
              ((symbol-function 'message)
               (lambda (format-string &rest arguments)
                 (setq reported (apply #'format format-string arguments)))))
      (zoro-treesit-show-parser-used-at-point)
      (should (equal reported "treesit is not available")))))

(ert-deftest zoro-shell-environment-import-applies-selected-values ()
  (let ((output (generate-new-buffer " *zoro-shell-output-test*"))
        (errors (generate-new-buffer " *zoro-shell-errors-test*"))
        (process-environment (copy-sequence process-environment))
        (exec-path '("before"))
        (previous-eshell-path-env (and (boundp 'eshell-path-env)
                                       (default-value 'eshell-path-env))))
    (unwind-protect
        (progn
          (with-current-buffer output
            (insert "PATH=/one:/two\0LANG=en_US.UTF-8\0IGNORED=value\0"))
          (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
                    ((symbol-function 'process-exit-status) (lambda (_) 0))
                    ((symbol-function 'process-buffer) (lambda (_) output))
                    ((symbol-function 'process-get)
                     (lambda (_ property)
                       (and (eq property 'stderr-buffer) errors))))
            (zoro--finish-shell-environment-import 'process "finished\n"))
          (should (equal (getenv "PATH") "/one:/two"))
          (should (equal (getenv "LANG") "en_US.UTF-8"))
          (should-not (getenv "IGNORED"))
          (should (equal exec-path
                         (append (parse-colon-path "/one:/two")
                                 (list exec-directory))))
          (should (equal (default-value 'eshell-path-env) "/one:/two"))
          (should-not (buffer-live-p output))
          (should-not (buffer-live-p errors)))
      (when (buffer-live-p output)
        (kill-buffer output))
      (when (buffer-live-p errors)
        (kill-buffer errors))
      (set-default 'eshell-path-env previous-eshell-path-env))))

(ert-deftest zoro-shell-environment-import-reports-process-failure ()
  (let ((output (generate-new-buffer " *zoro-shell-output-test*"))
        (errors (generate-new-buffer " *zoro-shell-errors-test*"))
        reported)
    (with-current-buffer errors
      (insert "shell failed\n"))
    (cl-letf (((symbol-function 'process-status) (lambda (_) 'exit))
              ((symbol-function 'process-exit-status) (lambda (_) 7))
              ((symbol-function 'process-buffer) (lambda (_) output))
              ((symbol-function 'process-get)
               (lambda (_ property)
                 (and (eq property 'stderr-buffer) errors)))
              ((symbol-function 'message)
               (lambda (format-string &rest arguments)
                 (setq reported (apply #'format format-string arguments)))))
      (zoro--finish-shell-environment-import 'process "finished\n"))
    (should (equal reported
                   "Login shell environment import failed: shell failed"))
    (should-not (buffer-live-p output))
    (should-not (buffer-live-p errors))))

(ert-deftest zoro-shell-environment-import-starts-login-shell-process ()
  (let ((shell-file-name "/bin/test-shell")
        process-arguments
        process-property)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest arguments)
                 (setq process-arguments arguments)
                 'process))
              ((symbol-function 'process-put)
               (lambda (_ property value)
                 (setq process-property (cons property value)))))
      (unwind-protect
          (should (eq (zoro-import-shell-environment) 'process))
        (when-let* ((buffer (plist-get process-arguments :buffer)))
          (kill-buffer buffer))
        (when-let* ((buffer (cdr process-property)))
          (kill-buffer buffer))))
    (should (equal (plist-get process-arguments :command)
                   '("/bin/test-shell" "-l" "-i" "-c"
                     "exec /usr/bin/env -0")))
    (should (eq (plist-get process-arguments :sentinel)
                #'zoro--finish-shell-environment-import))
    (should (eq (car process-property) 'stderr-buffer))))

(ert-deftest zoro-shell-environment-import-cleans-up-start-failure ()
  (let ((buffers-before (buffer-list))
        reported)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _) (error "cannot start")))
              ((symbol-function 'message)
               (lambda (format-string &rest arguments)
                 (setq reported (apply #'format format-string arguments)))))
      (should (equal (zoro-import-shell-environment)
                     "Could not start login shell: cannot start")))
    (should (equal reported "Could not start login shell: cannot start"))
    (should (equal (buffer-list) buffers-before))))

(ert-deftest zoro-dashboard-initial-buffer-is-stable ()
  (let ((first (zoro-initial-dashboard-buffer))
        (second (zoro-initial-dashboard-buffer)))
    (unwind-protect
        (should (eq first second))
      (kill-buffer first))))

(ert-deftest zoro-dashboard-buttons-and-shortcuts-run-the-same-command ()
  (dolist (action zoro-dashboard--actions)
    (pcase-let ((`(,key ,_icon ,_label ,command) action))
      (should (commandp command))
      (should (eq (keymap-lookup zoro-dashboard-mode-map key) command))
      (with-temp-buffer
        (let (invoked)
          (apply #'zoro-dashboard--insert-action action)
          (cl-letf (((symbol-function 'call-interactively)
                     (lambda (function &rest _) (setq invoked function))))
            (button-activate (next-button (point-min))))
          (should (eq invoked command)))))))

(ert-deftest zoro-gnus-and-message-hooks-run-their-configured-functions ()
  ;; Verify that topic mode can actually be loaded from its declared library.
  (when (autoloadp (symbol-function 'gnus-topic-mode))
    (autoload-do-load (symbol-function 'gnus-topic-mode) 'gnus-topic-mode))
  ;; Exercise hook dispatch without connecting to news servers or starting
  ;; an external spelling process.
  (let (called)
    (cl-letf (((symbol-function 'gnus-topic-mode)
               (lambda (&rest _) (push 'topics called)))
              ((symbol-function 'display-fill-column-indicator-mode)
               (lambda (&rest _) (push 'column called)))
              ((symbol-function 'flyspell-mode)
               (lambda (&rest _) (push 'spelling called))))
      (run-hooks 'gnus-group-mode-hook 'message-mode-hook))
    (dolist (feature '(topics column spelling))
      (should (memq feature called)))))

(ert-deftest zoro-org-pdf-compilation-preserves-source-logs-and-auto-directory ()
  (require 'ox-latex)
  (let* ((directory (make-temp-file "zoro-latex audit-" t))
         (default-directory (file-name-as-directory directory))
         (tools (expand-file-name "tools/" directory))
         (process-environment (copy-sequence process-environment))
         (preserved '("paper draft.tex" "paper draft.log" "paper draft.blg"
                      "paper draft.bbl" "paper draft.ind" "auto/keep.el")))
    (unwind-protect
        (progn
          (make-directory tools)
          (make-directory "auto")
          (dolist (file (append preserved '("paper draft.aux" "paper draft.out")))
            (with-temp-file file (insert "fixture\n")))
          ;; Run the configured shell pipeline with stand-ins for the TeX
          ;; tools.  Only the PDF creation is simulated; cleanup runs normally.
          (dolist (tool '("xelatex" "bibtex" "makeindex"))
            (let ((file (expand-file-name tool tools)))
              (with-temp-file file
                (insert "#!/bin/sh\n")
                (if (equal tool "xelatex")
                    (insert "for argument do source=$argument; done\n"
                            ": > \"${source%.tex}.pdf\"\n")
                  (insert "exit 0\n")))
              (set-file-modes file #o700)))
          (setenv "PATH" (concat tools path-separator (getenv "PATH")))
          (should (file-exists-p (org-latex-compile "paper draft.tex")))
          (should-not (file-exists-p "paper draft.aux"))
          (should-not (file-exists-p "paper draft.out"))
          (dolist (file preserved)
            (should (file-exists-p file))))
      (delete-directory directory t))))

(provide 'init-functions-tests)
;;; init-functions-tests.el ends here
