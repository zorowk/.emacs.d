;;; init-functions-tests.el --- Tests for configuration-owned functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise decision logic owned by this configuration.  Third-party package
;; internals remain the responsibility of their upstream test suites.

;;; Code:

(require 'test-helper)

(ert-deftest zoro-version-baseline-accepts-current-emacs ()
  (should (version<= "31.0.90" emacs-version)))

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
  (let ((numbers '(2 3)))
    (cl-letf (((symbol-function 'read-number)
               (lambda (&rest _) (pop numbers)))
              ((symbol-function 'read-string)
               (lambda (&rest _) "bmatrix")))
      (let ((matrix (zoro-tempo--latex-matrix)))
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

(ert-deftest zoro-dashboard-initial-buffer-is-stable ()
  (let ((first (zoro-initial-dashboard-buffer))
        (second (zoro-initial-dashboard-buffer)))
    (unwind-protect
        (should (eq first second))
      (kill-buffer first))))

(provide 'init-functions-tests)
;;; init-functions-tests.el ends here
