;;; config-score.el --- Evidence-based configuration scorecard -*- lexical-binding: t; -*-

;;; Commentary:
;; Produce a 100-point Markdown score using the documented nine dimensions.
;; Repository evidence and CI outcomes are shown explicitly; this is an audit
;; aid rather than a claim that subjective maintainability is fully automated.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst zoro-score-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root scored by this script.")

(defun zoro-score-file (relative)
  "Return the contents of RELATIVE file, or an empty string when absent."
  (let ((file (expand-file-name relative zoro-score-root)))
    (if (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (buffer-string))
      "")))

(defun zoro-score-match-p (regexp relative)
  "Return non-nil when REGEXP occurs in RELATIVE file."
  (string-match-p regexp (zoro-score-file relative)))

(defun zoro-score-count (regexp text)
  "Count non-overlapping matches for REGEXP in TEXT."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward regexp nil t)
        (setq count (1+ count)))
      count)))

(defun zoro-score-status (name)
  "Return non-nil when environment status NAME equals success."
  (string= (getenv name) "success"))

(defun zoro-score-elisp-files ()
  "Return the hand-written configuration modules."
  (directory-files (expand-file-name "elisp" zoro-score-root)
                   t "\\`init-.*\\.el\\'"))

(defun zoro-score-module-contracts-p ()
  "Return non-nil when init modules declare Commentary and matching provide."
  (seq-every-p
   (lambda (file)
     (let* ((contents (zoro-score-file (file-relative-name file zoro-score-root)))
            (feature (intern (file-name-base file))))
       (and (string-match-p ";;; Commentary:" contents)
            (string-match-p
             (format "(provide '%s)" (regexp-quote (symbol-name feature)))
             contents))))
   (zoro-score-elisp-files)))

(defun zoro-score-git-history-p ()
  "Return non-nil when enough non-empty Git history is available."
  (with-temp-buffer
    (and (zerop (call-process "git" nil t nil "-C" zoro-score-root
                              "rev-list" "--count" "HEAD"))
         (>= (string-to-number (string-trim (buffer-string))) 10))))

(let* ((readme (zoro-score-file "README.md"))
       (all-elisp (mapconcat
                   (lambda (file)
                     (zoro-score-file (file-relative-name file zoro-score-root)))
                   (zoro-score-elisp-files) "\n"))
       (test-text (concat (zoro-score-file "test/init-functions-tests.el")
                          (zoro-score-file "test/integration-tests.el")))
       (test-count (zoro-score-count "(ert-deftest " test-text))
       (finding-count (zoro-score-count "^## F[0-9]+" (zoro-score-file "test/FINDINGS.md")))
       (module-count (length (zoro-score-elisp-files)))
       (use-package-count (zoro-score-count "^[[:space:]]*(use-package " all-elisp))
       (ensure-count (zoro-score-count "^[[:space:]]*:ensure " all-elisp))
       (vc-count (zoro-score-count "^[[:space:]]*:vc " all-elisp))
       (origin-count (+ ensure-count vc-count))
       (rows
        (list
         (list "Structure and separation of concerns" 15
               (+ (if (>= module-count 15) 5 0)
                  (if (string-match-p "## 配置结构" readme) 4 0)
                  (if (zoro-score-module-contracts-p) 4 0)
                  (if (file-exists-p (expand-file-name "elisp/init-const.el" zoro-score-root)) 2 0))
               (format "%d responsibility-focused modules; README responsibility table; module Commentary/provide contracts"
                       module-count))
         (list "Preference for official and built-in features" 15
               (+ (if (zoro-score-match-p "package-install-upgrade-built-in nil" "elisp/init-package.el") 4 0)
                  (if (= use-package-count origin-count) 5 0)
                  (if (not (string-match-p "straight-use-package\\|quelpa\\|package-enable-at-startup t" all-elisp)) 3 0)
                  (if (string-match-p "Emacs 31 内置" readme) 3 0))
               (format "%d/%d package declarations explicitly use :ensure/:vc; built-in package replacement disabled"
                       origin-count use-package-count))
         (list "Dependency discipline" 10
               (+ (if (= use-package-count origin-count) 4 0)
                  (if (zoro-score-status "ZORO_BOOTSTRAP_STATUS") 3 0)
                  (if (string-match-p "包管理路径单一" readme) 3 0))
               "Explicit sources, a single package.el path, and clean-environment installation results")
         (list "Understandability and maintainability" 15
               (+ (if (zoro-score-module-contracts-p) 5 0)
                  (if (>= test-count 10) 4 0)
                  (if (zoro-score-status "ZORO_COMPILE_STATUS") 4 0)
                  (if (string-match-p "## 配置结构" readme) 2 0))
               (format "%d ERT tests; warning-as-error compilation=%s"
                       test-count (or (getenv "ZORO_COMPILE_STATUS") "not run")))
         (list "Reliability and testing" 15
               (max 0
                    (- (+ (if (zoro-score-status "ZORO_ERT_STATUS") 6 0)
                          (if (zoro-score-status "ZORO_STARTUP_STATUS") 4 0)
                          (if (file-exists-p (expand-file-name ".github/workflows/validate.yml" zoro-score-root)) 3 0)
                          (if (>= test-count 10) 2 0))
                       (min 2 finding-count)))
               (format "ERT=%s; startup=%s; %d recorded findings"
                       (or (getenv "ZORO_ERT_STATUS") "not run")
                       (or (getenv "ZORO_STARTUP_STATUS") "not run") finding-count))
         (list "Startup and runtime performance" 10
               (+ (if (file-exists-p (expand-file-name "elisp/benchmark-startup.el" zoro-score-root)) 3 0)
                  (if (zoro-score-status "ZORO_PERF_STATUS") 4 0)
                  (if (string-match-p "模块计时报告" readme) 3 0))
               (format "Benchmark, GC, and module timing; performance check=%s"
                       (or (getenv "ZORO_PERF_STATUS") "not run")))
         (list "Portability and recoverability" 8
               (+ (if (file-exists-p (expand-file-name "elisp/init-const.el" zoro-score-root)) 3 0)
                  (if (not (string-match-p "/home/[^/]" all-elisp)) 2 0)
                  (if (string-match-p "## 安装" readme) 2 0)
                  (if (zoro-score-match-p "/elpa/" ".gitignore") 1 0))
               "Centralized shared paths, no hard-coded /home paths, installation instructions, and ignore rules")
         (list "Documentation and history" 7
               (+ (if (and (string-match-p "## 验证" readme)
                           (string-match-p "## 配置结构" readme)) 3 0)
                  (if (zoro-score-module-contracts-p) 2 0)
                  (if (zoro-score-git-history-p) 2 0))
               "README verification instructions, module documentation, and sufficient Git history")
         (list "Fit with personal workflows" 5
               (+ (if (seq-every-p (lambda (word) (string-match-p word readme))
                                    '("Org" "Denote" "AI")) 3 0)
                  (if (and (string-match-p "org-capture-templates" all-elisp)
                           (string-match-p "hywiki-directory" all-elisp)) 2 0))
               "Practical configuration for Org/Denote/AI, GTD capture, and HyWiki")))
       (total (apply #'+ (mapcar (lambda (row) (nth 2 row)) rows)))
       (report
        (concat
         (format "## Emacs configuration score: %d/100\n\n" total)
         (format "Emacs: `%s` · ERT declarations: %d · Recorded findings: %d\n\n"
                 emacs-version test-count finding-count)
         "| Dimension | Score | Weight | Automated evidence |\n|---|---:|---:|---|\n"
         (mapconcat
          (lambda (row)
            (format "| %s | %d | %d | %s |"
                    (nth 0 row) (nth 2 row) (nth 1 row) (nth 3 row)))
          rows "\n")
         "\n\n> This score is based on documented rules and evidence from the current CI run; subjective quality still requires human review.\n")))
  (princ report)
  (when-let* ((summary (getenv "GITHUB_STEP_SUMMARY")))
    (with-temp-buffer
      (insert report)
      (append-to-file (point-min) (point-max) summary))))

;;; config-score.el ends here
