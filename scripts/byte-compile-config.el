;;; byte-compile-config.el --- Compile saved configuration without artifacts -*- lexical-binding: t; -*-

;;; Commentary:
;; Compile every hand-written init file into a temporary directory.  Warnings
;; are errors so removed Emacs APIs and stale package interfaces fail CI, while
;; no .elc files are written into the checkout.

;;; Code:

(require 'bytecomp)
(require 'subr-x)

(defconst zoro-compile-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root used by the compatibility compiler.")

(setq user-emacs-directory zoro-compile-root)
(when-let* ((package-directory (getenv "ZORO_PACKAGE_DIR")))
  (setq package-user-dir (file-name-as-directory package-directory)))
(add-to-list 'load-path (expand-file-name "elisp" zoro-compile-root))
(package-activate-all)
(require 'use-package)

(let* ((destination (make-temp-file "zoro-byte-compile-" t))
       (byte-compile-error-on-warn t)
       (byte-compile-dest-file-function
        (lambda (source)
          (expand-file-name
           (concat (file-name-nondirectory source) "c") destination)))
       (files
        (append
         (list (expand-file-name "early-init.el" zoro-compile-root)
               (expand-file-name "init.el" zoro-compile-root))
         (directory-files (expand-file-name "elisp" zoro-compile-root)
                          t "\\`init-.*\\.el\\'")))
       failures)
  (unwind-protect
      (dolist (file files)
        (princ (format "Compiling %s\n" (file-relative-name file zoro-compile-root)))
        (condition-case error-data
            (unless (byte-compile-file file)
              (push (file-relative-name file zoro-compile-root) failures))
          (error
           (princ (format "Compilation error: %s\n"
                          (error-message-string error-data)))
           (push (file-relative-name file zoro-compile-root) failures))))
    (delete-directory destination t))
  (when failures
    (error "Byte compilation failed for: %s"
           (string-join (nreverse failures) ", "))))

(princ (format "Byte compilation passed on Emacs %s.\n" emacs-version))

;;; byte-compile-config.el ends here
