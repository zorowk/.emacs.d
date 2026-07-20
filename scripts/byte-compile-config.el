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
(require 'init-const)
(defconst zoro-compile-state-directory
  (make-temp-file "zoro-compile-state-" t)
  "Disposable personal-data root used while compiling configuration.")
;; A `use-package' macro expansion may load Hyperbole before its runtime
;; `:init' form runs.  Keep that compile-time initialization disposable.
(setq hbmap:dir-user
      (expand-file-name "hyperbole/" zoro-compile-state-directory)
      hbmap:dir-filename (expand-file-name "HBMAP" hbmap:dir-user)
      hywiki-directory
      (expand-file-name "hywiki/" zoro-compile-state-directory)
      hywiki-org-publishing-directory
      (expand-file-name "public_hywiki/" zoro-compile-state-directory)
      hyrolo-default-file
      (expand-file-name "rolo.org" zoro-compile-state-directory)
      hyrolo-file-list (list hyrolo-default-file)
      hynote-directory-list (list hywiki-directory))
(package-activate-all)
(require 'use-package)
;; Load declarations from deferred LaTeX packages before compiling their
;; configuration with warnings promoted to errors.
(require 'tex)
(require 'reftex)

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
    (delete-directory destination t)
    (delete-directory zoro-compile-state-directory t))
  (when failures
    (error "Byte compilation failed for: %s"
           (string-join (nreverse failures) ", "))))

(princ (format "Byte compilation passed on Emacs %s.\n" emacs-version))

;;; byte-compile-config.el ends here
