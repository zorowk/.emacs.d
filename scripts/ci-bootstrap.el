;;; ci-bootstrap.el --- Install packages for clean CI validation -*- lexical-binding: t; -*-

;;; Commentary:
;; Refresh the configured package archives and load the complete configuration.
;; Every external package declaration is therefore exercised against a clean
;; or cached package directory before the offline validation steps run.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst zoro-ci-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root used by CI scripts.")

(defun zoro-ci-read-forms (file)
  "Return all top-level forms read from FILE without evaluating them."
  (with-temp-buffer
    (insert-file-contents file)
    (let (forms)
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file))
      (nreverse forms))))

(defun zoro-ci-collect-use-package-forms (form)
  "Return every unquoted `use-package' declaration nested in FORM."
  (when (and (consp form) (not (memq (car form) '(quote function))))
    (let ((found (and (eq (car form) 'use-package)
                      (symbolp (cadr form))
                      (list form)))
          (tail form))
      (while (consp tail)
        (setq found
              (nconc found
                     (zoro-ci-collect-use-package-forms (car tail)))
              tail (cdr tail)))
      (when tail
        (setq found
              (nconc found (zoro-ci-collect-use-package-forms tail))))
      found)))

(defun zoro-ci-third-party-packages ()
  "Return every package declared with `:ensure t' in init modules."
  (let (packages)
    (dolist (file (directory-files (expand-file-name "elisp" zoro-ci-root)
                                   t "\\`init-.*\\.el\\'"))
      (dolist (top-level (zoro-ci-read-forms file))
        (dolist (form (zoro-ci-collect-use-package-forms top-level))
          (when (eq (plist-get (cddr form) :ensure) t)
            (cl-pushnew (cadr form) packages)))))
    (sort packages #'string-lessp)))

(setq user-emacs-directory zoro-ci-root)
(when-let* ((package-directory (getenv "ZORO_PACKAGE_DIR")))
  (setq package-user-dir (file-name-as-directory package-directory)))
(add-to-list 'load-path (expand-file-name "elisp" zoro-ci-root))

(unless (version<= "31.0.90" emacs-version)
  (error "Emacs %s is older than the required 31.0.90 baseline" emacs-version))

(load (expand-file-name "early-init.el" zoro-ci-root) nil nil t)
(require 'package)
(require 'init-package)
(unless (getenv "ZORO_SKIP_PACKAGE_REFRESH")
  (package-refresh-contents))
(let ((packages (zoro-ci-third-party-packages)))
  (dolist (package packages)
    (unless (package-installed-p package)
      (princ (format "Installing %S\n" package))
      (package-install package)))
  (dolist (package packages)
    (unless (package-installed-p package)
      (error "Declared package was not installed: %S" package))))
(package-activate-all)
(load (expand-file-name "init.el" zoro-ci-root) nil nil t)

(princ (format "CI bootstrap completed with Emacs %s and %d activated packages.\n"
               emacs-version (length package-activated-list)))

;;; ci-bootstrap.el ends here
