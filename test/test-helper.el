;;; test-helper.el --- Shared test setup -*- lexical-binding: t; -*-

;;; Commentary:
;; Load the saved configuration exactly as the documented batch startup does,
;; then provide small helpers for configuration and package audits.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'package)
(require 'seq)
(require 'subr-x)

(defconst zoro-test-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Repository root used by the test suite.")

(setq user-emacs-directory zoro-test-root)
(when-let* ((package-directory (getenv "ZORO_PACKAGE_DIR")))
  (setq package-user-dir (file-name-as-directory package-directory)))
(add-to-list 'load-path (expand-file-name "elisp" zoro-test-root))

(load (expand-file-name "early-init.el" zoro-test-root) nil nil t)
(package-activate-all)
(load (expand-file-name "init.el" zoro-test-root) nil nil t)

(defun zoro-test-read-forms (file)
  "Return all top-level Lisp forms read from FILE without evaluating them."
  (with-temp-buffer
    (insert-file-contents file)
    (let (forms)
      (condition-case nil
          (while t
            (push (read (current-buffer)) forms))
        (end-of-file))
      (nreverse forms))))

(defun zoro-test-collect-use-package-forms (form)
  "Return every `use-package' form nested in FORM."
  (when (and (consp form) (not (memq (car form) '(quote function))))
    (let ((found (and (eq (car form) 'use-package)
                      (symbolp (cadr form))
                      (list form)))
          (tail form))
      (while (consp tail)
        (setq found
              (nconc found
                     (zoro-test-collect-use-package-forms (car tail)))
              tail (cdr tail)))
      (when tail
        (setq found
              (nconc found (zoro-test-collect-use-package-forms tail))))
      found)))

(defun zoro-test-bundled-library-p (library)
  "Return non-nil when LIBRARY is supplied by this Emacs installation."
  (or (eq library 'emacs)
      (package-built-in-p library)
      (when-let* ((file (locate-library (symbol-name library))))
        (string-prefix-p (file-truename (expand-file-name "../lisp" data-directory))
                         (file-truename file)))))

(defun zoro-test-use-package-forms ()
  "Return all `use-package' declarations from saved init modules."
  (let (forms)
    (dolist (file (directory-files (expand-file-name "elisp" zoro-test-root)
                                   t "\\`init-.*\\.el\\'"))
      (dolist (form (zoro-test-read-forms file))
        (setq forms
              (nconc forms (zoro-test-collect-use-package-forms form)))))
    forms))

(defmacro zoro-test-with-temp-file (contents variable &rest body)
  "Create a file containing CONTENTS, bind its name to VARIABLE, run BODY."
  (declare (indent 2) (debug (form symbolp body)))
  `(let ((,variable (make-temp-file "zoro-test-" nil ".txt")))
     (unwind-protect
         (progn
           (with-temp-file ,variable
             (insert ,contents))
           ,@body)
       (delete-file ,variable))))

(provide 'test-helper)
;;; test-helper.el ends here
