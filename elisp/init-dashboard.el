;;; init-dashboard.el --- Startup dashboard -*- lexical-binding: t -*-

;; Author: zorowk
;; Copyright (C) 2019 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure and populate the startup dashboard.

;;; Code:

(declare-function dashboard-insert-startupify-lists "dashboard")

(defvar dashboard-buffer-name)
(defvar dashboard-items)
(defvar dashboard-startup-banner)

(defun zoro-initial-dashboard-buffer ()
  "Return the lightweight initial Dashboard buffer."
  (get-buffer-create "*dashboard*"))

(defun zoro-dashboard-update-banner (appearance)
  "Set and refresh the Dashboard banner for APPEARANCE."
  (setq dashboard-startup-banner
        (expand-file-name
         (if (eq appearance 'dark)
             "images/KEC_Dark_BK_Small.png"
           "images/KEC_Light_BK_Small.png")
         user-emacs-directory))
  (when-let* ((buffer-name (and (boundp 'dashboard-buffer-name)
                                dashboard-buffer-name))
              (buffer (get-buffer buffer-name)))
    (with-current-buffer buffer
      (dashboard-insert-startupify-lists t))))

(defun zoro-dashboard-enable-agenda ()
  "Add the Agenda widget and refresh an existing Dashboard buffer."
  (require 'dashboard)
  (setq dashboard-items '((recents . 7)
                          (bookmarks . 7)
                          (agenda . 5)))
  (when-let* ((buffer (get-buffer dashboard-buffer-name)))
    (with-current-buffer buffer
      (dashboard-insert-startupify-lists t))))

(defun zoro-open-dashboard ()
  "Open the Dashboard buffer and jump to the first widget."
  (interactive)
  (require 'dashboard)
  (dashboard-insert-startupify-lists)
  (switch-to-buffer dashboard-buffer-name)
  (goto-char (point-min))
  (delete-other-windows))

(defun zoro-dashboard-browse-homepage (&rest _)
  "Open the configured homepage."
  (browse-url "https://zorowk.github.io/"))

(defun zoro-dashboard-find-config (&rest _)
  "Find a file in the Emacs configuration project."
  (let ((default-directory user-emacs-directory))
    (project-find-file)))

(defun zoro-dashboard-open-info (&rest _)
  "Open the Emacs Info reader."
  (info))

(setq initial-buffer-choice #'zoro-initial-dashboard-buffer)

(use-package dashboard
  :ensure t
  :defer t
  :diminish (dashboard-mode)
  :bind
  (("C-z d" . zoro-open-dashboard)
   :map dashboard-mode-map
   (("n" . dashboard-next-line)
    ("p" . dashboard-previous-line)
    ("N" . dashboard-next-section)
    ("F" . dashboard-previous-section)))
  :custom
  (dashboard-banner-logo-title "Close the world. Open the nExt.")
  (dashboard-items '((recents  . 7)
                     (bookmarks . 7)))
  (dashboard-set-heading-icons nil)
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-newline
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-navigator
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-items
     dashboard-insert-newline
     dashboard-insert-footer))
  (dashboard-navigator-buttons
   '((("" "Blog" "Browse Homepage"
       zoro-dashboard-browse-homepage)
     ("" "Configuration" "Edit a configuration file"
      zoro-dashboard-find-config)
     ("" "Info" "Open Emacs Info"
      zoro-dashboard-open-info))))
  :custom-face
  (dashboard-banner-logo-title ((t (:family "Apple Chancery" :height 200))))
  :config
  (zoro-dashboard-update-banner frame-background-mode)
  (add-hook 'window-size-change-functions #'dashboard-resize-on-hook 100))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
