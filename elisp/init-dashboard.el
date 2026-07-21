;;; init-dashboard.el --- Startup dashboard -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure and populate the startup dashboard.

;;; Code:

(declare-function dashboard-insert-startupify-lists "dashboard")

(defvar dashboard-buffer-name)
(defvar dashboard-items)

(defun zoro-initial-dashboard-buffer ()
  "Return the lightweight initial Dashboard buffer."
  (get-buffer-create "*dashboard*"))

(defun zoro-dashboard-load ()
  "Load Dashboard and render its widgets."
  (require 'dashboard)
  (with-current-buffer (zoro-initial-dashboard-buffer)
    (dashboard-insert-startupify-lists t)))

(defun zoro-open-dashboard ()
  "Open the Dashboard buffer and jump to the first widget."
  (interactive)
  (require 'dashboard)
  (dashboard-insert-startupify-lists)
  (switch-to-buffer dashboard-buffer-name)
  (goto-char (point-min))
  (delete-other-windows))

(setq initial-buffer-choice #'zoro-initial-dashboard-buffer)

(use-package dashboard
  :ensure t
  :demand t
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
  (dashboard-startup-banner 'logo-braille)
  (dashboard-items '((recents  . 7)
                     (bookmarks . 7)
                     (agenda . 5)))
  (dashboard-set-heading-icons nil)
  (dashboard-startupify-list
   '(dashboard-insert-banner
     dashboard-insert-banner-title
     dashboard-insert-newline
     dashboard-insert-init-info
     dashboard-insert-items
     dashboard-insert-newline
     dashboard-insert-footer))
  :custom-face
  (dashboard-banner-logo-title ((t (:family "Apple Chancery" :height 200))))
  :config
  (add-hook 'window-size-change-functions #'dashboard-resize-on-hook 100))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
