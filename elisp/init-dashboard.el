;;; init-dashboard.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-dashboard.el
;; Description: Initialize Dashboard
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 17:21:46 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d dashboard
;; Compatibility: Emacs 31
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes dashboard
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defconst zoro-dashboard-items-with-agenda
  '((recents . 7)
    (bookmarks . 7)
    (agenda . 5))
  "Dashboard items shown after deferred Agenda initialization.")

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(defun zoro-dashboard-load ()
  "Load Dashboard after the initial frame becomes idle."
  (require 'dashboard))

;; DashboardPac
(use-package dashboard
  :ensure t
  :defer t
  :diminish (dashboard-mode)
  :bind
  (("C-z d" . open-dashboard)
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
       (lambda (&rest _) (browse-url "https://zorowk.github.io/")))
     ("" "Configuration" "Edit a configuration file"
      (lambda (&rest _)
        (let ((default-directory user-emacs-directory))
          (project-find-file))))
     ("" "Info" "Open Emacs Info"
      (lambda (&rest _) (info))))))
  :custom-face
  (dashboard-banner-logo-title ((t (:family "Apple Chancery" :height 200))))
  :init
  (run-with-idle-timer 0.1 nil #'zoro-dashboard-load)
  :config
  (defun zoro-dashboard-update-banner (appearance)
    "Set and refresh the dashboard banner for APPEARANCE."
    (setq dashboard-startup-banner
          (expand-file-name
           (if (eq appearance 'dark)
               "images/KEC_Dark_BK_Small.png"
             "images/KEC_Light_BK_Small.png")
           user-emacs-directory))
    (when-let* ((buffer (get-buffer dashboard-buffer-name)))
      (with-current-buffer buffer
        (dashboard-insert-startupify-lists t))))
  (zoro-dashboard-update-banner frame-background-mode)
  (add-hook 'window-size-change-functions #'dashboard-resize-on-hook 100)

  (defun zoro-dashboard-enable-agenda ()
    "Add the Agenda widget and refresh an existing Dashboard buffer."
    (setq dashboard-items zoro-dashboard-items-with-agenda)
    (when-let* ((buffer (get-buffer dashboard-buffer-name)))
      (with-current-buffer buffer
        (dashboard-insert-startupify-lists t))))

  (run-with-idle-timer 2 nil #'zoro-dashboard-enable-agenda)

  ;; Open Dashboard function
  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)
    (goto-char (point-min))
    (delete-other-windows)))
;; -DashboardPac

(provide 'init-dashboard)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
