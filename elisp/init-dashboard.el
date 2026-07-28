;;; init-dashboard.el --- Scriptorium startup page -*- lexical-binding: t -*-

;; Author: Mingde (Matthew) Zeng
;; Maintainer: zorowk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Draw a small, theme-aware startup page without an external dashboard
;; package.  Its typography and spacing are inspired by a printed title page.

;;; Code:

(require 'button)

(defface zoro-dashboard-title
  '((t (:inherit variable-pitch :height 4.2 :weight light)))
  "Face for the dashboard title."
  :group 'faces)

(defface zoro-dashboard-subtitle
  '((t (:inherit variable-pitch :height 1.35 :weight light)))
  "Face for the dashboard subtitle."
  :group 'faces)

(defface zoro-dashboard-tagline
  '((t (:inherit shadow :height 1.05)))
  "Face for the dashboard tagline."
  :group 'faces)

(defface zoro-dashboard-action
  '((t (:inherit default :height 1.12)))
  "Face for dashboard actions."
  :group 'faces)

(defface zoro-dashboard-icon
  `((t (:inherit default
        :family ,(cond ((eq system-type 'gnu/linux) "Symbola")
                       ((eq system-type 'darwin) "Apple Symbols"))
        :height 1.25)))
  "Face for dashboard action icons."
  :group 'faces)

(defface zoro-dashboard-key
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Face for dashboard shortcut keys."
  :group 'faces)

(defconst zoro-dashboard--actions
  '(("r" "♖" "Recent Files"   consult-recent-file)
    ("p" "♔" "Projects"       project-switch-project)
    ("a" "♗" "Agenda"         consult-org-agenda)
    ("e" "♙" "Elfeed"         elfeed)
    ("n" "♕" "Notes"          consult-denote-find)
    ("b" "♘" "Bookmark"       consult-bookmark))
  "Shortcut, symbol, label, and command for each dashboard action.")

(defvar-local zoro-dashboard--rendering nil)

(defun zoro-dashboard--insert-centered (text &optional face)
  "Insert TEXT centered in the current window, optionally using FACE."
  (let ((rendered (if face (propertize text 'face face) text)))
    (if (display-graphic-p)
        (let ((half-width (/ (string-pixel-width rendered (current-buffer)) 2)))
          (insert (propertize
                   " " 'display
                   `(space :align-to (- center (,half-width))))))
      (let* ((width (max 1 (window-body-width)))
             (padding (max 0 (/ (- width (string-width text)) 2))))
        (insert (make-string padding ?\s))))
    (insert rendered)
    (insert "\n")))

(defun zoro-dashboard--insert-rule (ornament)
  "Insert a centered divider decorated with ORNAMENT."
  (let* ((ornament-text (concat " " ornament " "))
         (remaining-width (max 2 (- 40 (string-width ornament-text))))
         (left-width (/ remaining-width 2))
         (right-width (- remaining-width left-width))
         (left-rule (propertize (make-string left-width ?─) 'face 'shadow))
         (center (propertize ornament-text
                             'face '(:inherit zoro-dashboard-icon :height 1.0)))
         (right-rule (propertize (make-string right-width ?─) 'face 'shadow)))
    (zoro-dashboard--insert-centered (concat left-rule center right-rule))))

(defun zoro-dashboard--insert-action (key symbol label command)
  "Insert an action row described by KEY, SYMBOL, LABEL, and COMMAND."
  (if (display-graphic-p)
      (let* ((icon (propertize symbol 'face 'zoro-dashboard-icon))
             (icon-width (string-pixel-width icon (current-buffer)))
             (half-icon-width (round (/ icon-width 2.0))))
        (insert (propertize
                 " " 'display
                 `(space :align-to
                         (- center (12 . width) (,half-icon-width)))))
        (insert icon)
        (insert (propertize
                 " " 'display
                 '(space :align-to (- center (9 . width))))))
    (let* ((row-width 30)
           (padding (max 0 (/ (- (window-body-width) row-width) 2))))
      (insert (make-string padding ?\s))
      (insert (propertize (format "%-3s" symbol)
                          'face 'zoro-dashboard-icon))))
  (let ((button-label (if (display-graphic-p) label (format "%-20s" label))))
    (insert-text-button
     button-label
     'face 'zoro-dashboard-action
     'mouse-face 'highlight
     'follow-link t
     'help-echo (format "[%s] %s" key label)
     'action (lambda (_) (call-interactively command))))
  (when (display-graphic-p)
    (insert (propertize
             " " 'display
             '(space :align-to (+ center (12 . width))))))
  (unless (display-graphic-p)
    (insert " "))
  (insert (propertize key 'face 'zoro-dashboard-key))
  (insert "\n"))

(defun zoro-dashboard-render (&optional _frame)
  "Render the dashboard in its current window.
The optional FRAME argument makes this suitable for resize hooks."
  (interactive)
  (when-let* ((buffer (get-buffer "*dashboard*")))
    (let ((window (or (get-buffer-window buffer t) (selected-window))))
      (with-selected-window window
        (with-current-buffer buffer
          (unless zoro-dashboard--rendering
            (let ((zoro-dashboard--rendering t)
                  (inhibit-read-only t)
                  (top-padding (max 1 (/ (- (window-body-height) 31) 3))))
              (erase-buffer)
              (insert (make-string top-padding ?\n))
              (zoro-dashboard--insert-centered "E M A C S" 'zoro-dashboard-title)
              (zoro-dashboard--insert-centered
               "S C R I P T O R I U M" 'zoro-dashboard-subtitle)
              (insert "\n")
              (zoro-dashboard--insert-rule "⚚")
              (zoro-dashboard--insert-centered
               "KNOWLEDGE  ·  FREEDOM  ·  CRAFT" 'zoro-dashboard-tagline)
              (insert "\n")
              (dolist (action zoro-dashboard--actions)
                (apply #'zoro-dashboard--insert-action action))
              (insert "\n")
              (zoro-dashboard--insert-rule "⪻ ⚖ ⪼")
              (zoro-dashboard--insert-centered
               "Not to be served, but to serve." 'zoro-dashboard-tagline)
              (zoro-dashboard--insert-centered "—  GNU Emacs" 'shadow)
              (goto-char (point-min))
              (forward-button 1 t))))))))

(defvar-keymap zoro-dashboard-mode-map
  :parent special-mode-map
  "r" #'consult-recent-file
  "p" #'project-switch-project
  "a" #'consult-org-agenda
  "e" #'elfeed
  "n" #'consult-denote-find
  "b" #'consult-bookmark
  "q" #'save-buffers-kill-emacs
  "j" #'forward-button
  "k" #'backward-button
  "<down>" #'forward-button
  "<up>" #'backward-button
  "g" #'zoro-dashboard-render)

(define-derived-mode zoro-dashboard-mode special-mode "Scriptorium"
  "Major mode for the Scriptorium startup page."
  (setq-local cursor-type nil
              line-spacing 0.25
              truncate-lines t
              display-line-numbers nil)
  (face-remap-add-relative 'default 'variable-pitch))

(defun zoro-initial-dashboard-buffer ()
  "Return the stable initial dashboard buffer."
  (get-buffer-create "*dashboard*"))

(defun zoro-dashboard-load ()
  "Initialize and render the Scriptorium dashboard."
  (with-current-buffer (zoro-initial-dashboard-buffer)
    (zoro-dashboard-mode))
  (zoro-dashboard-render))

(defun zoro-open-dashboard ()
  "Open and refresh the Scriptorium dashboard."
  (interactive)
  (switch-to-buffer (zoro-initial-dashboard-buffer))
  (delete-other-windows)
  (zoro-dashboard-mode)
  (zoro-dashboard-render))

(setq initial-buffer-choice #'zoro-initial-dashboard-buffer)
(unless (keymapp (key-binding (kbd "C-z")))
  (global-set-key (kbd "C-z") (make-sparse-keymap)))
(global-set-key (kbd "C-z d") #'zoro-open-dashboard)
(add-hook 'window-size-change-functions #'zoro-dashboard-render 100)

(provide 'init-dashboard)
;;; init-dashboard.el ends here
