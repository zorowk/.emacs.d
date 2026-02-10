;;; init-shrface.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-shrface.el
;; Description: Initialize shrface
;; Author: zorowk
;; Copyright (C) 2021 zorowk
;; Created: Tue Feb 2 13:28:34 2021 (-0400)
;; Version: 3.0
;; URL: https://github.com/zorowk/.emacs.d.git
;; Keywords: M-EMACS .emacs.d shrface
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes shrface
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

;; eldoc-box
(use-package eldoc-box
  :straight t
  :defer t
  :custom
  (eldoc-box-max-pixel-width 600)   ; 最大宽度（可调，防止太宽）
  (eldoc-box-max-pixel-height 400)  ; 最大高度
  (eldoc-box-frame-position 'at-point)  ; 显示在光标位置附近
  (eldoc-box-only-multi-line t))

(use-package shr-tag-pre-highlight
  :ensure t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions))))

;; Olivetti
(use-package olivetti :defer t)
;; -Olivetti

(use-package shrface
  :defer t
  :config

  (defvar shrface-general-rendering-functions
    (append '((title . eww-tag-title)
              (form . eww-tag-form)
              (input . eww-tag-input)
              (button . eww-form-submit)
              (textarea . eww-tag-textarea)
              (select . eww-tag-select)
              (link . eww-tag-link)
              (meta . eww-tag-meta)
              (code . shrface-tag-code)
              (pre . shrface-shr-tag-pre-highlight))
            shrface-supported-faces-alist))

  (defvar shrface-nov-rendering-functions
    (append '((img . nov-render-img)
              (svg . nov-render-svg)
              (title . nov-render-title)
              (pre . shrface-shr-tag-pre-highlight)
              (code . shrface-tag-code)
              (form . eww-tag-form)
              (input . eww-tag-input)
              (button . eww-form-submit)
              (textarea . eww-tag-textarea)
              (select . eww-tag-select)
              (link . eww-tag-link)
              (meta . eww-tag-meta))
            shrface-supported-faces-alist))

  (setq shr-cookie-policy nil)
  (add-hook 'outline-view-change-hook 'shrface-outline-visibility-changed)
  (require 'shr-tag-pre-highlight)
  (setq shr-tag-pre-highlight-lang-modes
        '(("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist)
          ("asymptote" . asy) ("dot" . fundamental) ("sqlite" . sql)
          ("calc" . fundamental) ("C" . c) ("cpp" . c++) ("C++" . c++)
          ("screen" . shell-script) ("shell" . sh) ("bash" . sh)
          ("rust" . rustic)
          ("rust" . rustic)
          ("awk" . bash)
          ("json" . "js")
          ;; Used by language-detection.el
          ("emacslisp" . emacs-lisp)
          ;; Used by Google Code Prettify
          ("el" . emacs-lisp))))

(use-package eww
  :straight (:type built-in)
  :defer t
  :config
  (require 'shrface)
  (advice-add 'eww-display-html :around #'shrface-render-advice)
  (add-hook 'eww-after-render-hook #'olivetti-mode)
  (add-hook 'eww-after-render-hook #'eldoc-mode)
  (add-hook 'eww-after-render-hook #'eldoc-box-hover-mode)
  (add-hook 'eww-after-render-hook #'shrface-eww-setup))

(use-package nov
  :defer t
  :config
  (add-hook 'nov-mode-hook #'eldoc-mode)
  (add-hook 'nov-mode-hook #'olivetti-mode)
  (add-hook 'nov-mode-hook #'eldoc-box-hover-mode)
  (add-hook 'nov-mode-hook #'shrface-nov-setup)
  (require 'shrface)
  (advice-add 'shr--remove-blank-lines-at-the-end :override #'shrface-remove-blank-lines-at-the-end))

(defun shrface-eww-setup ()
  (unless shrface-toggle-bullets
    (shrface-regexp)
    (setq-local imenu-create-index-function #'shrface-imenu-get-tree))
  ;; workaround to show annotations in eww
  (shrface-show-all-annotations))

(defun shrface-shr-tag-pre-highlight (pre)
  "Highlighting code in PRE."
  (let* ((shr-folding-mode 'none)
         (shr-current-font 'default)
         (code (with-temp-buffer
                 (shr-generic pre)
                 ;; (indent-rigidly (point-min) (point-max) 2)
                 (buffer-string)))
         (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                   (let ((sym (language-detection-string code)))
                     (and sym (symbol-name sym)))))
         (mode (and lang
                    (shr-tag-pre-highlight--get-lang-mode lang))))
    (shr-ensure-newline)
    (shr-ensure-newline)
    (setq start (point))
    (insert
     ;; (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
     (or (and (fboundp mode)
              (with-demoted-errors "Error while fontifying: %S"
                (shr-tag-pre-highlight-fontify code mode)))
         code)
     ;; (propertize "#+END_SRC" 'face 'org-block-end-line )
     )
    (shr-ensure-newline)
    (setq end (point))
    (pcase (frame-parameter nil 'background-mode)
      ('light
       (add-face-text-property start end '(:background "#D8DEE9" :extend t)))
      ('dark
       (add-face-text-property start end '(:background "#292b2e" :extend t))))
    (shr-ensure-newline)
    (insert "\n")))

(defun shrface-remove-blank-lines-at-the-end (start end)
  "A fix for `shr--remove-blank-lines-at-the-end' which will remove image at the end of the document."
  (save-restriction
    (save-excursion
      (narrow-to-region start end)
      (goto-char end)
      (when (and (re-search-backward "[^ \n]" nil t)
                 (not (eobp)))
        (forward-line 1)
        (delete-region (point) (min (1+ (point)) (point-max)))))))

(defun shrface-nov-setup ()
  (unless shrface-toggle-bullets
    (shrface-regexp))
  (set-visited-file-name nil t))

(defun shrface-render-advice (orig-fun &rest args)
  (require 'eww)
  (let ((shrface-org nil)
        (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
        (shr-table-vertical-line "|")
        (shr-width 65)
        (shr-indentation 0)
        (shr-external-rendering-functions shrface-general-rendering-functions)
        (shrface-toggle-bullets nil)
        (shrface-href-versatile t)
        (shr-use-fonts nil))
    ;; workaround, need a delay to update the header line
    (run-with-timer 0.01 nil 'shrface-update-header-line)
    (apply orig-fun args)))

(defun shrface-show-all-annotations()
  (when (bound-and-true-p paw-annotation-mode)
    (paw-clear-annotation-overlay)
    (paw-show-all-annotations)
    (if paw-annotation-show-wordlists-words-p
        (paw-focus-find-words :wordlist t))
    (if paw-annotation-show-unknown-words-p
        (paw-focus-find-words))))

(provide 'init-shrface)
