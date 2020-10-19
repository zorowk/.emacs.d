;;; init-orgbrain.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-orgbrain.el
;; Description: Initialize journal and brain
;; Author: zorkwk
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 08:40:27 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d org brain
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes brain and journal
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

;; org journal
(use-package org-journal
  :ensure t
  :after org
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir "~/Dropbox/journal/"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-file-type 'weekly)

  (defun org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything \n\n")
       (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded \n\n")
       (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded \n\n")
       (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded \n\n"))))
  (setq org-journal-file-header 'org-journal-file-header-func)
)
;; -org journal

(provide 'init-orgbrain)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-orgbrain.el ends here
