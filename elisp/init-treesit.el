;;; init-treesit.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-treesit.el
;; Description: Initialize Parenthesis
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:17:13 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d parenthesis smartparens delete-block
;; Compatibility: emacs-version >= 31
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes tree-sitter
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

(when (treesit-available-p)
  (setopt treesit-enabled-modes
          '(c-ts-mode
            c++-ts-mode
            css-ts-mode
            java-ts-mode
            js-ts-mode
            json-ts-mode
            python-ts-mode
            rust-ts-mode
            typescript-ts-mode
            yaml-ts-mode))
  (setopt treesit-auto-install-grammar 'ask)
  (setopt treesit-extra-load-path
          (list (expand-file-name "tree-sitter" user-emacs-directory)))

  (defun treesit-show-parser-used-at-point ()
    "Show the Tree-sitter parser used at point."
    (interactive)
    (if-let* ((lang (treesit-language-at (point))))
        (message "%s" lang)
      (message "treesit is not available"))))

(let ((qml-ts-dir (expand-file-name "straight/repos/qml-ts-mode"
                                    user-emacs-directory)))
  (when (file-directory-p qml-ts-dir)
    (add-to-list 'load-path qml-ts-dir)))
(when (locate-library "qml-ts-mode")
  (require 'qml-ts-mode))

(provide 'init-treesit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treesit.el ends here
