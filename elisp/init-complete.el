;;; init-complete.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-complete.el
;; Description: Initialize in-buffer completion and LSP
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:02:00 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d corfu cape eglot
;; Compatibility: emacs-version >= 30
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes in-buffer completion with Corfu and Cape, plus Eglot.
;; Minibuffer completion and search commands live in `init-search.el'.
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

;; Eglot supplies LSP completion, navigation, diagnostics, and code actions.
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  (eglot-events-buffer-config '(:size 0 :format short))
  (eglot-ignored-server-capabilities
   '(:inlayHintProvider
     :documentHighlightProvider
     :foldingRangeProvider))
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c-ts-mode c++-mode c++-ts-mode) .
                 ("clangd"
                  "--background-index"
                  "--clang-tidy"
                  "--header-insertion=never"
                  "--limit-results=15"
                  "--pch-storage=memory")))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer")))
  (add-to-list 'eglot-server-programs
               '((latex-mode LaTeX-mode) . ("texlab")))
  (add-to-list 'eglot-server-programs
               '((web-mode js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-mode) .
                 ("typescript-language-server" "--stdio")))
  :bind (:map eglot-mode-map
              ("M-." . xref-find-definitions)
              ("M-," . xref-go-back)
              ("M-/" . eglot-find-implementation)
              ("M-i" . eglot-find-declaration)
              ("C-c l r" . xref-find-references)
              ("C-c l a" . eglot-code-actions)
              ("C-c l f" . eglot-format-buffer))
  :hook ((c-mode c-ts-mode c++-mode c++-ts-mode
                 python-mode python-ts-mode
                 rust-mode rust-ts-mode
                 latex-mode LaTeX-mode
                 web-mode js-mode js-ts-mode
                 typescript-mode typescript-ts-mode tsx-mode)
         . eglot-ensure))

;; Query workspace symbols through Consult's completing-read interface.
(use-package consult-eglot
  :ensure t
  :after eglot
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)
              ("C-c f" . consult-eglot-symbols)))

;; Corfu renders completion-at-point candidates in a child-frame popup.
(use-package corfu
  :ensure t
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :custom
  (global-corfu-minibuffer
   (lambda ()
     ;; Let Vertico own completion minibuffers, and never complete passwords.
     (not (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input)
              (eq (current-local-map) read-passwd-map)))))
  (corfu-auto t)
  (corfu-cycle t)
  :init
  (global-corfu-mode)
  :config
  ;; Reuse previous candidates and show documentation in a second popup.
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; TAB first indents, then invokes completion when indentation is unchanged.
(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  ;; Do not let the Emacs 30 Ispell CAPF take precedence in text buffers.
  (text-mode-ispell-word-completion nil))

;; Dabbrev completes words gathered from related buffers.
(use-package dabbrev
  :ensure nil
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; Cape contributes additional completion-at-point functions (Capfs).
(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Offer file names after any earlier, mode-specific Capfs decline.
  (add-hook 'completion-at-point-functions #'cape-file)
  :config
  ;; Let `cape-file' run when Eglot returns no matching candidates.
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive))

(provide 'init-complete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-complete.el ends here
