;;; init-search.el --- Minibuffer completion and search -*- lexical-binding: t -*-

;; Author: zorowk
;; Copyright (C) 2019 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Vertico renders candidates, Orderless filters them, and Marginalia annotates
;; them.  Consult provides search/navigation commands; Embark acts on or exports
;; the current candidate.  In-buffer completion lives in `init-complete.el'.

;;; Code:

;; Vertico displays standard completing-read candidates vertically.
(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("?" . minibuffer-completion-help)
              ("M-RET" . minibuffer-force-complete-and-exit)
              ("M-TAB" . minibuffer-complete))
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 20)
  (vertico-resize t)
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Savehist persists minibuffer history; Vertico uses it when sorting candidates.
(use-package savehist
  :ensure nil
  :init
  (unless noninteractive
    (savehist-mode 1)))

;; Built-in minibuffer behavior shared by Vertico and other completion commands.
(use-package emacs
  :ensure nil
  :custom
  ;; Enable Emacs's general right-click context menu (independent of Vertico).
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  ;; Never pop up *Completions* over Vertico; keep it live when opened manually.
  (completion-eager-display nil)
  (completion-eager-update t)
  (minibuffer-visible-completions 'up-down)
  ;; Hide commands from M-x when they cannot run in the current context.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Keep point out of the read-only prompt text.
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; Orderless treats space-separated input components as independent filters.
(use-package orderless
  :ensure t
  :custom
  ;; `basic' is the fallback required by some dynamic completion tables.
  (completion-styles '(orderless basic))
  ;; File completion keeps partial paths and wildcard support.  Eglot candidates
  ;; use Orderless instead of Eglot's built-in flex style.
  (completion-category-overrides '((file (styles partial-completion))
                                   (eglot-capf (styles orderless basic))))
  ;; On Emacs 31+, partial completion can also match inside path components.
  (completion-pcm-leading-wildcard t))

;; Marginalia adds category-aware annotations such as file size or command keys.
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (unless noninteractive
    (marginalia-mode 1)))

;; Consult supplies completing-read based navigation and asynchronous search.
(use-package consult
  :ensure t
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x M-:" . consult-complex-command)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x t b" . consult-buffer-other-tab)
         ("C-x r b" . consult-bookmark)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)
         ("C-M-#" . consult-register)
         ("M-y" . consult-yank-pop)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s d" . consult-find)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         :map minibuffer-local-map
         ("M-s" . consult-history)
         ("M-r" . consult-history))
  :custom
  ;; Press < followed by a source key to restrict grouped candidates.
  (consult-narrow-key "<")
  :init
  ;; Improve the built-in register preview and use Consult for xref selection.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5
        xref-search-program 'ripgrep
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; Expensive previews wait briefly; theme preview uses a shorter delay.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   :preview-key '(:debounce 0.4 any)))

;; Embark exposes context-sensitive actions for the candidate or thing at point.
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)
         ("C-h B" . embark-bindings))
  :init
  ;; C-h after any prefix opens a searchable list of the remaining bindings.
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Add Consult preview support to Embark collect buffers.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-search)
;;; init-search.el ends here
