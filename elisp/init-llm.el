;;; init-llm.el ---
;;
;; Filename: init-llm.el
;; Description:
;; Author: Mingde (Matthew) Zeng
;; Maintainer:
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Jan  9 13:07:47 2025 (-0500)
;; Version:
;; Package-Requires: ()
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
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

;; agent shell
(use-package agent-shell
  :defer t
  :bind (("C-z a" . agent-shell))
  :config
  (setq agent-shell-markdown-render-function #'agent-shell-markdown-replace-markup)
  (setq agent-shell-highlight-blocks t))
;; -agent shell

;; agent recall
(use-package agent-recall
  :ensure t
  :hook (agent-shell-mode . agent-recall-track-sessions)
  :config
  (setq agent-recall-search-paths '("~/Documents/code" "~/Downloads/work")
        agent-recall-search-function 'consult-ripgrep
        agent-recall-browse-sort 'modified-desc))
;; -agent recall

(provide 'init-llm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-llm.el ends here
