;;; init-shell.el --- Login-shell environment -*- lexical-binding: t -*-

;; Author: zorowk
;; Copyright (C) 2019 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Import environment variables for graphical macOS sessions.

;;; Code:

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :commands exec-path-from-shell-initialize
  :custom
  (exec-path-from-shell-variables
   '("PATH" "MANPATH" "LANG" "LC_ALL")))

(provide 'init-shell)
;;; init-shell.el ends here
