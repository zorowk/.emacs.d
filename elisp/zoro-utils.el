;;; zoro-utils.el --- Side-effect-free utility functions -*- lexical-binding: t -*-

;; Derived from M-EMACS configuration by Mingde (Matthew) Zeng.
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Define general helpers without installing hooks or changing global state.

;;; Code:

(defun where-am-i ()
  "Show and copy `buffer-file-name' or `buffer-name'."
  (interactive)
  (message (kill-new (if (buffer-file-name)
                         (buffer-file-name)
                       (buffer-name)))))

(provide 'zoro-utils)
;;; zoro-utils.el ends here
