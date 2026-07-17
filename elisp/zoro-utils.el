;;; zoro-utils.el --- Side-effect-free utility functions -*- lexical-binding: t -*-

;; Derived from M-EMACS configuration by Mingde (Matthew) Zeng.
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Define general helpers without installing hooks or changing global state.

;;; Code:

(defun display-line-overlay+ (pos str &optional face)
  "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
  (let ((ol (save-excursion
              (goto-char pos)
              (make-overlay (line-beginning-position)
                            (line-end-position)))))
    (overlay-put ol 'display str)
    (overlay-put ol 'face
                 (or face '(:background null :inherit highlight)))
    ol))

(defun read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer (insert-file-contents file-path)
                    (split-string (buffer-string) "\n" t)))

(defun where-am-i ()
  "Show and copy `buffer-file-name' or `buffer-name'."
  (interactive)
  (message (kill-new (if (buffer-file-name)
                         (buffer-file-name)
                       (buffer-name)))))

(provide 'zoro-utils)
;;; zoro-utils.el ends here
