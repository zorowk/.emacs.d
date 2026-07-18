;;; init-buffer.el --- Buffer list configuration -*- lexical-binding: t -*-

;; Author: zorowk
;; Copyright (C) 2019 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure the built-in Ibuffer interface.

;;; Code:

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-human-readable-size t)
  (ibuffer-use-header-line 'title)
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 35 35 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))))

(provide 'init-buffer)
;;; init-buffer.el ends here
