;;; init-ess.el --- Statistical programming tools -*- lexical-binding: t -*-

;; Author: zorowk
;; Copyright (C) 2019 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Configure ESS and Gnuplot support.

;;; Code:

(use-package ess
  :ensure t
  :defer t
  :commands R
  :config
  (load "ess-autoloads"))

(use-package gnuplot
  :ensure t
  :defer t)

(provide 'init-ess)
;;; init-ess.el ends here
