;;; init-vundo.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-vundo.el
;; Description: Initialize Vundo
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; VundoPac
(use-package vundo
  :ensure t
  :bind ("C-z u" . vundo))
;; -VundoPac

(provide 'init-vundo)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vundo.el ends here
