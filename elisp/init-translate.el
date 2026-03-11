;; Description: Initialize Translate
;; Author: WenHao Peng
;; Copyright (C) 2026
;; Created: Tue Sep  3 21:28:26 2019 (-0400)
;; Version: 3.0
;; Keywords: M-EMACS .emacs.d translate
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initialies translate
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
(eval-when-compile
  (require 'init-global-config))

;; google-translate
(use-package google-translate
  :defer t
  :bind (("C-c t" . google-translate-at-point)
         ("C-c T" . google-translate-query-translate))
  :custom
  (google-translate-default-source-language "en")
  (google-translate-default-target-language "zh")
  (google-translate-enable-ido-completion t)
  (google-translate-output-destination 'echo-area))
;; -google-translate

(provide 'init-translate)
