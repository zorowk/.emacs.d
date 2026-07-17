;;; init-const.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-const.el
;; Description: Initialize Constants
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Mon Mar 18 14:20:54 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d constants
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes constants
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

;; Personal identity.
(setq user-full-name "zorowk"
      user-mail-address "nearkingzero@outlook.com")

;; Shared environment constants.
(defconst zoro-windows-p
  (eq system-type 'windows-nt)
  "Non-nil when Emacs is running on Windows.")

(defconst zoro-dropbox-directory
  (file-name-as-directory (expand-file-name "~/Dropbox/"))
  "Root directory of the personal Dropbox tree.")

(defconst zoro-org-directory
  (expand-file-name "brain/" zoro-dropbox-directory)
  "Directory containing Org agenda files.")

(defconst zoro-denote-directory
  (expand-file-name "notes/" zoro-dropbox-directory)
  "Directory containing Denote notes.")

(defconst zoro-hywiki-directory
  (expand-file-name "hywiki/" zoro-dropbox-directory)
  "Directory containing HyWiki pages.")

(provide 'init-const)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
