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
;; Compatibility: Emacs 31
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

;; Personal identity and account paths.
(defconst zoro-primary-mail-address "nearkingzero@outlook.com"
  "Primary email address used by Emacs.")

(defconst zoro-gmail-address "near.kingzero@gmail.com"
  "Gmail address used by the Gnus account configuration.")

(defconst zoro-auth-source-file
  (expand-file-name "~/.authinfo.json.gpg")
  "Encrypted auth-source file used by mail accounts.")

(setq user-full-name "zorowk"
      user-mail-address zoro-primary-mail-address)

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
