;;; init-const.el --- Shared configuration constants -*- lexical-binding: t -*-

;; Derived from M-EMACS configuration by zorowk.
;; Copyright (C) 2019 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Keep only values shared by multiple feature modules or consumed as data.
;; One-off settings belong at their point of use so their purpose stays clear.

;;; Code:

;; Personal identity and account paths.
(defconst zoro-gmail-address "near.kingzero@gmail.com"
  "Gmail address used by the Gnus account configuration.")

(defconst zoro-auth-source-file
  (expand-file-name "~/.authinfo.json.gpg")
  "Encrypted auth-source file used by mail accounts.")

(setq user-full-name "zorowk"
      user-mail-address "nearkingzero@outlook.com")

;; Shared environment constants.
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
;;; init-const.el ends here
