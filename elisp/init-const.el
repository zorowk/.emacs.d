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

;; Startup policy.
(defconst better-gc-cons-threshold (* 16 1024 1024)
  "GC threshold restored after initialization completes.")

(defconst zoro-startup-idle-tasks
  '((:name dashboard  :delay 0.10 :function zoro-dashboard-load)
    (:name marginalia :delay 0.45 :function marginalia-mode)
    (:name corfu       :delay 0.65 :function global-corfu-mode)
    (:name server      :delay 0.90 :function zoro-start-server)
    (:name shell-env   :delay 1.15 :function zoro-initialize-shell-environment)
    (:name clock       :delay 1.40 :function display-time-mode)
    (:name pulsar      :delay 1.60 :function zoro-enable-pulsar)
    (:name popper      :delay 1.80 :function zoro-enable-popper)
    (:name agenda      :delay 2.10 :function zoro-dashboard-enable-agenda)
    (:name savehist    :delay 2.45 :function savehist-mode)
    (:name pixel-scroll :delay 2.70 :function pixel-scroll-precision-mode)
    (:name which-key   :delay 2.95 :function which-key-mode)
    (:name so-long     :delay 3.20 :function global-so-long-mode))
  "Ordered one-shot tasks scheduled after the initial frame becomes idle.

Delays reflect measured cold-load costs and user-facing priority.  Dashboard
gets a 350ms window because package loading and rendering measured about 261ms;
the remaining tasks are staggered so their 6-14ms loads do not form one burst.")

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

(defconst zoro-dashboard-items-with-agenda
  '((recents . 7)
    (bookmarks . 7)
    (agenda . 5))
  "Dashboard items shown after deferred Agenda initialization.")

(provide 'init-const)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
