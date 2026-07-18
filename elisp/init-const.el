;;; init-const.el --- Shared configuration constants -*- lexical-binding: t -*-

;; Derived from M-EMACS configuration by Mingde (Matthew) Zeng.
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Keep only values shared by multiple feature modules or consumed as data.
;; One-off settings belong at their point of use so their purpose stays clear.

;;; Code:

;; Startup policy.
(defconst zoro-startup-idle-tasks
  '((:name dashboard  :delay 0.10 :function zoro-dashboard-load)
    (:name marginalia :delay 0.45 :function marginalia-mode :arguments (1))
    (:name corfu       :delay 0.65 :function global-corfu-mode :arguments (1))
    (:name server      :delay 0.90 :function zoro-start-server)
    (:name shell-env   :delay 1.15 :function zoro-initialize-shell-environment)
    (:name clock       :delay 1.40 :function display-time-mode :arguments (1))
    (:name pulsar      :delay 1.60 :function pulsar-global-mode :arguments (1))
    (:name popper      :delay 1.80 :function zoro-enable-popper)
    (:name agenda      :delay 2.10 :function zoro-dashboard-enable-agenda)
    (:name savehist    :delay 2.45 :function savehist-mode :arguments (1))
    (:name pixel-scroll :delay 2.70 :function pixel-scroll-precision-mode
     :arguments (1))
    (:name which-key   :delay 2.95 :function which-key-mode :arguments (1))
    (:name so-long     :delay 3.20 :function global-so-long-mode :arguments (1)))
  "Ordered one-shot tasks scheduled after the initial frame becomes idle.

Delays reflect measured cold-load costs and user-facing priority.  Dashboard
gets a 350ms window because package loading and rendering measured about 261ms;
the remaining tasks are staggered so their 6-14ms loads do not form one burst.
Each optional :arguments list is passed unchanged to the task function.")

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
