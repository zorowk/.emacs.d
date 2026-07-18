;;; benchmark-startup.el --- Report synchronous startup costs -*- lexical-binding: t -*-

;; Author: zorowk
;; Copyright (C) 2026 zorowk
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Load this after early-init.el and before package activation and init.el.
;; The report intentionally excludes idle timers and other deferred work.

;;; Code:

(require 'subr-x)

(defvar zoro-startup-benchmark-start (current-time)
  "Time at which the startup benchmark began.")

(defvar zoro-startup-benchmark-gcs gcs-done
  "Number of completed garbage collections before the benchmark.")

(defvar zoro-startup-benchmark-events nil
  "Chronological startup events collected by the benchmark.")

(defun zoro-startup-benchmark--elapsed-ms ()
  "Return milliseconds elapsed since the benchmark began."
  (* 1000 (float-time
           (time-subtract (current-time) zoro-startup-benchmark-start))))

(defun zoro-startup-benchmark--require (original feature &rest arguments)
  "Time first loads of init FEATURE around ORIGINAL with ARGUMENTS."
  (let ((record (and (symbolp feature)
                     (not (featurep feature))
                     (string-prefix-p "init-" (symbol-name feature))))
        (start (current-time)))
    (prog1 (apply original feature arguments)
      (when record
        (push (list feature
                    (* 1000 (float-time
                             (time-subtract (current-time) start)))
                    (zoro-startup-benchmark--elapsed-ms))
              zoro-startup-benchmark-events)))))

(advice-add 'require :around #'zoro-startup-benchmark--require)

(defun zoro-startup-benchmark-activate-packages ()
  "Activate installed packages and record the operation's duration."
  (let ((start (current-time)))
    (package-activate-all)
    (push (list 'package-activation
                (* 1000 (float-time
                         (time-subtract (current-time) start)))
                (zoro-startup-benchmark--elapsed-ms))
          zoro-startup-benchmark-events)))

(defun zoro-startup-benchmark-report ()
  "Print the collected synchronous startup timing report."
  (advice-remove 'require #'zoro-startup-benchmark--require)
  (princ (format "%-24s %10s %12s\n" "Event" "Duration" "Elapsed"))
  (princ (make-string 48 ?-))
  (princ "\n")
  (dolist (event (nreverse zoro-startup-benchmark-events))
    (pcase-let ((`(,name ,duration ,elapsed) event))
      (princ (format "%-24s %8.2fms %10.2fms\n"
                     name duration elapsed))))
  (princ (format "\nTotal: %.2fms; GC runs: %d\n"
                 (zoro-startup-benchmark--elapsed-ms)
                 (- gcs-done zoro-startup-benchmark-gcs))))

(provide 'benchmark-startup)
;;; benchmark-startup.el ends here
