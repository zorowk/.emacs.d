;;; init-calendar.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-calendar.el
;; Description: Initialize calendar
;; Author: zorowk
;; Copyright (C) 2019 zorowk
;; Created: Thu Nov 27 15:47:34 2019 (-0400)
;; Version: 3.0
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d calendar
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes calendar
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

;; cal-china-x
(use-package cal-china-x
  :config
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq cal-china-x-general-holidays '((holiday-lunar 1 15 "元宵节")))
  (setq calendar-holidays
        (append cal-china-x-important-holidays
                cal-china-x-general-holidays)))

(setq cal-html-directory "~/Dropbox/calendar")
(setq diary-file "~/Dropbox/calendar/diary")
(setq diary-mail-addr "near.kingzero@gmail.com")

;;appointment
(setq appt-issue-message t)

;; 设置所在地的经纬度和地名，calendar 中按 S，可以根据这些信息告知你每天的
;; 日出和日落的时间：
(setq calendar-latitude +30.52)
(setq calendar-longitude +114.31)
(setq calendar-location-name "武汉")

(setq calendar-remove-frame-by-deleting t)
(setq calendar-week-start-day 1)              ; 每周第一天是周一
(setq mark-diary-entries-in-calendar t)       ; 标记有记录的日子
(setq mark-holidays-in-calendar nil)          ; 标记节假日
(setq view-calendar-holidays-initially nil)   ; 不显示节日列表

;;除去基督徒的节日、希伯来人的节日和伊斯兰教的节日。
;;(setq christian-holidays nil
;;      hebrew-holidays nil
;;      islamic-holidays nil
;;      solar-holidays nil
;;      bahai-holidays nil
;;      )

(setq mark-diary-entries-in-calendar t
      appt-issue-message nil
      mark-holidays-in-calendar t
      view-calendar-holidays-initially nil)

(setq diary-date-forms '((year "/" month "/" day "[^/0-9]"))
      calendar-date-display-form '(year "/" month "/" day)
      calendar-time-display-form
      '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")))
;; -cal-china-x

(provide 'init-calendar)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-calendar.el ends here
