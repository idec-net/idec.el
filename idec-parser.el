;;; idec-parser.el --- This file part of GNU Emacs client for IDEC network

;; Copyright (c) 2017 Denis Zheleztsov

;; Author: Denis Zheleztsov <difrex.punk@gmail.com>
;; Keywords: lisp,network,IDEC
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; In active developent.
;; Fetched node must be support modern IDEC extensions like /list.txt, /x/c, etc.

;;; Code:

;; Message fields pasing
(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING;
White space here is any of: space, tab, Emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))

(defun get-message-tags (msg)
    "Get MSG tags."
    ;; (trim-string
    (nth 0 (split-string msg "\n")))

(defun get-message-echo (msg)
    "Get MSG echo."
    ;; (trim-string
    (nth 1 (split-string msg "\n")))

(defun get-message-time (msg)
    "Get MSG time."
    ;; ;; (trim-string
     (current-time-string
     (car (read-from-string (nth 2 (split-string msg "\n"))))))

(defun get-message-author (msg)
    "Get MSG author."
    ;; ;; (trim-string
     (nth 3 (split-string msg "\n")))

(defun get-message-address (msg)
    "Get MSG address."
    ;; (trim-string
    (nth 4 (split-string msg "\n")))

(defun get-message-recipient (msg)
    "Get MSG recipient."
    ;; (trim-string
    (nth 5 (split-string msg "\n")))

(defun get-message-subj (msg)
    "Get MSG subject."
    ;; (trim-string
    (nth 6 (split-string msg "\n")))

(defun get-message-body (msg)
    "Get MSG body text.
Return list with body content."
    (-drop 8 (split-string msg "\n")))

(defun get-longest-field (field msg-list)
    "Return longest FIELD in MSG-LIST."
    (defvar field-legth '())
    (defvar field-max nil)
    (setq field-max 0)
    (maphash (lambda (id msg)
        (when (> (length (get-message-field msg field))
                 field-max)
            (setq field-max (length (get-message-field msg field)))))
             msg-list)
    field-max)

(defun get-message-field (msg field)
    "Get message MSG FIELD."
    (defvar fields-hash (make-hash-table :test 'equal)
        "Hashtable with MSG parsing functions.")

    ;; Define hashtable first
    (puthash "tags" (get-message-tags msg) fields-hash)
    (puthash "echo" (get-message-echo msg) fields-hash)
    (puthash "time" (get-message-time msg) fields-hash)
    (puthash "author" (get-message-author msg) fields-hash)
    (puthash "address" (get-message-address msg) fields-hash)
    (puthash "recipient" (get-message-recipient msg) fields-hash)
    (puthash "subj" (get-message-subj msg) fields-hash)
    (puthash "body" (get-message-body msg) fields-hash)
    (gethash field fields-hash))

(provide 'idec-parser)

;;; idec-parser.el ends here
