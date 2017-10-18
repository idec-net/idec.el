;;; idec-db.el --- This file is a part of GNU Emacs client for IDEC network

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
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'idec-parser)

(defun create-echo-mail-dir (echo)
    "Create ECHO directory inside `idec-mail-dir'."
    (if (file-exists-p idec-mail-dir)
            (message idec-mail-dir)
        (mkdir idec-mail-dir))
    (if (file-exists-p (concat idec-mail-dir "/" echo))
            (message (concat idec-mail-dir "/" echo))
        (mkdir (concat idec-mail-dir "/" echo))))

(defun get-echo-dir (echo)
    "Get ECHO dir from `idec-mail-dir'."
    (concat idec-mail-dir (concat "/" echo)))

(defun filename-to-store (content id)
    "Make filename from CONTENT unixtime and ID."
    (concat (nth 2 (split-string content)) "-" id))

(defun get-message-file (echo id)
    "Get ECHO message filename by ID."
    (concat (get-echo-dir echo) "/" id))

(defun get-counter-file (echo)
    "Get ECHO counter filename."
    (concat (get-echo-dir echo) "/counter"))

(defun store-message (content echo id)
    "Store CONTENT from ECHO message in `idec-mail-dir' with it ID."
    (create-echo-mail-dir echo)
    (write-region content nil (get-message-file echo id)))

(defun store-echo-counter (echo)
    "Store count messages in ECHO."
    (create-echo-mail-dir echo)
    (write-region (echo-messages-count echo) nil (get-counter-file echo)))

(defun check-message-in-echo (msg echo)
    "Check if exists message MSG in ECHO `idec-mail-dir'."
    (not (f-file? (get-message-file echo msg))))

(defun open-echo-db (echo)
    "Create or open sqlite database inside ECHO `idec-mail-dir'."
    (emacsql-sqlite (concat (get-echo-dir echo) "/db.sqlite3")))

(defun create-db-schema (echo)
    "Create db schema for ECHO."
    (emacsql (open-echo-db echo)
             [:create-table messages
                            ([(id :primary-key)
                              tags
                              author
                              to
                              echo
                              subj
                              (time :timestamp)
                              (body :text)
                              (unread integer :default 1)])
                            ])
    t)

(defun init-echo-db (echo)
    "Initialize new database for ECHO."
    (when (check-message-in-echo "db.sqlite3" echo)
        (and
         (create-db-schema echo)
         (message (concat "IDEC: Database for " echo " initialized.")))))

(defun insert-message-to-db (msg id &optional mark-read)
    "Insert MSG ID to echo db;
unread by default, but you can MARK-READ it."
    (if (not (emacsql (open-echo-db (get-message-field msg "echo"))
             [:insert :into messages
                      :values ([$s1 $s2 $s3 $s4 $s5 $s6 $s7 $s8 $s9])]
             id
             (get-message-field msg "tags")
             (get-message-field msg "author")
             (get-message-field msg "recipient")
             (get-message-field msg "echo")
             (get-message-field msg "subj")
             (get-message-field msg "time")
             (s-join "\n" (get-message-field msg "body"))
             mark-read))
            (message (concat "IDEC: Message " id " stored in db"))
        (message (concat "IDEC: Problem to store message " id))))

(defun check-message-in-db (msgid echo)
    "Check message MSGID in ECHO database."
    (if (not (emacsql (open-echo-db echo)
             [:select [id]
                      :from messages
                      :where (= id $s1)]
             msgid))
            t
        nil))

(provide 'idec-db)

;;; idec-db.el ends here
