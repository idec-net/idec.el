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

(defun get-local-echoes ()
    "Get local downloaded echoes from `idec-mail-dir'."
    (delete '".." (delete '"." (directory-files idec-mail-dir nil "\\w*\\.\\w*"))))

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
                              recipient
                              repto
                              echo
                              subj
                              (body :text)
                              (time :timestamp)
                              (unread integer :default 1)])])
    (emacsql (open-echo-db echo)
             [:create-table outbox
                            ([
                              (time :timestamp)
                              recipient
                              subj
                              (body :text)
                              ])])
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
    (when (check-message-in-db id (get-message-field msg "echo"))
        (when (not mark-read)
            (setq mark-read 1))
        (let (repto)
            (setq repto "")
            ;; Check repto tag
            (if (string-match "repto" (get-message-field msg "tags"))
                    (setq repto (nth 3 (split-string (get-message-field msg "tags") "/"))))

            (if (not (emacsql (open-echo-db (get-message-field msg "echo"))
                          [:insert :into messages
                                   :values ([$s1 $s2 $s3 $s4 $s5 $s6 $s7 $s8 $s9 $s10])]
                          id
                          (get-message-field msg "tags")
                          (get-message-field msg "author")
                          (get-message-field msg "recipient")
                          repto
                          (get-message-field msg "echo")
                          (get-message-field msg "subj")
                          (get-message-field msg "time")
                          (s-join "\n" (get-message-field msg "body"))
                          mark-read))
                    (message (concat "IDEC: Message " id " stored in db"))
                (message (concat "IDEC: Problem to store message " id))))
        ))

(defun total-db-messages (echo)
    "Return count of local ECHO messages."
    (length (emacsql (open-echo-db echo)
                     [:select [id]
                      :from messages])))

(defun get-echo-unread-messages (echo)
    "Get count of unread messages from ECHO database."
    (car (car (emacsql (open-echo-db echo)
             [:select (funcall count id)
              :from messages
              :where (= unread 1)]))))

(defun get-echo-messages-count (echo)
    "Get count of all messages in ECHO."
    (car (car (emacsql (open-echo-db echo)
             [:select (funcall count id)
              :from messages]))))

(defun get-message-from-db (msgid echo)
    "Retrieve message by MSGID from ECHO database."
    )

(defun delete-message-from-db (msgid echo)
    "Delete message by MSGID from ECHO database."
    (when (not
         (emacsql (open-echo-db echo) [:delete :from messages :where (= id $s1)] msgid))
        (message (concat "IDEC: Message " msgid " deleted"))))

(defun check-message-in-db (msgid echo)
    "Check message MSGID in ECHO database."
    (init-echo-db echo)
    (if (not (emacsql (open-echo-db echo)
             [:select [id]
              :from messages
              :where (= id $s1)]
             msgid))
            t
        nil))

(defun mark-message-read (msgid echo)
    "Mark message MSGID as read in ECHO database."
    (emacsql (open-echo-db echo)
             [:update messages
              :set (= unread 0)
              :where (= id $s1)]
             msgid))

(defun mark-all-messages-as-read (echo)
    "Mark all messages in ECHO as read."
    (message (concat "IDEC: RECEIVE ECHO: " echo))
    (if (not (string= "" echo))
            (emacsql (open-echo-db echo)
                     [:update messages :set (= unread 0)])
        (message "IDEC: empty echo receive in `mark-all-messages-as-read'")))

(provide 'idec-db)

;;; idec-db.el ends here
