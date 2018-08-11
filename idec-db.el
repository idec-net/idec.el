;;; idec-db.el --- This file is a part of client for IDEC network

;; Copyright (c) 2017 Denis Zheleztsov

;; Author: Denis Zheleztsov <difrex.punk@gmail.com>
;; Keywords: lisp,network,IDEC
;; Version: 0.1
;; Homepage: https://github.com/idec-net/idec.el

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
                              address
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
                                   :values ([$s1 $s2 $s3 $s4 $s5 $s6 $s7 $s8 $s9 $s10 $s11])]
                          id
                          (get-message-field msg "tags")
                          (get-message-field msg "author")
                          (get-message-field msg "address")
                          (get-message-field msg "recipient")
                          repto
                          (get-message-field msg "echo")
                          (get-message-field msg "subj")
                          (s-join "\n" (get-message-field msg "body"))
                          (get-message-field msg "time")
                          mark-read))
                    (message (concat "IDEC: Message " id " stored in db"))
                (message (concat "IDEC: Problem to store message " id))))
        ))

(defun total-db-messages (echo)
    "Return count of local ECHO messages."
    (length (emacsql (open-echo-db echo)
                     [:select [id]
                      :from messages])))

(defun make-hash-from-msg-list (msg-list)
    "Return hash table maded from MSG-LIST."
    (let (msg-hash)
        (setq msg-hash (make-hash-table :test 'equal))
        (if (and
             (listp msg-list)
             (> (length msg-list) 0))
                (let ()
                    (puthash "id" (nth 0 msg-list) msg-hash)
                    (puthash "tags" (nth 1 msg-list) msg-hash)
                    (puthash "author" (nth 2 msg-list) msg-hash)
                    (puthash "address" (nth 3 msg-list) msg-hash)
                    (puthash "recipient" (nth 4 msg-list) msg-hash)
                    (puthash "repto" (nth 5 msg-list) msg-hash)
                    (puthash "echo" (nth 6 msg-list) msg-hash)
                    (puthash "subj" (nth 7 msg-list) msg-hash)
                    (puthash "body" (nth 8 msg-list) msg-hash)
                    (puthash "time" (nth 9 msg-list) msg-hash)
                    (puthash "unread" (nth 10 msg-list) msg-hash)))
        msg-hash))

(defun get-echo-messages (echo)
    "Get ECHO messages ordered by time."
    (let (msgs)
        (setq msgs (make-list 0 (make-hash-table :test 'equal)))
        (dolist (l (if idec-desc-local-echo-sort
                           (and (message "Desc select")
                                (idec-db-desc-echo-select echo))
                       (and (message "Normal select")
                            (idec-db-normal-echo-select echo))))
            (if (> (length l) 0)
                    (setq msgs (append msgs (make-list 1 (make-hash-from-msg-list l))))))
        msgs))

(defun idec-db-normal-echo-select (echo)
    "Select from ECHO in normal order."
    (emacsql (open-echo-db echo)
             [:select [id, tags, author, address, recipient, repto, echo, subj, body, time, unread]
                      :from messages
                      :order-by rowid]))

(defun idec-db-desc-echo-select (echo)
    "Select from ECHO in desc order."
    (emacsql (open-echo-db echo)
             [:select [id, tags, author, address, recipient, repto, echo, subj, body, time, unread]
                      :from messages
                      :order-by rowid
                      :desc]))

(defun get-echo-subjects (echo)
    "Get list of subjects from ECHO."
    (let (subjects)
        (setq subjects (make-list 0 ""))
        (dolist (l (emacsql (open-echo-db echo)
                            [:select [subj]
                                     :from messages]))
            (setq subjects (append subjects (make-list 1 (nth 0 l)))))
        subjects))

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

(defun idec-db-get-message-by-id (msgid &optional echo)
    "Retrieve message by MSGID from ECHO database."
    (make-hash-from-msg-list (car (emacsql (open-echo-db echo)
             [:select [id, tags, author, address, recipient, repto, echo, subj, body, time, unread]
                      :from messages
                      :where (= id $s1)]
             msgid))))

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
    (message (concat "Mark read message " msgid " in " echo))
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
