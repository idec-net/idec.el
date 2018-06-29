;;; idec.el --- GNU Emacs client for IDEC network

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

;; (require 'idec-answers)
(require 'idec-parser)
;; (require 'idec-online)
(require 'idec-db)

;; FUNCTIONS
;; ;;;;;;;;;

(defun get-url-content (url)
    "Get URL content and return it without headers."
    (with-current-buffer
            (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "^$")
        (forward-line)
        (delete-region (point) (point-min))
        (buffer-string)))

;; LOCAL MAIL FUNCTIONS
;; ;;;;;;;;;;;;;;;;;;;;

(defun dots (echo)
    "Get dots from ECHO list."
    (make-string
     (+ (- (get-longest-string (get-local-echoes))
           (length echo))
        3)
     ? ))                               ; `? ' - Space character

(defun longest-local-echo-subj (echo)
    "Get longest subj from local ECHO."
    (get-longest-string (get-echo-subjects echo)))

(defun mark-all-as-read (echo &optional checkpoint)
    "Mark all messages in ECHO as read;
put cursor to CHECKPOINT."
    (mark-all-messages-as-read echo)
    (idec-local-browse checkpoint))

;; END OF LOCAL MAIL FUNCTIONS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-plain-text-message-from-hash(msg)
    "Make standard plain text message from MSG hash."
    (concat
     (gethash "tags" msg) "\n"
     (gethash "echo" msg) "\n"
     (nth 0 (split-string (number-to-string (time-to-seconds (date-to-time (gethash "time" msg)))) "\\.")) "\n"
     (gethash "author" msg) "\n"
     (gethash "address" msg) "\n"
     (gethash "recipient" msg) "\n"
     (gethash "subj" msg) "\n"
     "\n"
     (gethash "body" msg) "\n"))


(defun make-message-header (msg)
    "Make message header from MSG hash."
    (concat
     (concat "ID:      " (gethash "id" msg) "\n")
     (concat "From:    " (gethash "author" msg) "[" (gethash "address" msg) "]" "\n")
     (concat "To:      " (gethash "recipient" msg) "\n")
     (concat "Echo:    " (gethash "echo" msg) "\n")
     (concat "At:      " (gethash "time" msg) "\n")
     (concat "Subject: " (gethash "subj" msg) "\n")))

(defun display-message-hash (msg)
    "Disaply message MSG in new buffer."
    (mark-message-read (gethash "id" msg) (gethash "echo" msg))
    (with-output-to-temp-buffer (get-buffer-create (concat "*IDEC: " (gethash "subj" msg) "*"))
        (switch-to-buffer (concat "*IDEC: " (gethash "subj" msg) "*"))
        (princ (make-message-header msg))
        (princ (concat "__________________________________\n\n"
                       (replace-in-string "\r" "" (gethash "body" msg))))
        (princ "\n__________________________________\n")
        (princ "[")
        (let (answer-hash)
            (setq answer-hash (make-hash-table :test 'equal))
            (puthash "content" (make-plain-text-message-from-hash msg) answer-hash)
            (insert-button "Answer"
                           'action (lambda (x) (edit-answer-without-quote (button-get x 'id) (button-get x 'msg-hash)))
                           'id (gethash "id" msg)
                           'msg-hash answer-hash))
        (princ "]")
        (princ "\t   [")
        (insert-button "Quote answer")
        (princ "]")
        (add-text-properties (point-min) (point-max) 'read-only))
    (idec))

(defun display-message (msg)
    "Display message MSG in new buffer in idec-mode."
    (mark-message-read (gethash "id" msg) (get-message-field (gethash "msg" msg) "echo"))
    (defvar line "__________________________________")
    (with-output-to-temp-buffer (get-buffer-create (concat "*IDEC: "
                                                           (decode-coding-string
                                                            (get-message-field
                                                             (gethash "msg" msg) "subj")
                                                            'utf-8)
                                                           "*"))
        ;; Run in IDEC mode
        (switch-to-buffer (concat "*IDEC: " (decode-coding-string (get-message-field
                                                                   (gethash "msg" msg) "subj")
                                                                  'utf-8)
                                  "*"))
        (princ (concat "ID:      " (gethash "id" msg) "\n"))
        (princ (concat "From:    " (get-message-field (gethash "msg" msg) "author") "("
                       (get-message-field (gethash "msg" msg) "address") ")" "\n"))
        (princ (concat "To:      " (get-message-field (gethash "msg" msg) "recipient") "\n"))
        (princ (concat "Echo:    " (get-message-field (gethash "msg" msg) "echo") "\n"))
        (princ (concat "At:      " (get-message-field (gethash "msg" msg) "time") "\n"))
        (princ (concat "Subject: " (get-message-field (gethash "msg" msg) "subj") "\n"))
        (princ (concat line "\n\n"
                       (replace-in-string "\r" ""
                                          (s-join "\n" (get-message-field (gethash "msg" msg)
                                                                          "body")))))
        (princ (concat "\n" line "\n"))
        (princ "[")
        (let (answer-hash)
            (setq answer-hash (make-hash-table :test 'equal))
            (puthash "content" (gethash "msg" msg) answer-hash)
            (insert-button "Answer"
                           'action (lambda (x) (edit-answer-without-quote (button-get x 'id) (button-get x 'msg-hash)))
                           'id (gethash "id" msg)
                           'msg-hash answer-hash))
        (princ "]")
        (princ (concat (make-string 6 ? ) "["))
        (insert-button "Quote answer")
        (princ "]")
        (add-text-properties (point-min) (point-max) 'read-only))
    (point-max)
    (idec-mode))

(defun display-new-messages ()
    "Display new fetched messages from `new-messages-list'."
    (if (= (hash-table-count new-messages-list) 0)
            (message "IDEC: No new messages.")
        (with-output-to-temp-buffer (get-buffer-create "*IDEC: New messages*")
            (switch-to-buffer "*IDEC: New messages*")

            (maphash (lambda (id msg)
                         (let (m)
                             (setq m (make-hash-table :test 'equal))
                             (puthash "id" id m)
                             (puthash "msg" msg m)
                             ;; Write message subj
                             (insert-text-button (concat (get-message-field msg "subj")
                                                         (make-string
                                                          (- (get-longest-field "subj" new-messages-list)
                                                             (length (get-message-field msg "subj")))
                                                          ? ))
                                                 'help-echo "Read message"
                                                 'msg-hash m
                                                 'action (lambda (x) (display-message (button-get x 'msg-hash)))))
                         ;; Write message time and echo
                         (princ (format "  %s(%s)%s%s\t%s\n"
                                        (get-message-field msg "author")
                                        (get-message-field msg "address")
                                        (make-string (-
                                                      (+
                                                       (get-longest-field "author" new-messages-list)
                                                       (get-longest-field "address" new-messages-list)
                                                       1)
                                                      (+
                                                       (length (get-message-field msg "author"))
                                                       (length (get-message-field msg "address")))
                                                      )
                                                     ? )
                                        (get-message-field msg "echo")
                                        (get-message-field msg "time"))))
                     new-messages-list))
        (idec-mode)))

(defun hash-table-keys (hash-table)
    "Get list of keys from HASH-TABLE."
    (let ((keys ()))
        (maphash (lambda (k v) (push k keys)) hash-table)
        keys))

(defun get-messages-content (messages)
    "Get MESSAGES content from `idec-primary-node'."
    (let (new-hash)
        (setq new-hash (make-hash-table :test 'equal))
        ;; (message (get-url-content (make-messages-url (hash-table-keys messages))))
        (dolist (line (split-string (get-url-content (make-messages-url (hash-table-keys messages))) "\n"))
            (when (not (string= "" line))
                (let (msgid content mes)
                    (setq mes (make-hash-table :test 'equal))
                    (setq msgid (nth 0 (split-string line ":")))
                    (setq content
                          (decode-coding-string
                           (base64-decode-string
                            (nth 1 (split-string line ":")))
                           'utf-8))
                    ;; Populate message hash: {"echo": "echo name", "content": "message content"}
                    (puthash "echo" (get-message-field content "echo") mes)
                    (puthash "content" content mes)
                    (puthash msgid mes new-hash))))
        new-hash))

(defun download-message (ids)
    "Download messages with IDS to `idec-mail-dir'."
    (if (= (hash-table-count ids) 0)
            nil
        (maphash (lambda (id msg)
                     (insert-message-to-db (gethash "content" msg) id)
                     ;; (store-message (gethash "content" msg) (gethash "echo" msg) id)
                     (puthash id (gethash "content" msg) new-messages-list))
                 (get-messages-content ids))))

(defun download-subscriptions ()
    "Download messages from echoes defined in `idec-echo-subscriptions' from `idec-primary-node'."
    (message (make-echo-url (split-string idec-echo-subscriptions ",")))
    (message idec-echo-subscriptions)
    (get-url-content
     (make-echo-url (split-string idec-echo-subscriptions ","))))

;; ECHOES FUNCTIONS
;; ;;;;;;;;;;;;;;;;

(defun make-echo-url (echoes &optional online)
    "Make ECHOES url to retreive messages from `idec-primary-node';
with `idec-download-offset' and `idec-download-limit';
If ONLINE is t uses `idec-online-download-limit' and `idec-online-download-offset'."
    ;; Check ECHOES is list
    (let (limit offset)
        (if online
                (and (setq limit idec-online-download-limit)
                     (setq offset idec-online-download-offset))
            (and (setq limit idec-download-limit)
                 (setq offset idec-download-offset)))
        (if (listp echoes)
                ;; Required GNU Emacs >= 25.3
                (message (concat idec-primary-node "u/e/"
                                 (s-join "/" echoes) "/" offset ":" limit))
            (message (concat idec-primary-node "u/e/" echoes "/" offset ":" limit)))))

(defun make-messages-url (messages)
    "Make MESSAGES url to retreive messages from `idec-primary-node'."
    ;; Check MESSAGES is list
    (if (listp messages)
            ;; Required GNU Emacs >= 25.3
            (concat idec-primary-node "u/m/" (s-join "/" messages))
        (concat idec-primary-node "u/m/" messages)))

(defun make-count-url (echo)
    "Return messages count url in `idec-primary-node' from ECHO."
    (concat idec-primary-node "/x/c/" echo))

(defun echo-messages-count (echo)
    "Get messages count in ECHO."
    (nth 1 (split-string
            (get-url-content (make-count-url echo)) ":")))


;; END OF ECHOES FUNCTIONS
;; ;;;;;;;;;;;;;;;;;;;;;;;

;; END OF FUNCTIONS
;; ;;;;;;;;;;;;;;;;


(provide 'idec)

;;; idec.el ends here
