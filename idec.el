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

(require 'idec-mode)
;; (require 'idec-answers)
(require 'idec-parser)
(require 'idec-online)
(require 'idec-db)

(defgroup idec nil
    "IDEC configuration."
    :group 'network)

;; Not used
(defcustom idec-nodes-list
    "http://idec.spline-online.tk/,https://ii-net.tk/ii/ii-point.php?q=/"
    "List(comma separated) of IDEC nodes."
    :type 'string
    :group 'idec)

(defcustom idec-primary-node nil
    "Primary node to send messages."
    :type 'string
    :group 'idec)

;; Never used at this time.
(defcustom idec-use-list-txt t
    "Use /list.txt extension."
    :group 'idec)

(defcustom idec-smart-fetch t
    "Enable smat fetching;
Download only new messages; Not implemented."
    :type 'boolean
    :group 'idec)

(defcustom idec-download-limit "50"
    "Limit of download messages;
Not used if `idec-smart-fetching' is not nil."
    :type 'string
    :group 'idec)

(defcustom idec-download-offset "-50"
    "Offset of download messages;
Not used if `idec-smart-fetching' is not nil."
    :type 'string
    :group 'idec)

(defcustom idec-echo-subscriptions nil
    "List of subribes echoes."
    :type 'string
    :group 'idec)

(defcustom idec-mail-dir "~/.emacs.d/idec-mail"
    "Directory to store mail."
    :type 'string
    :group 'idec)

(defcustom idec-online-download-limit "0"
    "Download limit on online browsing;
Default to `idec-download-lmit'"
    :type 'string
    :group 'idec)

(defcustom idec-online-download-offset "0"
    "Download limit on online browsing;
Default to `idec-download-offset'"
    :type 'string
    :group 'idec)

(defgroup idec-accounts nil
    "IDEC accounts settings."
    :group 'idec)

(defcustom idec-account-nick ""
    "Account nickname."
    :type 'string
    :group 'idec-accounts)

(defcustom idec-account-node ""
    "Node to send messages."
    :type 'string
    :group 'idec-accounts)

(defcustom idec-account-auth ""
    "Account authstring."
    :type 'string
    :group 'idec-accounts)

;; END OF CUSTOMIZATION
;; ;;;;;;;;;;;;;;;;;;;;

;; VARIABLES
;; ;;;;;;;;;

(defvar smart-download-limit nil
    "Used with `idec-smart-fetch'.")

(defvar smart-download-offset nil
    "Used with `idec-smart-fetch'.")

(defvar new-messages-list nil
    "New messages for display.")

(setq idec-online-download-limit idec-download-limit)
(setq idec-online-download-offset idec-download-offset)

;; END OF VARIABLES
;; ;;;;;;;;;;;;;;;;

;; NAVIGATION FUNCTIONS
;; ;;;;;;;;;;;;;;;;;;;;

(defun idec-next-message ()
    "Show next message."
    (interactive)
    (kill-this-buffer)
    (forward-button 1)
    (push-button))

(defun idec-previous-message ()
    "Show next message."
    (interactive)
    (kill-this-buffer)
    (backward-button 1)
    (push-button))

;; END OF NAVIGATION FUNCTIONS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
     ? ))

(defun mark-all-as-read (echo &optional checkpoint)
    "Mark all messages in ECHO as read;
put cursor to CHECKPOINT."
    (mark-all-messages-as-read echo)
    (idec-local-browse checkpoint))

(defun idec-mark-all-as-read (&optional echo)
    "Mark all messages in ECHO as read."
    (interactive)
    (if (not echo)
            (mark-all-messages-as-read (read-string "Enter echo name: "))
        (mark-all-messages-as-read echo)))

(defun idec-local-browse (&optional checkpoint)
    "Browse local mail from `idec-mail-dir';
optionaly return cursor to CHECKPOINT."
    (interactive)
    (get-buffer-create "*IDEC: INBOX*")
    (with-output-to-temp-buffer (get-buffer-create "*IDEC: INBOX*")
        (switch-to-buffer "*IDEC: INBOX*")
        (save-excursion
            (dolist (echo (get-local-echoes))
                (if echo
                        (let (unread start end)
                            ;; Echo name with unread messages
                            ;; ii.test.14 (5)*
                            (insert-button echo
                                           'action (lambda (x) (browse-local-echo (button-get x 'echo)))
                                           'echo echo
                                           '(face nil))

                            (beginning-of-line)
                            (setq start (point))
                            (end-of-line)
                            (setq end (point))
                            (add-text-properties start end '(comment t face '(:foreground "light green")))

                            (princ (concat (dots echo)
                                           "("
                                           (number-to-string (get-echo-messages-count echo))
                                           ")"))

                            (setq unread (get-echo-unread-messages echo))
                            (when (> unread 0)
                                (princ "*"))
                            (princ " ")

                            ;; [New message] button
                            (princ "\t[")
                            (insert-button "New message"
                                           'action (lambda (x) (idec-new-message (button-get x 'echo)))
                                           'echo echo)
                            (princ "]\t[")
                            ;; [Mark read] button
                            (insert-button "Mark read"
                                           'action (lambda (x) (mark-all-as-read (button-get x 'echo) (button-get x 'point)))
                                           'echo echo
                                           'point (point))
                            (princ "]\n"))
                    (message (concat "IDEC: FUUUUUU <" echo ">")))
                ))
        (add-text-properties (point-min) (point-max) 'read-only))
    (if checkpoint
            (goto-char checkpoint))
    (idec-mode))

(defun browse-local-echo (&optional echo)
    "Get messages from local ECHO."
    (interactive)
    (if (not echo)
            (setq echo (read-string "Enter echo name: ")))
    (with-output-to-temp-buffer (get-buffer-create (concat "*IDEC: INBOX->(" echo ")") )
        (princ (number-to-string (length (get-echo-messages echo))))
        (idec-mode)))

;; END OF LOCAL MAIL FUNCTIONS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun idec-load-new-messages ()
    "Load new messages from IDEC `idec-primary-node'."
    (interactive)
    (defvar current-echo nil)
    (setq new-messages-list (make-hash-table :test 'equal))
    (let (msgid-for-download)
        (setq msgid-for-download (make-hash-table :test 'equal))
        (dolist (line (split-string (download-subscriptions) "\n"))
            (if (string-match "\\." line)
                    (and (setq current-echo line)
                         (store-echo-counter line))
                (when (and ;; (check-message-in-echo line current-echo)
                           (> (length line) 1)
                           (check-message-in-db line current-echo))
                    (when (not (string= "" line))
                        (puthash line current-echo msgid-for-download)))))
        (download-message msgid-for-download))
    ;; (print (hash-table-count new-messages-list))
    ;; (message (gethash "id" (nth 0 new-messages-list)))
    (display-new-messages)
    )

(defun display-message (msg)
    "Display message MSG in new buffer in idec-mode."
    (mark-message-read (gethash "id" msg) (get-message-field (gethash "msg" msg) "echo"))
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
        (princ (concat "__________________________________\n\n"
                       (s-join "\n" (get-message-field (gethash "msg" msg) "body"))))
        (princ "\n__________________________________\n")
        (princ "[")
        (let (answer-hash)
            (setq answer-hash (make-hash-table :test 'equal))
            (puthash "content" (gethash "msg" msg) answer-hash)
            (insert-button "Answer"
                           'action (lambda (x) (edit-answer-without-quote (button-get x 'id) (button-get x 'msg-hash)))
                           'id (gethash "id" msg)
                           'msg-hash answer-hash))
        (princ "]")
        (princ "\t   [")
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

(defun idec-new-message (&optional echo)
    "Make new message to ECHO."
    (interactive)
    (if (not echo) (edit-new-message (read-string "Echo: "))
        (edit-new-message echo)))

;; END OF FUNCTIONS
;; ;;;;;;;;;;;;;;;;


(provide 'idec)

;;; idec.el ends here
