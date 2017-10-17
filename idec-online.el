;;; idec-online.el --- This file part of GNU Emacs client for IDEC network

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
(require 'idec-answers)

(defun display-echo-messages (messages)
    "Display downloaded MESSAGES from echo."
    (message (concat "RECEIVED MESSAGES: " messages))
    (let (msgs echo echo-msg-hash)
        (setq echo-msg-hash (make-hash-table :test 'equal))
        (setq echo (nth 0 (split-string messages "\n")))
        (setq msgs (split-string messages "\n"))
        (dolist (id msgs)
            (when (not (or
                        (string-match "\\." id)
                        (string= "" id)))
                (puthash id echo echo-msg-hash)))
        (with-output-to-temp-buffer (get-buffer-create (concat "*IDEC: online browse " echo "*" ))
            (switch-to-buffer (concat "*IDEC: online browse " echo "*"))
            (maphash (lambda (id msg-hash)
                         (when (equal (get-message-field (gethash "content" msg-hash) "echo") echo)
                         (princ "__________________________________\n")
                         (princ (concat "ID:      " id "\n"))
                         (princ (concat "From:    " (get-message-field (gethash "content" msg-hash) "author") "("
                                        (get-message-field (gethash "content" msg-hash) "address") ")" "\n"))
                         (princ (concat "To:      " (get-message-field (gethash "content" msg-hash) "recipient") "\n"))
                         (princ (concat "Echo:    " (get-message-field (gethash "content" msg-hash) "echo") "\n"))
                         (princ (concat "At:      " (get-message-field (gethash "content" msg-hash) "time") "\n"))
                         (princ (concat "Subject: " (get-message-field (gethash "content" msg-hash) "subj") "\n"))
                         (princ (concat "__________________________________\n\n"
                                        (s-join "\n" (get-message-field (gethash "content" msg-hash) "body"))))
                         (princ "\n__________________________________\n")
                         (princ "[")
                         (insert-button "Answer"
                                        'action (lambda (x) (edit-answer-without-quote (button-get x 'id) (button-get x 'msg-hash)))
                                        'id id
                                        'msg-hash msg-hash)
                         (princ "]")
                         (princ "\t   [")
                         (insert-button "Answer with quote")
                         (princ "]\n\n")))
                     ;; Plain messages hash proccesing
                     (get-messages-content echo-msg-hash))
            (idec-mode)))
    (add-text-properties (point-min) (point-max) 'read-only))


(defun load-echo-messages (echo &optional online)
    "Load messages from ECHO with ONLINE selector."
    (when (not online)
        (message (concat "Update counter of " echo))
        (store-echo-counter echo))
    (display-echo-messages (get-url-content (make-echo-url echo))))

(defun proccess-echo-message (msg echo)
    "Download new message MSG in ECHO."
    (with-output-to-temp-buffer (get-buffer-create "*IDEC: DEBUG*")
        (switch-to-buffer "*IDEC: DEBUG*")
        (princ msg)
        (princ echo)))

(defun proccess-echo-list (raw-list)
    "Parse RAW-LIST from HTTP response."
    (with-output-to-temp-buffer (get-buffer-create "*IDEC: list.txt*")
        (switch-to-buffer "*IDEC: list.txt*")
        (dolist (line (split-string (decode-coding-string raw-list 'utf-8) "\n"))
            (when (not (equal line ""))
                ;; Defind echo
                (defvar current-echo nil)
                (setq current-echo (nth 0 (split-string line ":")))
                ;; Create clickable button
                (insert-text-button current-echo
                                    'action (lambda (x) (load-echo-messages (button-get x 'echo) t))
                                    'help-echo (concat "Go to echo " current-echo)
                                    'echo current-echo)
                (princ (format "\t\t||%s\t\t%s\n"
                               (nth 2 (split-string line ":"))
                               (nth 1 (split-string line ":")))))
            ))
    (idec-mode))

(defun idec-fetch-echo-list (nodeurl)
    "Fetch echoes list from remote NODEURL."
        (proccess-echo-list (get-url-content nodeurl)))

(defun idec-online-browse ()
    "Load echoes list.txt from node `idec-primary-node'."
    (interactive)
    (idec-fetch-echo-list (concat idec-primary-node "list.txt")))

(provide 'idec-online)

;;; idec-online.el ends here
