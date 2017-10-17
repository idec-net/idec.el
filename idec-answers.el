;;; idec-answers.el --- This file part of GNU Emacs client for IDEC network

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
(require 'web)

(defun point-url ()
    "Return url with `idec-primary-node' to send messages."
    (concat idec-primary-node "u/point"))

(defun request-is-done (result)
    "Show message with RESULT code."
    (message "IDEC: Sended. Result: %S" result))

(defun do-post-request (url data)
    "Make POST request to URL with DATA."
    (web-http-post
     (lambda (con header data)
         (request-is-done data))
     :url url
     :data data))

(defun post-message (encoded-message)
    "Do POST request to `idec-primary-node' with Base64 ENCODED-MESSAGE."
    (let (json)
        (setq json (make-hash-table :test 'equal))
        (puthash "pauth" idec-account-auth json)
        (puthash "tmsg" encoded-message json)
        (do-post-request (point-url) json))
    (message "Message sended"))

(defun do-send-reply-post-request (message)
    "Make IDEC compatible point MESSAGE and send it to `idec-primary-node'."
    (message (gethash "body" message))
    (let (point-message)
        (setq point-message (list
                             (gethash "echo" message)
                             (gethash "author" message)
                             (gethash "subj" message)
                             ""
                             (concat "@repto:" (gethash "id" message))
                             (gethash "body" message)))
        ;; Encode message in Base64
        (post-message (base64-encode-string (encode-coding-string (s-join "\n" point-message) 'utf-8)))))

(defun send-message (msg)
    "Send message MSG to `idec-primary-node'."
    (switch-to-buffer (concat "*IDEC: answer to " (gethash "id" msg) "*"))
    (puthash "body"
             (s-join "\n" (-drop-last 1 (-drop 4 (split-string (buffer-string) "\n"))))
             msg)
    (message (gethash "body" msg))
    (do-send-reply-post-request msg))

(defun get-answers-hash (id msg-hash)
    "Make answers hashtable from ID and MSG-HASH."
    (let (answer-hash)
        (setq answer-hash (make-hash-table :test 'equal))
        (puthash "id" id answer-hash)
        (puthash "echo" (get-message-field (gethash "content" msg-hash) "echo") answer-hash)
        (puthash "author" (get-message-field (gethash "content" msg-hash) "author") answer-hash)
        (puthash "time" (get-message-field (gethash "content" msg-hash) "time") answer-hash)

        (setq subj (get-message-field (gethash "content" msg-hash) "subj"))

        ;; Make `Re:' in subj if it not present.
        (if (not (string-match "Re:" subj))
                (puthash "subj" (concat "Re: " subj) answer-hash)
            (puthash "subj" subj answer-hash))
        answer-hash))

(defun make-answer-header (id msg-hash)
    "Make header with reto to ID from MSG-HASH."
    (let (answer-hash subj p)
        (setq answer-hash (get-answers-hash id msg-hash))

        (concat
         (concat "Answer to " id " in " (gethash "echo" answer-hash) "\n")
         (concat "Author: "
                 (gethash "author" answer-hash)
                 (concat " at " (gethash "time" answer-hash))
                 "\n")
         (concat "Subj:   " (gethash "subj" answer-hash) "\n")
         "------- YOU MESSAGE BELLOW -------\n")))

(defun edit-answer-without-quote (id msg-hash)
    "Answer to message with ID MSG-HASH."
    (let (answer-hash p)
        (setq answer-hash (get-answers-hash id msg-hash))
        (switch-to-buffer (get-buffer-create (concat "*IDEC: answer to " id "*")))

        (insert (make-answer-header id msg-hash))
        (forward-line)
        (add-text-properties (point) (point-min) 'read-only)
        (setq p (point))

        (insert "\n")
        (insert-text-button "[Send]"
                            'action (lambda (x) (send-message (button-get x 'msg)))
                            'msg answer-hash)
        (goto-char p)
    (idec-mode)))

;; END OF ANSWERS

(provide 'idec-answers)

;;; idec-answers.el ends here
