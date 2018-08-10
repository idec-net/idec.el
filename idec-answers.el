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

(require 'idec)
(require 'web)

(defun replace-in-string (what with str)
    "Replace WHAT WITH in STR."
    (replace-regexp-in-string (regexp-quote what) with str nil 'literal))

(defun base64-to-base64url (str)
    "Make url safe base64 string STR."
    (when (string-match (regexp-quote "=") str)
        (and (base64-to-base64url (replace-regexp-in-string "=+$" "" str))))
    (when (string-match (regexp-quote "+") str)
        (and (base64-to-base64url (replace-in-string "+" "-" str))))
    (when (string-match (regexp-quote "/") str)
        (and (base64-to-base64url (replace-in-string "/" "_" str))))
    str)

(defun base64url-to-base64 (str)
    "Make base64 from urlsafe STR."
    (when (string-match (regexp-quote "-") str)
        (and (base64url-to-base64 (replace-in-string "-" "+" str))))
    (when (string-match (regexp-quote "_") str)
        (and (base64url-to-base64 (replace-in-string "_" "/" str))))
    str)

(defun base64url-encode-string (str)
    "Decode base64 urlsafe string STR."
    (message (concat "Base64url: " (base64-to-base64url (base64-encode-string str t))))
    (base64-to-base64url (base64-encode-string str nil)))

(defun base64url-decode-string (str)
    "Encode base64 urlsafe string STR."
    (base64-decode-string (base64url-to-base64 str)))

(defun point-url ()
    "Return url with `idec-primary-node' to send messages."
    (concat idec-primary-node "u/point"))

(defun request-is-done ()
    "Show message."
    (message "IDEC: Message sended."))

(defun do-post-request (url msg)
    "Make POST request to URL with data MSG."
    (message (gethash 'tmsg msg))
    (web-http-post
     (lambda (con header data)
         (request-is-done))
     :url url
     :data msg
     )
    ;; (request
    ;;  url
    ;;  :type "POST"
    ;;  :data '(("pauth" . (gethash "path" msg)) ("tmsg" . (gethash "tmsg" msg)))
    ;;  ;; :headers '(("Content-Type" . "application/json"))
    ;;  ;; :parser 'json-read
    ;;  :success (function*
    ;;            (lambda (&key data &allow-other-keys)
    ;;                (message "I sent: %S" (assoc-default 'json data)))))
    )

(defun post-message (encoded-message)
    "Do POST request to `idec-primary-node' with Base64 ENCODED-MESSAGE."
    ;; (message (base64url-decode-string encoded-message))
    (let (json)
        (setq json (make-hash-table :test 'equal))
        (puthash 'pauth idec-account-auth json)
        (puthash 'tmsg encoded-message json)
        (do-post-request (point-url) json))
    ;; (message (base64url-decode-string encoded-message))
    (message "Message sended"))

(defun send-new-message (echo)
    "Send new message to ECHO."
    (switch-to-buffer (concat "*IDEC: New message to echo " echo "*"))
    (let ((msg (make-hash-table :test 'equal)))
        (puthash "body"
                 (encode-coding-string (s-join "\n" (-drop-last 1 (-drop 4 (split-string (buffer-string) "\n"))))
                                       'utf-8)

        (puthash "subj"
                 (encode-coding-string (nth 1 (split-string (nth 2 (split-string (buffer-string) "\n")) "bj: "))
                                       'utf-8)
                 msg)
        (puthash "echo" echo msg)
        (do-send-new-post-request msg)))

(defun do-send-new-post-request (msg)
    "Make IDEC compatible point message MSG and send it to `idec-primary-node'."
    (let (point-message)
        (setq point-message (list
                             (gethash "echo" msg)
                             "All"
                             (gethash "subj" msg)
                             ""
                             (gethash "body" msg)))
        ;; Encode message in Base64
        (post-message (base64url-encode-string (s-join "\n" point-message)))))

(defun do-send-reply-post-request (message)
    "Make IDEC compatible point MESSAGE and send it to `idec-primary-node'."
    (let (point-message)
        (setq point-message (list
                             (gethash "echo" message)
                             (gethash "author" message)
                             (gethash "subj" message)
                             ""
                             (concat "@repto:" (gethash "id" message))
                             (gethash "body" message)))
        ;; Encode message in Base64
        (post-message (base64url-encode-string (encode-coding-string (s-join "\n" point-message) 'utf-8)))
        (kill-buffer (concat "*IDEC: answer to " (gethash "id" message) "*"))))

(defun send-reply-message (msg)
    "Send message MSG to `idec-primary-node'."
    (switch-to-buffer (concat "*IDEC: answer to " (gethash "id" msg) "*"))
    (puthash "body"
             (s-join "\n" (-drop-last 1 (-drop 4 (split-string (buffer-string) "\n"))))
             msg)
    (do-send-reply-post-request msg))

(defun get-answers-hash (id msg-hash)
    "Make answers hashtable from ID and MSG-HASH."
    (if (gethash "content" msg-hash)
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
                answer-hash)
        (let (subj)
            (setq subj (gethash "subj" msg-hash))

            ;; Make `Re:' in subj if it not present.
            (if (not subj)
                    (puthash "subj" (concat "Re: " "") msg-hash)
                (if (not (string-match "Re:" subj))
                        (puthash "subj" (concat "Re: " subj) msg-hash)))
            msg-hash)))

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
                            'action (lambda (x) (send-reply-message (button-get x 'msg)))
                            'msg answer-hash)
        (goto-char p)
        (idec)))

(defun idec-answers-get-author-for-quote (author)
    "Get AUTHOR."
    ;; Parse author
    (if (string-match ".* .*" author)
            (concat
             (nth 1 (s-split "" (nth 0 (s-split " " author))))
             (nth 1 (s-split "" (nth 1 (s-split " " author)))))
        author))

(defun idec-answers-insert-quote (head tail author)
    "Recursive function for inserting quoted body text;
receive string HEAD, list TAIL and original message AUTHOR."
    (message "Quote insert %S" author)
    (when (not (string-match "^$" head))
        (insert author)
        (insert "> "))
    (insert (replace-in-string "\r" "" head)) (insert "\n")
    (when (> (length tail) 0)
        (idec-answers-insert-quote (car tail) (cdr tail) author)))

(defun idec-answers-edit-answer-with-quote (id msg-hash)
    "Answer to message with quoted body with ID, MSG-HASH and BODY."
    (let (answer-hash p author)
        (setq answer-hash (get-answers-hash id msg-hash))
        (switch-to-buffer (get-buffer-create (concat "*IDEC: answer to " id "*")))

        ;; Answer message header
        (insert (make-answer-header id msg-hash))
        (forward-line)
        (add-text-properties (point) (point-min) 'read-only)


        ;; Quote
        (idec-answers-insert-quote
         (car (get-message-field (gethash "content" msg-hash) "body"))
         (cdr (get-message-field (gethash "content" msg-hash) "body"))
         (idec-answers-get-author-for-quote (get-message-field (gethash "content" msg-hash) "author")))
        (insert "\n")
        
        (setq p (- (point) 1))

        ;; [Send] button
        (setq start (point))
        (insert-button "[Send]"
                            'action (lambda (x) (send-reply-message (button-get x 'msg)))
                            'msg answer-hash)
        (goto-char p))
    (idec))

;; END OF ANSWERS

;; NEW MESSAGE
(defun make-new-message-header (echo)
    "Return header for new message with filled ECHO field."
    (concat
     (concat "New message to echo " echo "\n")
     "\n"
     "Subj: \n"
     "------- YOU MESSAGE BELLOW -------\n"))

(defun edit-new-message (echo)
    "Edit new message to ECHO."
    (switch-to-buffer (get-buffer-create (concat "*IDEC: New message to echo " echo "*")))
    (insert (make-new-message-header echo))
    (forward-line)

    (let (p)
        (setq p (point))
        (insert "\n")
        (insert-text-button "[Send]"
                            'action (lambda (x) (send-new-message (button-get x 'msg-echo)))
                            'msg-echo echo)
        (goto-char p))
    (org-idec))

(provide 'idec-answers)

;;; idec-answers.el ends here
