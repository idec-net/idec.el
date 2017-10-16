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

(require 'idec-customize)

;; MODE
;; ;;;;

(defun idec-close-message-buffer ()
    "Close buffer with message."
    (kill-this-buffer))

(defvar idec-mode-hook nil)

(defvar idec-mode-map
    (let ((map (make-keymap)))
        (define-key map "\C-c \C-c" 'kill-this-buffer)
        (define-key map "\C-c \C-n" 'idec-next-message)
        (define-key map "\C-c \C-b" 'idec-previous-message)
        map)
    "Keymapping for IDEC mode.")

(defconst idec-font-lock-keywords-1
    (list
     '("\\(ID:.*\\)" . font-lock-function-name-face)
     '("\\<\\(\\(?:Echo\\|From\\|Subj\\|T\\(?:ime\\|o\\)\\):\\)\\>" . font-lock-function-name-face))
    "Minimal highlighting expressions for IDEC mode.")

(defconst idec-font-lock-keywords-2
    (append idec-font-lock-keywords-1 (list
                                       '("\\<\\(>>?.*\\)\s\\>" . font-lock-comment-face)))
    "Quotes highligting for IDEC mode.")

(defvar idec-font-lock-keywords idec-font-lock-keywords-2
    "Default highlighting expressions for IDEC mode.")

(defvar idec-mode-syntax-table
    (make-syntax-table text-mode-syntax-table))

;; Mode function
(defun idec-mode ()
    "Major mode for view and editing IDEC messages."
    (interactive)
    (kill-all-local-variables)
    ;; Mode definition
    ;; (set-syntax-table idec-mode-syntax-table)
    (use-local-map idec-mode-map)
    ;; (font-lock-add-keywords 'idec-mode '(idec-font-lock-keywords))
    ;; (set (make-local-variable 'font-lock-defaults) '(idec-font-lock-keywords))
    (setq major-mode 'idec-mode)
    (setq mode-name "[IDEC]")
    (setq imenu-generic-expression "*IDEC")
    (run-hooks 'idec-mode-hook))

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

;; END OF MODE
;; ;;;;;;;;;;;

;; FUNCTIONS
;; ;;;;;;;;;

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

(defun get-local-echoes ()
    "Get local downloaded echoes from `idec-mail-dir'."
    (delete '".." (delete '"." (directory-files idec-mail-dir nil "\\w*\\.\\w*"))))

(defun idec-browse-local-mail ()
    "Browse local mail from `idec-mail-dir'."
    (message (s-join " " (get-local-echoes))))

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
                (when (and (check-message-in-echo line current-echo)
                           (> (length line) 1))
                    (when (not (string= "" line))
                        (puthash line current-echo msgid-for-download)))))
        (download-message msgid-for-download))
    ;; (print (hash-table-count new-messages-list))
    ;; (message (gethash "id" (nth 0 new-messages-list)))
    (display-new-messages)
    )

(defun display-message (msg)
    "Display message MSG in new buffer in idec-mode."
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
        (insert-button "Answer"
                       'action (lambda (x) (message "OK")))
        (princ "]")
        (princ "\t   [")
        (insert-button "Answer with quote")
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

;; Online reading
(defun display-echo-messages (messages)
    "Display downloaded MESSAGES from 2echo."
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

;; ANSWERS
(defun make-answer-header (id msg-hash)
    "Make header with reto to ID from MSG-HASH."
    (let (answer-hash subj p)
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
    (let (answer-hash subj p)
        (setq answer-hash (make-hash-table :test 'equal))
        (puthash "id" id answer-hash)
        (puthash "echo" (get-message-field (gethash "content" msg-hash) "echo") answer-hash)
        (puthash "author" (get-message-field (gethash "content" msg-hash) "author") answer-hash)

        (setq subj (get-message-field (gethash "content" msg-hash) "subj"))

        ;; Make `Re:' in subj if it not present.
        (if (not (string-match "Re:" subj))
                (puthash "subj" (concat "Re: " subj) answer-hash)
            (puthash "subj" subj answer-hash))
        
        (switch-to-buffer (get-buffer-create (concat "*IDEC: answer to " id "*")))

        (insert (make-answer-header id msg-hash))
        (forward-line)
        (add-text-properties (point-min) (point) 'read-only)

        (forward-line)
        (setq p (point))

        (point-max)
        (insert "\n")
        (insert-text-button "[Send]"
                            'action (lambda (x) (message "Send...")))
        (goto-char p)
        )
    (idec-mode))

(defun edit-answer-without-quote (id msg-hash)
    "Answer to message with ID MSG-HASH."
    (let (answer-hash subj p)
        (setq answer-hash (make-hash-table :test 'equal))
        (puthash "id" id answer-hash)
        (puthash "echo" (get-message-field (gethash "content" msg-hash) "echo") answer-hash)
        (puthash "author" (get-message-field (gethash "content" msg-hash) "author") answer-hash)

        (setq subj (get-message-field (gethash "content" msg-hash) "subj"))

        ;; Make `Re:' in subj if it not present.
        (if (not (string-match "Re:" subj))
                (puthash "subj" (concat "Re: " subj) answer-hash)
            (puthash "subj" subj answer-hash))
        
        (switch-to-buffer (get-buffer-create (concat "*IDEC: answer to " id "*")))

        (insert (concat "Answer to " id " in " (gethash "echo" answer-hash)))
        ;; Write header
        ;; (princ (concat "Answer to " id " in " (gethash "echo" answer-hash)))

        ;; Make it readonly
        ;; (add-text-properties (point) (point-min) 'read-only)

        ;; Write author
        (forward-line)
        (insert (concat "\nAuthor: " (gethash "author" answer-hash) "\n"))
        (add-text-properties (point-min) (point) 'read-only)

        ;; Write subj
        (point-max)
        (forward-line)
        (insert (concat "Subj: "(gethash "subj" answer-hash)))
        (forward-line)

        ;; Body
        (insert "\n------- YOU MESSAGE BELLOW -------\n")
        ;; (add-text-properties (beginning-of-line) (end-of-line) 'read-only)
        (forward-line)
        (setq p (point))

        (point-max)
        (insert "\n")
        (insert-text-button "[Send]"
                            'action (lambda (x) (message "Send...")))
        (goto-char p)
        )
    (idec-mode))

;; END OF ANSWERS

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
                     (store-message (gethash "content" msg) (gethash "echo" msg) id)
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

(defun make-echo-url (echoes)
    "Make ECHOES url to retreive messages from `idec-primary-node';
with `idec-download-offset' and `idec-download-limit'."
    ;; Check ECHOES is list
    (if (listp echoes)
            ;; Required GNU Emacs >= 25.3
            (message (concat idec-primary-node "u/e/"
                             (s-join "/" echoes) "/" idec-download-offset ":" idec-download-limit))
        (message (concat idec-primary-node "u/e/" echoes "/" idec-download-offset ":" idec-download-limit))))

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

(defun load-echo-messages (echo)
    "Load messages from ECHO."
    (message (concat "DEBUG: " echo))
    (message (concat "URL: " (make-echo-url echo)))
    (message (concat "CONTENT: " (get-url-content (make-echo-url echo))))
    ;; (store-echo-counter echo)
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
                                    'action (lambda (x) (load-echo-messages (button-get x 'echo)))
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

;; END OF ECHOES FUNCTIONS
;; ;;;;;;;;;;;;;;;;;;;;;;;

;; END OF FUNCTIONS
;; ;;;;;;;;;;;;;;;;


(provide 'idec)

;;; idec.el ends here
