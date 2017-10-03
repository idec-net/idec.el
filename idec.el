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

;; CUSTOMIZATION
;; ;;;;;;;;;;;;;

(defgroup idec nil
    "IDEC configuration."
    :group 'network)

;; Not used
(defcustom idec-nodes-list
    '("http://idec.spline-online.tk/"
      "https://ii-net.tk/ii/ii-point.php?q=/")
    "List of IDEC nodes."
    :type 'alist
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

(defgroup idec-accounts nil
    "IDEC accounts settings."
    :group 'idec)

(defcustom idec-account-nick nil
    "Account nickname."
    :type 'string
    :group 'idec-accounts)

(defcustom idec-account-node nil
    "Node to send messages."
    :type 'string
    :group 'idec-accounts)

(defcustom idec-account-auth nil
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

;; END OF VARIABLES
;; ;;;;;;;;;;;;;;;;

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
   '("\\<\\(\\(?:Echo\\|From\\|Subj\\|T\\(?:ime\\|o\\)\\):\\)\\>" . font-lock-variable-name-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Minimal highlighting expressions for IDEC mode.")

(defconst idec-font-lock-keywords-2
    (append idec-font-lock-keywords-1 (list
                                       '("\\<\\(>>?.*\\)\s\\>" . font-lock-comment-face)
                                       '("\\('\\w*'\\)" . font-lock-variable-name-face)))
    "Quotes highligting for IDEC mode.")

(defvar idec-font-lock-keywords idec-font-lock-keywords-2
    "Default highlighting expressions for IDEC mode.")

(defvar idec-mode-syntax-table
    (let ((st (make-syntax-table)))))

(defun idec-mode ()
    "Major mode for view and editing IDEC messages."
    (interactive)
    (kill-all-local-variables)
    ;; Mode definition
    ;; (set-syntax-table idec-mode-syntax-table)
    (use-local-map idec-mode-map)
    (set (make-local-variable 'font-lock-defaults) '(idec-font-lock-keywords))
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
    (trim-string (nth 0 (split-string msg "\n"))))

(defun get-message-echo (msg)
    "Get MSG echo."
    (trim-string (nth 1 (split-string msg "\n"))))

(defun get-message-time (msg)
    "Get MSG time."
    (trim-string (current-time-string
     (car (read-from-string (nth 2 (split-string msg "\n")))))))

(defun get-message-author (msg)
    "Get MSG author."
    (trim-string (nth 3 (split-string msg "\n"))))

(defun get-message-address (msg)
    "Get MSG address."
    (trim-string (nth 4 (split-string msg "\n"))))

(defun get-message-recipient (msg)
    "Get MSG recipient."
    (trim-string (nth 5 (split-string msg "\n"))))

(defun get-message-subj (msg)
    "Get MSG subject."
    (trim-string (nth 6 (split-string msg "\n"))))

(defun get-message-body (msg)
    "Get MSG body text.
Return list with body content."
    (-drop 8 (split-string msg "\n")))

(defun get-longest-field (field msg-list)
    "Return longest FIELD in MSG-LIST."
    (defvar field-legth '())
    (defvar field-max nil)
    (setq field-max 0)
    (dolist (msg msg-list)
        (when (> (length (get-message-field (cdr (assoc 'content msg)) field))
                 field-max)
            (setq field-max (length (get-message-field (cdr (assoc 'content msg)) field)))))
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

(defun idec-load-new-messages ()
    "Load new messages from IDEC `idec-primary-node'."
    (interactive)
    (defvar current-echo nil)
    (defvar new-messages '())
    (dolist (line (split-string (download-subscriptions) "\n"))
        (if (string-match "\\." line)
                (and (setq current-echo line)
                     (store-echo-counter line))
            (when (and (check-message-in-echo line current-echo)
                       (> (length line) 1))
                (download-message current-echo line))))
    (display-new-messages))

(defun answer-message (msg)
    "Make answer to message MSG."
    (get-buffer-create (concat "*IDEC: Answer to " (cdr (assoc 'id msg))))
    (switch-to-buffer (concat "*IDEC: Answer to " (cdr (assoc 'id msg)))))

(defun display-message (msg)
    "Display message MSG in new buffer in idec-mode."
    (with-output-to-temp-buffer (get-buffer-create (concat
                                                    "*IDEC: "
                                                    (decode-coding-string
                                                     (get-message-field
                                                      (cdr (assoc 'content msg)) "subj")
                                                     'utf-8)
                                                    "*"))
        ;; Run in IDEC mode
        (switch-to-buffer (concat "*IDEC: " (decode-coding-string (get-message-field (cdr (assoc 'content msg)) "subj") 'utf-8) "*"))
        (princ (concat "From:    " (get-message-field (cdr (assoc 'content msg)) "author") "(" (get-message-field (cdr (assoc 'content msg)) "address") ")" "\n"))
        (princ (concat "To:      " (get-message-field (cdr (assoc 'content msg)) "recipient") "\n"))
        (princ (concat "Echo:    " (get-message-field (cdr (assoc 'content msg)) "echo") "\n"))
        (princ (concat "At:      " (get-message-field (cdr (assoc 'content msg)) "time") "\n"))
        (princ (concat "Subject: " (get-message-field (cdr (assoc 'content msg)) "subj") "\n"))
        (princ (concat "__________________________________\n\n"
                       (s-join "\n" (get-message-field (assoc 'content msg) "body"))))
        (princ "\n__________________________________\n")
        (princ "[")
        (insert-button "Answer"
                       'action (lambda (x) (message "OK")))
        (princ "]")
        (princ "\t[")
        (insert-button "Answer with quote")
        (princ "]")
        (add-text-properties (point-min) (point-max) 'read-only))
    (point-max)
    (idec-mode))

(defun display-new-messages ()
    "Display new fetched messages from `new-messages-list'."
    (with-output-to-temp-buffer (get-buffer-create "*IDEC: New messages*")
        (switch-to-buffer "*IDEC: New messages*")
        (if (= (length new-messages-list) 0)
                (princ "No new messages.")
            (dolist (msg (reverse new-messages-list))
                ;; Write message subj
                (insert-text-button (concat (get-message-field (cdr (assoc 'content msg)) "subj")
                                            (make-string
                                             (- (get-longest-field "subj" new-messages-list)
                                                (length (get-message-field (cdr (assoc 'content msg)) "subj")))
                                             ? ))
                                    'help-echo "Read message"
                                    ;; 'plain-msg msg
                                    'action (lambda (x) (display-message msg;; (button-get x 'plain-msg)
                                                                         )))
                ;; Write message time and echo
                (princ (format "  %s(%s)%s%s\t%s\n"
                               (get-message-field (cdr (assoc 'content msg)) "author")
                               (get-message-field (cdr (assoc 'content msg)) "address")
                               (make-string (-
                                             (+
                                              (get-longest-field "author" new-messages-list)
                                              (get-longest-field "address" new-messages-list)
                                              1)
                                             (+
                                              (length (get-message-field (cdr (assoc 'content msg)) "author"))
                                              (length (get-message-field (cdr (assoc 'content msg)) "address")))
                                             )
                                            ? )
                               (get-message-field (cdr (assoc 'content msg)) "echo")
                               (get-message-field (cdr (assoc 'content msg)) "time")))
                (add-to-invisibility-spec '((cdr (assoc 'content msg)) . t)))))
    (idec-mode))

(defun get-message-content (echo msg)
    "Get ECHO MSG content from `idec-primary-node'."
    (decode-coding-string
     (base64-decode-string
      (nth 1 (split-string (get-url-content (make-messages-url msg)) ":")))
     'utf-8))

(defun download-message (echo msg)
    "Download ECHO message MSG to `idec-mail-dir'."
    (message (concat "Download message " msg " to " echo))
    (defvar message-content)
    (if (string= "" msg)
            (message "Nil message")
        (when (and (not (string= "" msg))
                   (check-message-in-echo msg echo))
            (setq message-content (get-message-content echo msg))
            (store-message message-content echo msg)
            (setq new-messages-list (-concat
                                     'new-messages-list
                                     '(
                                       (content . message-content)
                                       (id . msg))
                                     )))))

(defun download-subscriptions ()
    "Download messages from echoes defined in `idec-echo-subscriptions' from `idec-primary-node'."
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
    ;; Check ECHOES is list
    (if (listp messages)
            ;; Required GNU Emacs >= 25.3
            (message (concat idec-primary-node "u/m/"
                    (s-join "/" messages)))
        (message (concat idec-primary-node "u/m/" messages))))

(defun make-count-url (echo)
    "Return messages count url in `idec-primary-node' from ECHO."
    (concat idec-primary-node "/x/c/" echo))

(defun echo-messages-count (echo)
    "Get messages count in ECHO."
    (nth 1 (split-string
            (get-url-content (make-count-url echo)) ":")))

(defun display-echo-messages (messages)
    "Display downloaded MESSAGES from echo."
    (with-output-to-temp-buffer (get-buffer-create (concat "*IDEC: browse echo*"))
        (switch-to-buffer "*IDEC: browse echo*")
        (princ messages)))

(defun load-echo-messages (echo)
    "Load messages from ECHO."
    (store-echo-counter echo)
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
                (setq current-echo (assoc 'content (split-string line ":")))
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
