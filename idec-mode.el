;;; idec-mode.el --- This file part of GNU Emacs client for IDEC network

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
(require 'idec-online)

(defgroup idec nil
    "IDEC configuration."
    :group 'network)

;; Not used
(defcustom idec-nodes-list ""
    "List(comma separated) of IDEC nodes;
http://idec.spline-online.tk/,https://ii-net.tk/ii/ii-point.php?q=/;
Currently not used."
    :type 'string
    :group 'idec)

(defcustom idec-primary-node ""
    "Primary node to send messages."
    :type 'string
    :group 'idec)

;; Never used at this time.
(defcustom idec-use-list-txt t
    "Use /list.txt extension."
    :type 'boolean
    :group 'idec)

(defcustom idec-smart-fetch t
    "Enable smart fetching;
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

(defcustom idec-echo-subscriptions "idec.talks,pipe.2032"
    "List(comma separated string) of subribes echoes."
    :type 'string
    :group 'idec)

(defcustom idec-mail-dir (concat user-emacs-directory "idec-mail")
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

(defcustom idec-desc-local-echo-sort nil
    "Sort order in local echo display mode."
    :type 'boolean
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

;; MODE
;; ;;;;

(defun idec-close-message-buffer ()
    "Close buffer with message."
    (kill-this-buffer))

(defvar idec-mode-hook nil)

(defvar idec-mode-map
    (let ((map (make-sparse-keymap)))
        (define-key map "\C-c \C-c" 'kill-this-buffer)
        (define-key map "\C-c \C-n" 'idec-next-message)
        (define-key map "\C-c \C-b" 'idec-previous-message)
        (define-key map "\C-c \C-e" 'idec-new-message)
        map)
    "Keymapping for IDEC mode.")

(defvar idec-mode-syntax-table
    (let ((st (make-syntax-table)))
        (modify-syntax-entry ?# "<" st)
        (modify-syntax-entry ?\n ">" st)
        st))

(defconst idec-font-lock-keywords-1
    (list
     '("\\(ID:.*\\)" . font-lock-function-name-face)
     '("\\<\\(\\(?:Echo\\|From\\|Subj\\|T\\(?:ime\\|o\\)\\):\\)\\>" . font-lock-function-name-face))
    "Minimal highlighting expressions for IDEC mode.")

(defconst idec-font-lock-keywords-2
    (append idec-font-lock-keywords-1 (list
                                       '("\\<\\(>>?.*\\)\s\\>" . font-lock-comment-face)))
    "Quotes highligting for IDEC mode.")

(defvar idec-font-lock-keywords idec-font-lock-keywords-1
    ;; '(("ID" (1 font-lock-function-name-face)))
    "Default highlighting expressions for IDEC mode.")

;; Mode function
(define-generic-mode
        'idec
    '("//" ">" "ЗЫ" "# ")
    '("ii://" "ID" "$subj" "сабж" "субж"
      "Subject" "From" "To" "Echo" "At")
    '(("=" . 'font-lock-operator)
      (";" . 'font-lock-builtin))
    '("\\.*idec\\.*|\\.*Idec\\.*|\\.*IDEC\\.*$")
    nil)

(define-derived-mode org-idec text-mode "IDEC"
    "Major mode for view and editing IDEC messages."
    :syntax-table idec-mode-syntax-table
    (setq-local comment-start "/{2} ")
    (setq-local font-lock-defaults
                '(idec-font-lock-keywords))
    (use-local-map idec-mode-map)
    (setq-local indent-line-function 'org-indent-line)
    (setq imenu-generic-expression "*IDEC")
    (setq mode-name "[IDEC]")
    (run-hooks 'idec-mode-hook))

(defun idec-mode ()
    "Major mode for view and editing IDEC messages."
    (interactive)
    (kill-all-local-variables)
    ;; Mode definition
    (set-syntax-table idec-mode-syntax-table)
    (use-local-map idec-mode-map)
    ;; (font-lock-add-keywords 'idec-mode '(idec-font-lock-keywords))
    ;; (set (make-local-variable 'font-lock-defaults) '(idec-font-lock-keywords))
    (setq major-mode 'idec-mode)
    (setq mode-name "[IDEC]")
    (setq imenu-generic-expression "*IDEC")
    (run-hooks 'idec-mode-hook))

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
                                           'action (lambda (x) (idec-browse-local-echo (button-get x 'echo)))
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
        (add-text-properties (beginning-of-buffer) (end-of-buffer) 'read-only))
    (if checkpoint
            (goto-char checkpoint))
    (idec))

(defun idec-browse-local-echo (&optional echo)
    "Get messages from local ECHO."
    (interactive)
    (if (not echo)
            (setq echo (read-string "Enter echo name: ")))
    (let (longest)
        (setq longest (+ 1 (longest-local-echo-subj echo)))
        (with-output-to-temp-buffer (get-buffer-create (concat "*IDEC: INBOX->(" echo ")") )
            (switch-to-buffer (concat "*IDEC: INBOX->(" echo ")"))
            (dolist (msg (get-echo-messages echo))
                (setq subj-length (length (gethash "subj" msg)))
                (insert-button (gethash "subj" msg)
                               'action (lambda (x) (display-message-hash (button-get x 'msg-hash)))
                               'subj (gethash "subj" msg)
                               'help-echo (concat "Read message *" (gethash "subj" msg) "*")
                               'msg-hash msg)

                ;; Mark by asterisk unread message
                (if (= 1 (gethash "unread" msg))
                        (and
                         (princ "*")
                         (if (> longest subj-length)
                                 (setq subj-length (+ subj-length 1)))))

                (princ (make-string (- longest subj-length) ? ))
                (princ (concat " " (gethash "time" msg)))
                (princ (concat "\t" (gethash "author" msg) "\n")))
            (add-text-properties (beginning-of-buffer) (end-of-buffer) 'read-only)))
    (idec))


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

(defun idec-new-message (&optional echo)
    "Make new message to ECHO."
    (interactive)
    (if (not echo) (edit-new-message (read-string "Echo: "))
        (edit-new-message echo)))

;; Online
(defun idec-online-browse ()
    "Load echoes list.txt from node `idec-primary-node'."
    (interactive)
    (idec-fetch-echo-list (concat idec-primary-node "list.txt")))

(defun idec-online-browse-hidden ()
    "Browse hidden echo."
    (interactive)
    (load-echo-messages (read-string "Enter echo name: ") t))


;;; idec-mode.el ends here

(provide 'idec-mode)
