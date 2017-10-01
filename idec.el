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

(defcustom idec-smart-fetch nil
    "Enable smat fetching;
Download only new messages; Traffic saving; Not implemented."
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
    :type 'list
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

;; END OF VARIABLES
;; ;;;;;;;;;;;;;;;;

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
    (file-exists-p (get-message-file echo msg)))

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
    "Load new messages from IDEC `idec-primary-node';
Not implemented."
    (interactive)
    (defvar current-echo nil)
    (defvar new-messages '())
    (dolist (line (split-string (download-subscriptions) "\n"))
        (if (string-match "\\." line)
                (and (setq current-echo line)
                     (store-echo-counter line))
            (download-message current-echo line))))

(defun get-message-content (echo msg)
    "Get ECHO MSG content from `idec-primary-node'."
    (decode-coding-string
     (base64-decode-string
      (nth 1 (split-string (get-url-content (make-messages-url msg)) ":")))
     'utf-8))

(defun download-message (echo msg)
    "Download ECHO message MSG to `idec-mail-dir'."
    (message (concat "Download message " msg " to " echo))
    (if (not (string-match "^$" msg))
            (if (not
                 (check-message-in-echo msg echo))
                    (store-message (get-message-content echo msg) echo msg))))

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
                             (string-join echoes "/") "/" idec-download-offset ":" idec-download-limit))
        (message (concat idec-primary-node "u/e/" echoes "/" idec-download-offset ":" idec-download-limit))))

(defun make-messages-url (messages)
    "Make MESSAGES url to retreive messages from `idec-primary-node'."
    ;; Check ECHOES is list
    (if (listp messages)
            ;; Required GNU Emacs >= 25.3
            (message (concat idec-primary-node "u/m/"
                    (string-join messages "/")))
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
                (setq current-echo (nth 0 (split-string line ":")))
                ;; Create clickable button
                (insert-text-button current-echo
                                    'action (lambda (x) (load-echo-messages (button-get x 'echo)))
                                    'help-echo (concat "Go to echo " current-echo)
                                    'echo current-echo)
                (princ (format "\t\t||%s\t\t%s\n"
                               (nth 2 (split-string line ":"))
                               (nth 1 (split-string line ":")))))
            )))

(defun idec-fetch-echo-list (nodeurl)
    "Fetch echoes list from remote NODEURL."
        (proccess-echo-list (get-url-content nodeurl)))

(defun idec-load-echoes ()
    "Load echoes list.txt from node `idec-primary-node'."
    (interactive)
    (idec-fetch-echo-list (concat idec-primary-node "list.txt")))

;; END OF ECHOES FUNCTIONS
;; ;;;;;;;;;;;;;;;;;;;;;;;

;; END OF FUNCTIONS
;; ;;;;;;;;;;;;;;;;


(provide 'idec)

;;; idec.el ends here
