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

(defcustom idec-download-limit "50"
    "Limit of download messages."
    :type 'string
    :group 'idec)

(defcustom idec-download-offset "-50"
    "Offset of download messages."
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

;; FUNCTIONS
;; ;;;;;;;;;

(defun create-echo-mail-dir (echo)
    "Create ECHO directory inside `idec-mail-dir'."
    (if (file-exists-p idec-mail-dir)
            (message idec-mail-dir)
        (mkdir idec-mail-dir))
    (if (file-exists-p (concat idec-mail-dir (concat "/" echo)))
            (message (concat idec-mail-dir (concat "/" echo)))
        (mkdir (concat idec-mail-dir (concat "/" echo)))))

(defun idec-load-new-messages ()
    "Load new messages from IDEC nodes Not implemented.")

;; ECHOES FUNCTIONS
;; ;;;;;;;;;;;;;;;;

(defun make-echo-url (echoes)
    "Make ECHOES url to retreive messages."
    ;; Check ECHOES is list
    (if (listp echoes)
            ;; Required GNU Emacs >= 25.3
            (message (concat idec-primary-node "u/e/"
                             (string-join echoes "/") "/" idec-download-offset ":" idec-download-limit))
        (message (concat idec-primary-node "u/e/" echoes "/" idec-download-offset ":" idec-download-limit))))

(defun display-echo-messages (messages)
    "Display downloaded MESSAGES from echo."
    (with-output-to-temp-buffer (get-buffer-create (concat "*IDEC: browse echo*"))
        (switch-to-buffer "*IDEC: browse echo*")
        (princ messages)))

(defun load-echo-messages (echo)
    "Load messages from ECHO."
    (with-current-buffer
            (url-retrieve-synchronously (make-echo-url echo))
        (goto-char (point-min))
        (re-search-forward "^$")
        (delete-region (point) (point-min))
        (display-echo-messages (buffer-string))))

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
    (with-current-buffer
            (url-retrieve-synchronously nodeurl)
        (goto-char (point-min))
        (re-search-forward "^$")
        (delete-region (point) (point-min))
        (proccess-echo-list (buffer-string))))

(defun idec-load-echoes ()
    "Load echoes list from node."
    (interactive)
    (idec-fetch-echo-list (concat idec-primary-node "list.txt")))

;; END OF ECHOES FUNCTIONS
;; ;;;;;;;;;;;;;;;;;;;;;;;

;; END OF FUNCTIONS
;; ;;;;;;;;;;;;;;;;


(provide 'idec)

;;; idec.el ends here
