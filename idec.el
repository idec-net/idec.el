;;; idec.el -- GNU Emacs client for IDEC network

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

;; In active developent full featured IDEC client.
;; Fetched node must be support modern IDEC extensions like /list.txt, /x/c, etc.

;;; Code:

;; CUSTOMIZATION
;; ;;;;;;;;;;;;;

(defgroup idec nil
    "IDEC configuration."
    :group 'network)

(defcustom idec-nodes-list
    '("http://idec.spline-online.tk/"
      "https://ii-net.tk/ii/ii-point.php?q=/")
    "List of IDEC nodes."
    :type 'alist
    :group 'idec)

(defcustom idec-primary-node nil
    "Primary node to send messages."
    :type 'string
    :group 'idec
    )

;; Never used at this time.
(defcustom idec-use-list-txt t
    "Use /list.txt extension."
    :group 'idec)

(defcustom idec-download-limit 50
    "Limit of download messages."
    :group 'idec)

(defcustom idec-download-offset -50
    "Offset of download messages."
    :group 'idec)

(defcustom idec-echo-subscriptions nil
    "List of subribes echoes."
    :type 'list
    :group 'idec)

(defgroup idec-accounts nil
    "IDEC accounts settings."
    :group 'idec
    )

(defcustom idec-account-nick nil
    "Account nickname."
    :type 'string
    :group 'idec-accounts
    )

(defcustom idec-account-node nil
    "Node to send messages."
    :type 'string
    :group 'idec-accounts)

(defcustom idec-account-auth nil
    "Account authstring."
    :type 'string
    :group 'idec-accounts
    )

;; END OF CUSTOMIZATION
;; ;;;;;;;;;;;;;;;;;;;;

;; FUNCTIONS
;; ;;;;;;;;;
(defun idec-load-new-messages ()
    "Load new messages from IDEC nodes."
    )

;; ECHOES FUNCTIONS
;; ;;;;;;;;;;;;;;;;

(defun proccess-echo-list (raw-list)
    "Parse RAW-LIST from HTTP response."
    (with-output-to-temp-buffer "*IDEC: list.txt*"
        (print raw-list)))

(defun idec-fetch-echo-list (nodeurl)
    "Fetch echoes list from remote NODEURL."
    (url-retrieve nodeurl)
    (switch-to-buffer (current-buffer)))

(defun idec-load-echoes ()
    "Load echoes list from node."
    (interactive)
    (dolist (node idec-nodes-list)
        (idec-fetch-echo-list (concat node "list.txt"))))

;; END OF ECHOES FUNCTIONS
;; ;;;;;;;;;;;;;;;;;;;;;;;

;; END OF FUNCTIONS
;; ;;;;;;;;;;;;;;;;


(provide 'idec)

;;; idec.el ends here
