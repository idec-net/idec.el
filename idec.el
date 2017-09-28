;;; idec.el -- GNU Emacs clien for IDEC network

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

(defvar idec-nodes-list
    '("http://idec.spline-online.tk/"
      "https://ii-net.tk/ii/ii-point.php?q=/")
    "List of IDEC nodes.")

;; Never used at this time.
(defvar idec-use-list-txt t
    "Use /list.txt extension.")

(defvar idec-download-limit 50
    "Limit of download messages.")

(defvar idec-download-offset -50
    "Offset of download messages.")

(defvar idec-subscriptions '()
    "List of subribes echoes.")

;; FUNCTIONS
;; ;;;;;;;;;
(defun idec-load-new-messages ()
    "Load new messages from IDEC node."
    )

;; ECHOES FUNCTIONS
;; ;;;;;;;;;;;;;;;;
(defun fetch-echo-list (url)
    "Fetch echoes list from remote URL."
    (message url))

(defun idec-load-echoes ()
    "Load echoes list from node."
    (dolist (node idec-nodes-list)
        (setq download-url (concat node "list.txt"))
        (fetch-echo-list download-url)
        ))

;; END OF ECHOES FUNCTIONS
;; ;;;;;;;;;;;;;;;;;;;;;;;

;; END OF FUNCTIONS
;; ;;;;;;;;;;;;;;;;


(provide 'idec-load-echoes)
(provide 'idec)

;;; idec.el ends here
