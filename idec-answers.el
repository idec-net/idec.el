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

(provide 'idec-answers)

;;; idec-answers.el ends here
