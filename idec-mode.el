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

;; MODE
;; ;;;;

(defun idec-close-message-buffer ()
    "Close buffer with message."
    (kill-this-buffer))

(defvar idec-mode-hook nil)

(defvar idec-mode-map
    (let ((map (make-sparse-keymap)))
        (define-key map ["C-c C-c"] 'kill-this-buffer)
        (define-key map ["C-c C-n"] 'idec-next-message)
        (define-key map ["C-c C-b"] 'idec-previous-message)
        (define-key map ["C-c C-e"] 'idec-new-message)
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

(defvar idec-font-lock-keywords
    '(("function \\(\\sw+\\)" (1 font-lock-function-name-face)))
    "Default highlighting expressions for IDEC mode.")

;; Mode function
(define-derived-mode idec-mode org-mode "IDEC"
    "Major mode for view and editing IDEC messages."
    :syntax-table idec-mode-syntax-table
    (setq-local comment-start "// ")
    (setq-local comment-start-skip "//+\\s-*")
    (setq-local font-lock-defaults
                '(idec-font-lock-keywords))
    (use-local-map idec-mode-map)
    (setq-local indent-line-function 'org-indent-line)
    (setq imenu-generic-expression "*IDEC")
    (setq mode-name "[IDEC]")
    (run-hooks 'idec-mode-hook))

;; (defun idec-mode ()
;;     "Major mode for view and editing IDEC messages."
;;     (interactive)
;;     (kill-all-local-variables)
;;     ;; Mode definition
;;     (set-syntax-table idec-mode-syntax-table)
;;     (use-local-map idec-mode-map)
;;     ;; (font-lock-add-keywords 'idec-mode '(idec-font-lock-keywords))
;;     ;; (set (make-local-variable 'font-lock-defaults) '(idec-font-lock-keywords))
;;     (setq major-mode 'idec-mode)
;;     (setq mode-name "[IDEC]")
;;     (setq imenu-generic-expression "*IDEC")
;;     (run-hooks 'idec-mode-hook))

(provide 'idec-mode)

;;; idec-mode.el ends here
