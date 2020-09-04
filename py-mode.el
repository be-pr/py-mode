;;; py-mode.el --- Major mode for Python -*- lexical-binding: t -*-

;; Copyright (c) 2019, 2020 Bernhard Pröll

;; Author: Bernhard Pröll
;; Maintainer: Bernhard Pröll
;; Created: 2019-06-07
;; Version: 0.0.1
;; Keywords: Languages
;; Package-Requires: ((emacs "27.0.50"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'comint)
(require 'py-indent)

(declare-function py-repl-send "py-repl" (proc read &rest input))
(declare-function py-repl--primary-bounds "py-repl" (&optional names-only))

(autoload 'py-completion-function "py-complete")
(autoload 'py-eldoc-documentation-function "py-eldoc")
(autoload 'py-xref-backend "py-xref")
(autoload 'py-xref--nenv "py-xref")
(autoload 'py-switch-to-repl "py-repl" nil t)
(autoload 'run-py "py-repl" nil t)
(autoload 'py-repl-get-process "py-repl")
(autoload 'py-eval-defun "py-repl" nil t)
(autoload 'py-eval-region "py-repl" nil t)
(autoload 'py-eval-last-primary "py-repl" nil t)

(defvar electric-pair-pairs)
(defvar electric-pair-inhibit-predicate)
(defvar electric-pair-skip-self)
(defvar py-repl-output)
(defvar imenu-use-markers)

(defvar py-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol syntax.
    (let ((symbol (string-to-syntax "_"))
          (sst (standard-syntax-table)))
      (dotimes (i 128)
        (when (equal symbol (aref sst i))
          (modify-syntax-entry i "." table))))
    (modify-syntax-entry ?\\ "\\" table) ;escape syntax
    (modify-syntax-entry ?'  "\"" table) ;string delimiter
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?_  "_"  table)
    (modify-syntax-entry ?#  "<"  table) ;open comment
    (modify-syntax-entry ?\n ">"  table) ;close comment
    (modify-syntax-entry ?`  "$"  table) ;deprecated alias for `repr'
    table)
  "Syntax table for Python code.")

(defun py-beginning-of-defun (&optional arg)
  (or arg (setq arg 1))
  (if (> arg 0)
      (forward-comment (- (point)))
    (end-of-line 1)
    (forward-comment (buffer-size)))
  (forward-line 0)
  (let ((n (if (> arg 0) -1 1)))
    (while (and (or (not (zerop (current-indentation)))
                    (looking-at "[ \t]*\\(?:$\\|#\\)")
                    (progn (setq arg (+ arg n))
                           (not (zerop arg))))
                (zerop (forward-line n)))))
  (not (or (bobp) (eobp))))

(defun py-end-of-defun ()
  (while (and (zerop (forward-line 1))
              (or (not (zerop (current-indentation)))
                  (looking-at "[ \t]*\\(?:$\\|#\\)"))))
  (forward-comment (- (point))))

(defconst py--def-rx "^[ \t]*\\(?:\\(?:async[ \t]+\\)?def\\|class\\)[ \t]+")

(defun py-imenu-create-index ()
  (goto-char (point-max))
  (let ((rx (concat py--def-rx "\\([^ (:]+\\)"))
        (pm imenu-use-markers) acc)
    (while (re-search-backward rx nil t 1)
      (when (zerop (py-xref--nenv))
        (push (cons (match-string 1)
                    (if pm (point-marker) (point)))
              acc)))
    (nreverse acc)))

(defun py-electric-pair-inhibit (c)
  (if (and (eq (char-syntax c) ?\")
           (save-excursion (= (skip-chars-backward (string c)) -3)))
      (save-excursion
        (when (/= (following-char) c)
          (insert (make-string 3 c)))
        t)
    (funcall (default-value 'electric-pair-inhibit-predicate) c)))

(defun py-electric-pair-skip (c)
  (or (and (eq (char-syntax c) ?\")
           (save-excursion (< (skip-chars-backward (string c)) -3)))
      (funcall (default-value 'electric-pair-skip-self) c)))

;; Add an additional newline between triple quotes when calling `newline'.
(defun py-electric-pair-open-newline-p ()
  (let (state column)
    ;; Delimited by a generic string delimiter character.
    (when (and (eq last-command-event ?\n)
               (setq state (syntax-ppss))
               (eq (nth 3 state) t)
               (save-excursion
                 (forward-char -1)
                 (and (= -3 (skip-syntax-backward "\"|"))
                      (= (point) (nth 8 state))
                      (setq column (current-column))
                      t)))
      (save-excursion
        (insert "\n")
        (indent-to column))
      (indent-to column)))
  (default-value 'electric-pair-open-newline-between-pairs))

(defvar py-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-c?\C-c] 'py-eval-file)
    (define-key map [?\C-c?\C-z] 'py-switch-to-repl)
    (define-key map [?\C-\M-x] 'py-eval-defun)
    (define-key map [?\C-c?\C-r] 'py-eval-region)
    (define-key map [remap eval-last-sexp] 'py-eval-last-primary)
    (define-key map [?\C-c?\C-d] 'py-describe-symbol)
    (define-key map [backtab] 'py-indent-dedent)
    (define-key map [C-tab] 'py-indent-indent)
    map)
  "Mode map for `py-mode'.")

(defvar py-syntax-propertize-function
  (let ((delimiter (string-to-syntax "|")))
    (syntax-propertize-rules
     ("\"\\{3\\}\\|'\\{3\\}"
      (0 (let* ((start (match-beginning 0))
                (state (save-excursion (syntax-ppss start))))
           (when (nth 3 state)
             (setq start (+ start 2)))
           (cond
             ;; Inside comment.
             ((nth 4 state))
             ;; Starting quote is escaped.
             ((nth 5 state)
              (goto-char (1+ (match-beginning 0))))
             (t (put-text-property start (1+ start) 'syntax-table
                                   delimiter)))))))))

(define-derived-mode py-mode prog-mode
  "Py" "Major mode for editing Python code.

\\{py-mode-map}"
  :group 'py
  :keymap py-mode-map
  (setq-local tab-width 4)
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local comment-start "#")
  (setq-local syntax-propertize-function py-syntax-propertize-function)
  (setq font-lock-defaults '(()))
  (setq-local indent-tabs-mode nil)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local beginning-of-defun-function #'py-beginning-of-defun)
  (setq-local end-of-defun-function #'py-end-of-defun)
  (setq-local imenu-create-index-function #'py-imenu-create-index)
  (add-hook 'xref-backend-functions #'py-xref-backend nil t)
  (add-hook 'completion-at-point-functions #'py-completion-function nil t)
  (add-function :override (local 'eldoc-documentation-function)
                #'py-eldoc-documentation-function)
  (setq-local indent-line-function #'py-indent-function)
  (setq-local electric-pair-pairs '((?\' . ?\') (?\" . ?\")))
  (setq-local electric-pair-inhibit-predicate #'py-electric-pair-inhibit)
  (setq-local electric-pair-skip-self #'py-electric-pair-skip)
  (setq-local electric-pair-open-newline-between-pairs
              #'py-electric-pair-open-newline-p)
  (setq-local electric-pair-skip-whitespace t))

(defun py-describe-symbol ()
  (interactive)
  (save-excursion
    (skip-syntax-forward "w_")
    (let* ((proc (py-repl-get-process))
           (bds (py-repl--primary-bounds t))
           (prim (if bds (apply #'buffer-substring-no-properties bds)
                   (read-string "Primary: ")))
           (buf (get-buffer-create "*python doc*"))
           (result (py-repl-send proc nil "help(" prim ")"))
           (rx "^[[:upper:]]\\{2,\\}.*")
           (pop-up-windows t)
           (inhibit-read-only t)
           (case-fold-search nil))
      (when result
        (with-current-buffer buf
          (erase-buffer)
          (py-doc-mode)
          (insert result)
          (goto-char (point-min))
          (save-excursion
            (while (re-search-forward rx nil t 1)
              (with-silent-modifications
	            (put-text-property (match-beginning 0) (point) 'face 'bold)))))
        (pop-to-buffer buf '((display-buffer-reuse-window
                              display-buffer-pop-up-window)))))))

(defvar py-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map [?i] 'imenu)
    (define-key map [?n] 'end-of-defun)
    (define-key map [?p] 'beginning-of-defun)
    map))

(define-derived-mode py-doc-mode special-mode
  "pydoc"
  :group 'py
  :keymap py-doc-mode-map
  (setq-local buffer-read-only t)
  (setq-local beginning-of-defun-function
              #'py-doc-beginning-of-defun)
  (setq-local end-of-defun-function #'py-doc-end-of-defun)
  (setq-local imenu-case-fold-search nil)
  (setq-local imenu-generic-expression
              '((nil "^[[:upper:]]\\{2,\\}.*" 0))))

(defun py-doc-beginning-of-defun (&optional arg)
  (interactive "p")
  (unless arg (setq arg 1))
  (or (and (> arg 0) (bolp))
      (end-of-line 1))
  (let (case-fold-search)
    (and (re-search-forward "^[[:upper:]]\\{2,\\}.*" nil t (- arg))
         (goto-char (match-beginning 0))
         t)))

(defun py-doc-end-of-defun ()
  (let (case-fold-search)
    (re-search-forward "^[[:upper:]]\\{2,\\}.*" nil t 1)
    (forward-line 0)))


(provide 'py-mode)
;;; py-mode.el ends here
