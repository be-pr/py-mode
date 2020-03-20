;;; py-mode.el --- Major mode for Python -*- lexical-binding: t -*-

;; Copyright (c) 2019 Bernhard Pröll

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

(declare-function py-xref-backend "py-xref")
(declare-function py-xref--nenv "py-xref")
(declare-function py-eldoc-documentation-function "py-eldoc")
(declare-function py-repl-get-process "py-repl")
(declare-function py-repl-send "py-repl" (proc read &rest input))

(autoload 'py-completion-function "py-complete")
(autoload 'py-eldoc-documentation-function "py-eldoc")
(autoload 'py-xref-backend "py-xref")
(autoload 'py-switch-to-repl "py-repl" nil t)
(autoload 'run-py "py-repl" nil t)

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

;; Note that this `beginning-of-defun-function' strictly moves according to the
;; indentation level it finds at the current position.
(defun py-beginning-of-defun (&optional arg)
  (or arg (setq arg 1))
  ;; With an argument greater than 1, always start the second call to
  ;; `beginning-of-defun-raw' in `end-of-defun' from the previous block
  ;; statement.
  (when (and (< arg 0) (eq this-command 'end-of-defun))
    (forward-comment (- (point))))
  (let ((level (current-indentation))
        (n (if (< arg 0) 1 -1)))
    (if (and (> arg 0)
             (> (current-column) level)
             (py-indent--beginning-of-block-p))
        (move-to-column level)
      (if (> arg 0)
          (forward-comment (- (point)))
        (end-of-line 1)
        (forward-comment (buffer-size)))
      (forward-line 0)
      (while (and (or (> (current-indentation) level)
                      (looking-at "[ \t]*\\(?:$\\|#\\)")
                      (and (py-indent--beginning-of-block-p)
                           (progn
                             (setq level (current-indentation))
                             (setq arg (+ arg n))
                             (/= 0 arg))))
                  (zerop (forward-line n))))))
  ;; Backslash continuations.
  (py-indent--beginning-of-continuation)
  ;; Multiline block start.
  (let ((state (syntax-ppss)))
    (unless (zerop (car state))
      (goto-char (car (last (nth 9 state))))
      (forward-line 0)))
  ;; Return a nil value so that we skip the `beginning-of-line' in
  ;; `beginning-of-defun'.
  (progn (back-to-indentation) nil))

(defun py-end-of-defun ()
  (let ((level (current-indentation)))
    (while (and (zerop (forward-line 1))
                (or (> (current-indentation) level)
                    (looking-at "[ \t]*\\(?:$\\|#\\)")
                    (nth 8 (syntax-ppss)))))
    (forward-comment (- (point)))))

(defconst py--def-rx
  "^[ \t]*\\(?:\\(?:async[ \t]+\\)?def\\|class\\)[ \t]+")

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
           (save-excursion
             (= (skip-chars-backward (string c)) -3)))
      (save-excursion
        (when (/= (following-char) c)
          (insert (make-string 3 c)))
        t)
    (funcall (default-value 'electric-pair-inhibit-predicate) c)))

(defun py-electric-pair-skip (c)
  (or (and (eq (char-syntax c) ?\")
           (save-excursion
             (< (skip-chars-backward (string c)) -3)))
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
    (define-key map [?\C-c?\C-l] 'py-eval-line)
    (define-key map [remap eval-last-sexp] 'py-eval-last-expression)
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
             (t (put-text-property
                 start (1+ start)
                 'syntax-table delimiter)))))))))

(define-derived-mode py-mode prog-mode
  "Py" "Major mode for editing Python code.

\\{py-mode-map}"
  :group 'py
  :keymap py-mode-map
  (setq-local tab-width 4)
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local comment-start "#")
  (setq-local syntax-propertize-function
              py-syntax-propertize-function)
  (setq font-lock-defaults '(()))
  (setq-local indent-tabs-mode nil)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local beginning-of-defun-function #'py-beginning-of-defun)
  (setq-local end-of-defun-function #'py-end-of-defun)
  (setq-local imenu-create-index-function #'py-imenu-create-index)
  (add-hook 'xref-backend-functions #'py-xref-backend nil t)
  (add-hook 'completion-at-point-functions
            #'py-completion-function nil t)
  (add-function :override (local 'eldoc-documentation-function)
                #'py-eldoc-documentation-function)
  (setq-local indent-line-function #'py-indent-function)
  (setq-local electric-pair-pairs '((?\' . ?\') (?\" . ?\")))
  (setq-local electric-pair-inhibit-predicate
              #'py-electric-pair-inhibit)
  (setq-local electric-pair-skip-self #'py-electric-pair-skip)
  (setq-local electric-pair-open-newline-between-pairs
              #'py-electric-pair-open-newline-p)
  (setq-local electric-pair-skip-whitespace t))

(defun py--object-at-point (&optional end)
  (save-excursion
    (let ((limit (line-beginning-position))
          forward-sexp-function beg)
      (or end (setq end (save-excursion
                          (skip-syntax-forward "w_")
                          (point))))
      (skip-syntax-backward "w_")
      ;; Since we pass identifiers found to `eval', ignore any function calls.
      (when (/= (preceding-char) ?\))
        (setq beg (point))
        ;; Jump over attributerefs.
        (while (= (preceding-char) ?.)
          (forward-sexp -1)
          ;; Multiline string inputs will cause a SyntaxError in `eval'.
          (unless (or (< (point) limit)
                      (= (following-char) ?\())
            (setq beg (point))))
        (unless (= beg end)
          (buffer-substring-no-properties beg end))))))

(defun py-describe-symbol ()
  (interactive)
  (let ((obj (py--object-at-point))
        (pop-up-windows t)
        (proc (py-repl-get-process))
        (buf (get-buffer-create "*python doc*"))
        (inhibit-read-only t)
        (case-fold-search nil))
    (when (string= obj "")
      (setq obj (read-string "Symbol: ")))
    (let ((result (py-repl-send proc nil "help(" obj ")"))
          (rx "^[[:upper:]]\\{2,\\}.*"))
      (when result
        (with-current-buffer buf
          (erase-buffer)
          (py-doc-mode)
          (insert result)
          (goto-char (point-min))
          (save-excursion
            (while (re-search-forward rx nil t 1)
              (with-silent-modifications
	        (put-text-property
                 (match-beginning 0) (point) 'face 'bold)))))))
    (pop-to-buffer buf '((display-buffer-reuse-window
                          display-buffer-pop-up-window)))))

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
    (and (re-search-forward
          "^[[:upper:]]\\{2,\\}.*" nil t (- arg))
         (goto-char (match-beginning 0))
         t)))

(defun py-doc-end-of-defun ()
  (let (case-fold-search)
    (re-search-forward "^[[:upper:]]\\{2,\\}.*" nil t 1)
    (forward-line 0)))


(provide 'py-mode)
;;; py-mode.el ends here
