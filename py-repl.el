;; py-repl.el -*- lexical-binding: t -*-

;; Copyright (c) 2019 Bernhard Pröll

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
(eval-when-compile (require 'subr-x)) ;string-blank-p, string-trim-right

(declare-function py-xref-backend "py-xref")
(declare-function py-indent-function "py-mode")
(declare-function py-indent--beginning-of-block-p "py-indent")
(declare-function py-completion-function "py-complete")
(declare-function py-eldoc-documentation-function "py-eldoc")

(defvar py-mode-syntax-table)
(defvar py-repl-prompt-regexp
  "^[^ \n]*\\(?:\\(?:>>>\\|\\.\\{3\\}\\) \\)+\\|(Pdb) ")
(defvar py-repl-output nil)
(defvar-local py-dedicated-process-buffer nil)

(defvar py-mode-path
  (if load-file-name (file-name-directory load-file-name)
    default-directory))

(defun py-repl-process-buffer ()
  (or  (and (buffer-live-p py-dedicated-process-buffer)
            py-dedicated-process-buffer)
       (get-buffer "*python*")
       (let ((proc (get-buffer-process (current-buffer))))
         (and proc (equal (process-name proc) "Py REPL")
              (current-buffer)))))

(defun py-repl-get-process ()
  (or (get-buffer-process (py-repl-process-buffer))
      (user-error "No running Python process")))

;; Check whether the process is under Pdb's control.
(defun py-repl--pdb-p ()
  (when comint-last-prompt
    (save-excursion
      (goto-char (car comint-last-prompt))
      (looking-at "(Pdb) "))))

(defun py-repl--filter (str)
  (let ((buf (get-buffer " *py-repl tempbuf*")))
    (with-current-buffer buf (insert str))
    (when (string-match-p "> \\'" str)
      (remove-hook 'comint-preoutput-filter-functions
                   #'py-repl--filter t)
      (with-current-buffer buf
        (skip-chars-backward " >.")
        ;; Find the actual last prompt.
        (when (re-search-forward ">\\{3\\} " nil t 1)
          (goto-char (match-beginning 0)))
        (let ((end (point)))
          (goto-char (point-min))
          (re-search-forward
           "^\\(?:\\(?:>>>\\|\\.\\{3\\}\\) \\)*" end t 1)
          (setq py-repl-output (buffer-substring (point) end)))
        (kill-buffer)))
    ""))

(defun py-repl--send (proc str)
  (get-buffer-create " *py-repl tempbuf*")
  (with-current-buffer (process-buffer proc)
    (add-hook 'comint-preoutput-filter-functions
              #'py-repl--filter nil t))
  (process-send-string proc str)
  (process-send-string proc "\n")
  (let (inhibit-quit)
    ;; Intentionally block further execution until the subprocess returns.
    (while (get-buffer " *py-repl tempbuf*")
      (accept-process-output proc))))

(defun py-repl-send (proc str)
  (declare (indent 1))
  (setq py-repl-output nil)
  (unless (with-current-buffer (process-buffer proc)
            (py-repl--pdb-p))
    (py-repl--send proc str)
    t))

;; Sending multiline statements to the subprocess and also applying the right
;; __code__.co_firstlineno attributes seems to be too complicated.
;; `python-shell-buffer-substring' & `python-shell-send-file' go a long way and
;; achieve this with a lot of string juggling, tempfiles etc. I prefer to
;; simply (re-)read the current buffer-file when I need the right
;; __code__.co_firstlineno attributes so that python-xref finds definitions.
(defun py-eval-file ()
  (interactive)
  (when (buffer-modified-p)
    (basic-save-buffer))
  (let* ((file (buffer-file-name))
         (buf (py-repl-process-buffer))
         (proc (get-buffer-process buf)))
    (unless file (user-error "Current buffer is not visiting a file"))
    (unless proc (user-error "No running Python process"))
    (with-current-buffer buf
      (when (py-repl--pdb-p)
        (user-error "Process under Pdb's control")))
    (process-send-string
     proc (format "with open(r'''%s''') as f:
    exec(compile(f.read(), r'''%s''', 'exec'))\n\n" file file))))

(defun py-eval-defun ()
  (interactive)
  (let* ((buf (py-repl-process-buffer))
         (proc (get-buffer-process buf)))
    (unless proc (user-error "No running Python process"))
    (with-current-buffer buf
      (when (py-repl--pdb-p)
        (user-error "Process under Pdb's control")))
    (save-excursion
      (comment-forward (buffer-size))
      (end-of-line)
      (funcall beginning-of-defun-function)
      (while (not (bolp))
        (funcall beginning-of-defun-function))
      (let ((beg (point)))
        (unless (py-indent--beginning-of-block-p)
          (error "No statement at point"))
        (funcall end-of-defun-function)
        (process-send-string proc "exec(compile(r'''")
        (process-send-region proc beg (point))
        (process-send-string proc "''', '<stdin>', 'exec'))\n")))))

(defun py-eval-region (&optional beg end)
  (interactive "r")
  (let* ((buf (py-repl-process-buffer))
         (proc (get-buffer-process buf))
         (str (buffer-substring-no-properties beg end)))
    (unless proc (user-error "No running Python process"))
    (with-current-buffer buf
      (when (py-repl--pdb-p)
        (user-error "Process under Pdb's control")))
    (process-send-string
     proc
     ;; Anything that is a statement, i.e., not a single expression, has to be
     ;; compiled in 'exec' mode, before being passed to `eval' as a bytecode
     ;; object. Otherwise `eval' throws a SyntaxError.
     (if (string-match-p "\n" str)
         (format "eval(compile(r'''%s''', '<stdin>', 'exec'))" str)
       (format "%s" str)))
    (process-send-string proc "\n")
    (deactivate-mark)))

(defun py-eval-line ()
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (skip-chars-forward " \t")
    (unless (eolp)
      (py-eval-region (point) (line-beginning-position 2)))))

(defun py-repl--tokenize ()
  (let ((end (point))
        forward-sexp-function)
    (skip-chars-backward " \t")
    (cond
      ((bolp) nil)
      ((zerop (skip-syntax-backward "."))
       (condition-case nil
           (progn
             (forward-sexp -1)
             (buffer-substring-no-properties (point) end))
         (scan-error nil)))
      ((memq (char-after) '(?: ?, ?`)) nil)
      (t (buffer-substring-no-properties (point) end)))))

;; Find a single expression before point.
(defun py-repl--last-expression ()
  (let (token acc)
    (save-excursion
      (setq token (py-repl--tokenize))
      (while token
        (when (stringp token)
          (setq acc (concat token acc)))
        (setq token (py-repl--tokenize))))
    acc))

(defun py-eval-last-expression (&optional arg)
  (interactive "P")
  (let* ((proc (py-repl-get-process))
         (expr (py-repl--last-expression))
         (end (point)))
    (when expr
      (or (py-repl-send proc expr)
          (user-error "Process under Pdb's control"))
      (when (and py-repl-output
                 (not (string-blank-p py-repl-output)))
        (let ((str (string-trim-right py-repl-output "\n")))
          (if arg
              (save-excursion
                (insert "\n" str)
                (comment-region end (point)))
            (message "%s" str)))))))

;;;###autoload
(defun py-switch-to-repl (&optional arg)
  (interactive "P")
  (if-let* ((proc (get-buffer-process (py-repl-process-buffer))))
      (pop-to-buffer (process-buffer proc))
    (when (yes-or-no-p "Start inferior Python process?")
      (run-py arg))))

;;;###autoload
(defun run-py (&optional arg)
  "Run an inferior Python process.
With an \\[universal-argument], specify the path to the executable."
  (interactive "P")
  (and (eq major-mode 'py-mode)
       (not (buffer-live-p py-dedicated-process-buffer))
       (y-or-n-p "Dedicate process to current buffer?")
       (setq py-dedicated-process-buffer
             (generate-new-buffer
              (format "*python[%s]*" (buffer-name)))))
  (let ((buf (or (py-repl-process-buffer)
                 (get-buffer-create "*python*")))
        (path (concat py-mode-path "utils.py"))
        (startup (getenv "PYTHONSTARTUP")))
    (when (and startup (not (equal startup path)))
      (message "An existing PYTHONSTARTUP has been replaced"))
    ;; Avoid __pycache__ and run utils.py as a startup script.
    (setenv "PYTHONSTARTUP" path)
    (make-comint-in-buffer
     "Py REPL" buf
     (if arg (read-file-name-default
              "Executable: " nil nil t nil 'file-executable-p)
       (or (executable-find "python3")
           (error "Python executable not found")))
     nil "-i" "-B")
    (pop-to-buffer buf)
    (py-repl-mode)))

(defvar py-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-a] 'comint-bol)
    map)
  "Mode map for `run-py'")

(define-derived-mode py-repl-mode comint-mode
  "Python REPL"
  "Major mode for interacting with a Python interpreter.

\\<py-repl-mode-map>"
  :syntax-table py-mode-syntax-table
  (setq comint-prompt-regexp py-repl-prompt-regexp)
  (setq comint-process-echoes nil)
  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-scroll-to-bottom-on-input t)
  (setq-local comint-move-point-for-output t)
  (setq-local font-lock-keywords-only t)
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start py-repl-prompt-regexp)
  (add-hook 'xref-backend-functions #'py-xref-backend nil t)
  (add-hook 'completion-at-point-functions
            #'py-completion-function nil t)
  (add-function :override (local 'eldoc-documentation-function)
                #'py-eldoc-documentation-function))


(provide 'py-repl)
;;; py-repl.el ends here
