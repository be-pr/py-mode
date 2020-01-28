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

(declare-function py-xref-backend "py-xref")
(declare-function py-indent--beginning-of-block-p "py-indent")
(declare-function py-completion-function "py-complete")
(declare-function py-eldoc-documentation-function "py-eldoc")

(defvar py-mode-syntax-table)
(defvar py-repl-prompt-regexp
  "^[^ \n]*\\(?:\\(?:>>>\\|\\.\\{3\\}\\) \\)+\\|(Pdb) ")
(defvar py-dedicated-process-buffer nil)
(make-variable-buffer-local 'py-dedicated-process-buffer)

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

(defun py-repl--barf-on-pdb (&optional proc-buffer)
  (with-current-buffer (or proc-buffer (current-buffer))
    (and comint-last-prompt
         (save-excursion
           (goto-char (car comint-last-prompt))
           (looking-at "(Pdb) "))
         (user-error "Process under Pdb's control"))))

(defvar py-repl--receiving-p nil)

(defun py-repl--filter-create (buffer)
  (lambda (_proc chunk)
    (princ chunk buffer)
    (when (string-match-p "> \\'" chunk)
      (setq py-repl--receiving-p nil)
      (with-current-buffer buffer
        (goto-char (point-max))
        (let ((rx "^\\(?:\\(?:>>>\\|\\.\\{3\\}\\) \\)+"))
          ;; Delete parts of the result that can only be prompts.
          (re-search-backward ">>> " nil t)
          (skip-chars-backward " \t\n")
          (delete-region (point) (point-max))
          (goto-char (point-min))
          (when (re-search-forward rx nil t 1)
            (delete-region (point) (point-min))))))))

;; The `input' can either be a list of any number of strings or a list of two
;; integers/markers drawing the boundaries of a region in the current buffer.
(defun py-repl-send (proc read &rest input)
  (declare (indent 2))
  (let ((oldfilter (process-filter proc))
        (oldbuf (current-buffer))
        (in (generate-new-buffer " *py in*"))
        (out (generate-new-buffer " *py out*")))
    (unwind-protect
         (let ((standard-output in))
           (with-current-buffer (process-buffer proc)
             (py-repl--barf-on-pdb)
             (set-process-filter proc (py-repl--filter-create out)))
           (if (integer-or-marker-p (car input))
               (with-current-buffer in
                 (insert-buffer-substring oldbuf (car input)
                                          (cadr input)))
             (dolist (str input) (princ str)))
           (setq standard-output out)
           (setq py-repl--receiving-p t)
           (with-current-buffer in
             (process-send-region proc (point-min) (point-max))
             (process-send-string proc "\n"))
           ;; Intentionally block further execution until the subprocess
           ;; returns.
           (while py-repl--receiving-p
             (accept-process-output proc))
           (unless (zerop (buffer-size out))
             (with-current-buffer out
               (if (not read) (buffer-string)
                 (when (= (following-char) ?\()
                   (condition-case nil (read out)
                     (invalid-read-syntax nil)))))))
      (set-process-filter proc oldfilter)
      (and (buffer-name in) (kill-buffer in))
      (and (buffer-name out) (kill-buffer out))
      (setq py-repl--receiving-p nil))))

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
  (let* ((file buffer-file-name)
         (buf (py-repl-process-buffer))
         (proc (get-buffer-process buf)))
    (unless file (user-error "Current buffer is not visiting a file"))
    (unless proc (user-error "No running Python process"))
    (py-repl--barf-on-pdb buf)
    (process-send-string
     proc (format "with open(r'''%s''') as f:
    exec(compile(f.read(), r'''%s''', 'exec'))\n\n" file file))))

(defun py-eval-defun ()
  (interactive)
  (let* ((buf (py-repl-process-buffer))
         (proc (get-buffer-process buf)))
    (unless proc (user-error "No running Python process"))
    (py-repl--barf-on-pdb buf)
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
         (proc (get-buffer-process buf)))
    (unless proc (user-error "No running Python process"))
    (py-repl--barf-on-pdb buf)
    ;; Anything that is a statement, i.e., not a single expression, has to be
    ;; compiled in 'exec' mode, before being passed to `eval' as a bytecode
    ;; object. Otherwise `eval' throws a SyntaxError.
    (save-excursion
      (goto-char end)
      (if (>= beg (line-beginning-position))
          (process-send-region proc beg end)
        (process-send-string proc "eval(compile(r'''")
        (process-send-region proc beg end)
        (process-send-string proc "''', '<stdin>', 'exec'))")))
    (process-send-string proc "\n")
    (deactivate-mark)))

(defun py-eval-line ()
  (interactive)
  (save-excursion
    (beginning-of-line 1)
    (skip-chars-forward " \t")
    (unless (eolp)
      (py-eval-region (point) (line-beginning-position 2)))))

(defun py-repl--backward-token ()
  (let ((orig (point)) forward-sexp-function)
    (skip-chars-backward " \t")
    (condition-case nil
        (cond ((bolp) nil)
              ((zerop (skip-syntax-backward "."))
               (forward-sexp -1) t)
              ((memq (following-char) '(?: ?, ?`)) nil)
              ((/= orig (point)) t))
      (scan-error nil))))

(defun py-repl--last-expression-bounds ()
  (let ((end (point)))
    (save-excursion
      (while (py-repl--backward-token))
      (unless (= (point) end)
        (list (point) end)))))

(defun py-eval-last-expression (&optional arg)
  (interactive "P")
  (let ((proc (py-repl-get-process))
        (bds (py-repl--last-expression-bounds)))
    (when bds
      (funcall (if arg 'insert 'message)
               (apply #'py-repl-send
                      (cons proc (cons nil bds)))))))

(defun py-switch-to-repl (&optional arg)
  (interactive "P")
  (let ((proc (get-buffer-process (py-repl-process-buffer))))
    (cond (proc (pop-to-buffer (process-buffer proc)))
          ((yes-or-no-p "Start inferior Python process?")
           (run-py arg)))))

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
