;; py-repl.el -*- lexical-binding: t -*-

;; Copyright (c) 2019, 2020 Bernhard Pröll

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

(defconst py-mode-path
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
        (forward-line 0)
        (let ((rx "\\(?:\\(?:>>>\\|\\.\\{3\\}\\) \\)+"))
          ;; Delete parts of the result that can only be prompts.
          (if (re-search-forward (concat rx "\\'") nil t)
              (goto-char (match-beginning 0))
            (forward-line 1))
          (skip-chars-backward "\n")
          (delete-region (point) (point-max))
          (goto-char (point-min))
          (when (re-search-forward (concat "\\`" rx) nil t)
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
                (insert-buffer-substring oldbuf (car input) (cadr input)))
            (dolist (str input) (princ str)))
          (setq standard-output out)
          (setq py-repl--receiving-p t)
          (with-current-buffer in
            (process-send-region proc (point-min) (point-max))
            (process-send-string proc "\n"))
          ;; Intentionally block further execution until the subprocess
          ;; returns.
          (while py-repl--receiving-p
            (accept-process-output proc 0.01))
          (unless (zerop (buffer-size out))
            (with-current-buffer out
              (if (not read) (buffer-string)
                (when (= (following-char) ?\()
                  (condition-case nil
                      (read out)
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
    (process-send-string proc (format "with open(r'''%s''') as f:
    exec(compile(f.read(), r'''%s''', 'exec'))\n\n" file file))))

(defun py-repl--backward-decorators ()
  (let ((orig (point)) decorated)
    (while (and (zerop (forward-line -1))
                (or (and (= (following-char) ?@)
                         (setq decorated t))
                    (progn (skip-chars-forward " \t")
                           (eolp)))))
    (if decorated
        (forward-comment (buffer-size))
      (goto-char orig))))

(defun py-eval-defun ()
  (interactive)
  (let* ((buf (py-repl-process-buffer))
         (proc (get-buffer-process buf)))
    (unless proc (user-error "No running Python process"))
    (py-repl--barf-on-pdb buf)
    (save-excursion
      (forward-line 0)
      (comment-forward (buffer-size))
      (end-of-line)
      (funcall beginning-of-defun-function)
      (let ((beg (point)))
        (funcall end-of-defun-function)
        (forward-comment (- (point)))
        (let ((end (point))
              (lbp (line-beginning-position)))
          (goto-char beg)
          (py-repl--backward-decorators)
          (let ((single (= (line-beginning-position) lbp)))
            (process-send-string proc (concat (if single "eval" "exec")
                                              "(compile(r'''"))
            (process-send-region proc (point) end)
            (process-send-string proc (if single
                                          "''', '<stdin>', 'single'))\n"
                                        "\n''', '<stdin>', 'exec'))\n"))))))))

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
        (process-send-string proc "''', '<stdin>', 'exec'))\n")))
    (deactivate-mark)))

(defun py-repl--backward-token (&optional names-only)
  (let* ((c (preceding-char))
         (stx (char-syntax c))
         forward-sexp-function)
    (cond ((bobp) nil)
          ((bolp) (unless names-only
                    (forward-comment (- (point)))
                    ;; Line continuation.
                    (not (zerop (skip-chars-backward "\\\\")))))
          ((= c ?.) (skip-chars-backward ".") t)
          ((memq stx '(?. ?\()) nil)
          ((and names-only (memq stx '(?\) ? ))) nil)
          ;; Do not parse beyond the delimiter of a string.
          ((and (eq stx ?\") (nth 3 (syntax-ppss))) nil)
          (t (forward-sexp -1) t))))

;; Optionally ignore everything but a name at point, i.e., an atom or an
;; attribute reference, so that we can safely pass the substring to xref &
;; eldoc.
(defun py-repl--primary-bounds (&optional names-only)
  (save-excursion
    (unless names-only
      (forward-comment (- (point))))
    (let ((end (point)))
      (while (py-repl--backward-token names-only)
        (unless names-only
          (skip-chars-backward " \t")))
      (when (/= (point) end)
        (forward-comment end)
        (list (point) end)))))

;; primary ::= atom | attributeref | subscription | slicing | call
(defun py-eval-last-primary (&optional arg)
  "Evaluate the primary before point.
A primary represents the most tightly bound operation in Python. Either
print the value into the echo area or, with prefix argument ARG, insert the
value at point."
  (interactive "P")
  (let* ((proc (py-repl-get-process))
         (bds (py-repl--primary-bounds))
         (args (cons proc (cons nil bds)))
         (result (when bds (apply #'py-repl-send args))))
    (when result
      (funcall (if arg 'insert 'message) result))))

(defun py-switch-to-repl (&optional arg)
  (interactive "P")
  (let ((proc (get-buffer-process (py-repl-process-buffer))))
    (cond (proc (pop-to-buffer (process-buffer proc)))
          ((yes-or-no-p (format "Start %sinferior Python process?"
                                (if arg "dedicated " "")))
           (run-py arg)))))

(defvar py-executable (executable-find "python3"))

(defun run-py (&optional arg)
  "Run an inferior Python process.
With an \\[universal-argument], dedicate it to the current buffer."
  (interactive "P")
  (and arg (eq major-mode 'py-mode)
       (not (buffer-live-p py-dedicated-process-buffer))
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
    (make-comint-in-buffer "Py REPL" buf
                           (or py-executable (py-find-executable))
                           nil "-i" "-B")
    (pop-to-buffer buf)
    (py-repl-mode)))

(defun py-find-executable ()
  (interactive)
  (setq py-executable
        (read-file-name-default "Executable: " nil (executable-find "python3")
                                t nil 'file-executable-p)))

(defvar py-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?\C-a] 'comint-bol)
    map)
  "Mode map for `run-py'")

(define-derived-mode py-repl-mode comint-mode
  "Python REPL"
  "Major mode for interacting with a Python interpreter.

\\{py-repl-mode-map}"
  :syntax-table py-mode-syntax-table
  (setq comint-prompt-regexp py-repl-prompt-regexp)
  (setq comint-process-echoes nil)
  (setq-local comint-use-prompt-regexp nil)
  (setq-local comint-prompt-read-only t)
  (setq-local comint-scroll-to-bottom-on-input t)
  (setq-local comint-move-point-for-output t)
  (setq-local font-lock-keywords-only t)
  (setq-local paragraph-start py-repl-prompt-regexp)
  (add-hook 'xref-backend-functions #'py-xref-backend nil t)
  (add-hook 'completion-at-point-functions #'py-completion-function nil t)
  (add-function :override (local 'eldoc-documentation-function)
                #'py-eldoc-documentation-function))


(provide 'py-repl)
;;; py-repl.el ends here
