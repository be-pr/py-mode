;; ob-py.el -*- lexical-binding: t -*-

;; Copyright (c) 2019 Bernhard Pröll

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'ob)
(require 'py-repl)
(require 'cl-lib)
(require 'pcase)
(eval-when-compile (require 'subr-x)) ;when-let*

(defun org-babel-execute:py (body params)
  "Execute a block of Python code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let ((proc (get-buffer-process (py-repl-process-buffer)))
        table-form)
    (if (not proc)
        (ob-py--call-process body)
      ;; Anything that is a statement, i.e., not a single expression, has to be
      ;; compiled in 'exec' mode before being passed to `eval' as a bytecode
      ;; object. Otherwise `eval' throws a SyntaxError.
      (or (py-repl-send proc
            (if (string-match-p "\n" body)
                (format "eval(compile(r'''%s''', '<stdin>', 'exec'))"
                        body)
              (format "%s" body)))
          (user-error "Process under Pdb's control"))
      (when-let* ((result py-repl-output))
        (if (eq (cdr (assq :result-type params)) 'output)
            result
          (ob-py--lispify proc py-repl-output)
          (let ((obj (read py-repl-output)))
            (when (listp obj)
              (setq table-form obj))
            (cond
              ((consp (car table-form))
               (ob-py--apply-colnames
                table-form (cdr (assq :colnames params))))
              (table-form)
              (t result))))))))

(defun ob-py--lispify (proc expr)
  (py-repl-send proc
    (format "_lispify(_unfold(eval(r'''%s''')))" expr)))

(defun ob-py--apply-colnames (result colnames)
  (pcase colnames
    ("yes"
     (cons (car result)
           (cons 'hline (cdr result))))
    ("no" (cdr result))
    ((pred identity)
     (cons colnames (cons 'hline result)))
    (_ result)))

(defun ob-py--call-process (str)
  (let ((tempfile (make-temp-file "ob-py-")))
    (unwind-protect
         (with-temp-buffer
           (write-region str nil tempfile)
           (call-process "python3" nil t nil "-B" tempfile)
           (buffer-string))
      (when (file-exists-p tempfile)
        (delete-file tempfile)))))


(provide 'ob-py)
;;; ob-py.el ends here
