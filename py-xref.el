;;; py-xref.el --- Find definitions in Python -*- lexical-binding: t -*-


;; Copyright (c) 2018, 2019 Bernhard Pröll

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

(require 'py-repl)
(require 'py-complete)
(require 'xref)
(require 'pcase)
(eval-when-compile (require 'subr-x)) ;string-empty-p, string-trim-right

(declare-function py--object-at-point "py-mode")

;;;###autoload
(defun py-xref-backend () 'python)

(cl-defmethod xref-backend-identifier-at-point
    ((_backend (eql python)))
  (py--object-at-point))

(cl-defmethod xref-backend-definitions ((_backend (eql python)) id)
  (let ((proc (get-buffer-process (py-repl-process-buffer))))
    (if (not proc)
        (py-xref--find-definition id)
      (if (py-repl-send proc
            (format "print(_get_location(%S))" id))
          (when py-repl-output
            (let ((out (string-trim-right py-repl-output "\n")))
              (unless (string-empty-p out)
                (pcase (read out)
                  (`(quote ,_)
                    (or (py-xref--find-definition id)
                        (user-error out)))
                  (`(,file . ,line)
                    (if (equal file "<stdin>")
                        (user-error "%s defined at %s" id file)
                      (list (py-xref--make-xref id file line))))
                  ('None (user-error "Failed to locate %s" id))
                  ;; If all fails, regexp search for the identifier in the
                  ;; current buffer.
                  (_ (or (py-xref--find-definition id)
                         (user-error out)))))))
        (py-xref--find-definition id)))))

(defun py-xref--re-search-identifier (id &optional end)
  (let ((rx (concat "^[ \t]*\\(?:\\(?:async[ \t]+\\)?def\\|class\\)"
                    "[ \t]+" id "\\_>")))
    (and (buffer-file-name)
         (re-search-forward rx end t 1)
         (list (py-xref--make-xref
                id (buffer-file-name) (line-number-at-pos))))))

(defun py-xref--find-definition (id)
  (save-excursion
    ;; Possibly strip "self" or "cls" from local definitions.
    (if (string-match "[^.]+\\.\\([^.]+\\)" id)
        (let ((id (match-string 1 id)))
          (funcall beginning-of-defun-function)
          ;; Goto toplevel definition.
          (while (not (bolp))
            (funcall beginning-of-defun-function))
          (py-xref--re-search-identifier
           id (save-excursion
                (funcall end-of-defun-function)
                (point))))
      ;; Otherwise explore the entire buffer.
      (goto-char (point-min))
      (py-xref--re-search-identifier id))))

(defun py-xref--make-xref (id file line)
  (xref-make id (xref-make-file-location file line 0)))

(defalias 'py-xref-table (py-complete--table-create t))

(cl-defmethod xref-backend-identifier-completion-table
    ((_backend (eql python)))
  #'py-xref-table)


(provide 'py-xref)
;;; py-xref ends here
